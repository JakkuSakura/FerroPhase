use super::*;
use fp_core::ast::QuoteFragmentKind;

impl<'ctx> AstInterpreter<'ctx> {
    pub(crate) fn build_quoted_fragment(
        &mut self,
        quote: &fp_core::ast::ExprQuote,
    ) -> QuotedFragment {
        if let Some(kind) = quote.kind {
            return match kind {
                QuoteFragmentKind::Expr => {
                    if let Some(expr) = quote.block.last_expr() {
                        QuotedFragment::Expr(expr.clone())
                    } else {
                        self.emit_error("quote<expr> requires a trailing expression");
                        QuotedFragment::Stmts(quote.block.stmts.clone())
                    }
                }
                QuoteFragmentKind::Stmt => QuotedFragment::Stmts(quote.block.stmts.clone()),
                QuoteFragmentKind::Item => match self.collect_items_from_block(&quote.block) {
                    Some(items) => QuotedFragment::Items(items),
                    None => QuotedFragment::Items(Vec::new()),
                },
                QuoteFragmentKind::Type => {
                    self.emit_error("quote<type> is not supported for splicing yet");
                    QuotedFragment::Stmts(quote.block.stmts.clone())
                }
            };
        }

        // If kind is explicitly provided, we still infer from block shape to build fragment
        let block = quote.block.clone();
        // If only a single expression without preceding statements ⇒ Expr fragment
        let is_only_expr = block.first_stmts().is_empty() && block.last_expr().is_some();
        if is_only_expr {
            if let Some(expr) = block.last_expr() {
                return QuotedFragment::Expr(expr.clone());
            }
            // Defensive fallback: should be unreachable given is_only_expr check
            return QuotedFragment::Stmts(block.stmts.clone());
        }
        // If contains only items ⇒ Items fragment
        let only_items = block.stmts.iter().all(|s| matches!(s, BlockStmt::Item(_)));
        if only_items {
            let items: Vec<Item> = block
                .stmts
                .iter()
                .filter_map(|s| match s {
                    BlockStmt::Item(i) => Some((**i).clone()),
                    _ => None,
                })
                .collect();
            return QuotedFragment::Items(items);
        }
        // Fallback ⇒ Stmts fragment (keep entire statement list)
        QuotedFragment::Stmts(block.stmts.clone())
    }

    fn collect_items_from_block(&mut self, block: &ExprBlock) -> Option<Vec<Item>> {
        let mut items = Vec::new();
        for stmt in &block.stmts {
            match stmt {
                BlockStmt::Item(item) => items.push((**item).clone()),
                BlockStmt::Expr(expr_stmt) => {
                    if !self.collect_items_from_expr(expr_stmt.expr.as_ref(), &mut items) {
                        return None;
                    }
                }
                BlockStmt::Let(_) => {
                    self.emit_error("quote<item> does not support let statements");
                    return None;
                }
                BlockStmt::Noop => {}
                BlockStmt::Any(_) => {
                    self.emit_error("quote<item> contains unsupported statements");
                    return None;
                }
            }
        }
        Some(items)
    }

    fn collect_items_from_expr(&mut self, expr: &Expr, items: &mut Vec<Item>) -> bool {
        match expr.kind() {
            ExprKind::Block(block) => match self.collect_items_from_block(block) {
                Some(mut nested) => {
                    items.append(&mut nested);
                    true
                }
                None => false,
            },
            ExprKind::If(if_expr) => {
                let mut cond = if_expr.cond.as_ref().clone();
                match self.eval_expr(&mut cond) {
                    Value::Bool(flag) => {
                        if flag.value {
                            self.collect_items_from_expr(if_expr.then.as_ref(), items)
                        } else if let Some(elze) = if_expr.elze.as_ref() {
                            self.collect_items_from_expr(elze.as_ref(), items)
                        } else {
                            true
                        }
                    }
                    _ => {
                        self.emit_error("quote<item> if condition must be boolean");
                        false
                    }
                }
            }
            _ => {
                self.emit_error("quote<item> expects item blocks or item conditionals");
                false
            }
        }
    }

    pub(crate) fn resolve_splice_fragments(
        &mut self,
        token: &mut Expr,
    ) -> Option<Vec<QuotedFragment>> {
        if let Some(fragment) = self.fragment_from_expr(token) {
            return Some(vec![fragment]);
        }

        let value = self.eval_expr(token);
        match value {
            Value::List(list) => {
                let mut fragments = Vec::with_capacity(list.values.len());
                for entry in list.values {
                    if let Some(fragment) = self.fragment_from_value(entry) {
                        fragments.push(fragment);
                    } else {
                        self.emit_error("splice expects a list of quote tokens");
                        return None;
                    }
                }
                Some(fragments)
            }
            other => {
                if let Some(fragment) = self.fragment_from_value(other) {
                    Some(vec![fragment])
                } else {
                    self.emit_error("splice expects a quote token expression");
                    None
                }
            }
        }
    }

    fn fragment_from_expr(&mut self, expr: &Expr) -> Option<QuotedFragment> {
        match expr.kind() {
            ExprKind::Quote(quote) => Some(self.build_quoted_fragment(quote)),
            _ => None,
        }
    }

    fn fragment_from_value(&mut self, value: Value) -> Option<QuotedFragment> {
        match value {
            Value::Expr(expr) => self.fragment_from_expr(&expr),
            _ => None,
        }
    }
}
