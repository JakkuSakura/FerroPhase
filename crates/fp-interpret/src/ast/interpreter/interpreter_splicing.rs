use super::*;
use fp_core::ast::{QuoteFragmentKind, QuoteTokenValue, ValueQuoteToken};

impl<'ctx> AstInterpreter<'ctx> {
    pub(super) fn quote_token_from_fragment_kind(
        &mut self,
        fragment: QuotedFragment,
        kind: Option<QuoteFragmentKind>,
    ) -> Value {
        let (default_kind, value) = match fragment {
            QuotedFragment::Expr(expr) => (QuoteFragmentKind::Expr, QuoteTokenValue::Expr(expr)),
            QuotedFragment::Stmts(stmts) => (QuoteFragmentKind::Stmt, QuoteTokenValue::Stmts(stmts)),
            QuotedFragment::Items(items) => (QuoteFragmentKind::Item, QuoteTokenValue::Items(items)),
            QuotedFragment::Type(ty) => (QuoteFragmentKind::Type, QuoteTokenValue::Type(ty)),
        };
        let Some(kind) = kind else {
            return Value::QuoteToken(ValueQuoteToken {
                kind: default_kind,
                value,
            });
        };

        let allowed = match (&value, kind) {
            (QuoteTokenValue::Expr(_), QuoteFragmentKind::Expr) => true,
            (QuoteTokenValue::Stmts(_), QuoteFragmentKind::Stmt) => true,
            (QuoteTokenValue::Items(_), QuoteFragmentKind::Item) => true,
            (QuoteTokenValue::Type(_), QuoteFragmentKind::Type) => true,
            _ => false,
        };
        let kind = if allowed { kind } else { default_kind };
        if !allowed {
            self.emit_error("quote kind does not match fragment contents");
        }
        Value::QuoteToken(ValueQuoteToken { kind, value })
    }

    pub(crate) fn build_quote_token_from_body(
        &mut self,
        kind: QuoteFragmentKind,
        body: &Expr,
    ) -> Value {
        match kind {
            QuoteFragmentKind::Item => {
                let mut items = Vec::new();
                if !self.collect_items_from_expr(body, &mut items) {
                    return Value::undefined();
                }
                if !self.items_match_kind(&items, kind) {
                    return Value::undefined();
                }
                self.quote_token_from_fragment_kind(QuotedFragment::Items(items), Some(kind))
            }
            QuoteFragmentKind::Expr | QuoteFragmentKind::Stmt | QuoteFragmentKind::Type => {
                let block = body.clone().into_block();
                let fragment = match kind {
                    QuoteFragmentKind::Expr => {
                        QuotedFragment::Expr(block.into_expr())
                    }
                    QuoteFragmentKind::Stmt => QuotedFragment::Stmts(block.stmts),
                    QuoteFragmentKind::Type => {
                        let mut block_expr = Expr::block(block);
                        let value = self.eval_expr(&mut block_expr);
                        match value {
                            Value::Type(ty) => QuotedFragment::Type(ty),
                            _ => {
                                self.emit_error("quote<type> requires a type expression");
                                QuotedFragment::Stmts(Vec::new())
                            }
                        }
                    }
                    QuoteFragmentKind::Item => unreachable!(),
                };
                self.quote_token_from_fragment_kind(fragment, Some(kind))
            }
        }
    }

    pub(crate) fn build_quoted_fragment(
        &mut self,
        quote: &fp_core::ast::ExprQuote,
    ) -> QuotedFragment {
        if let Some(kind) = quote.kind {
            return match kind {
                QuoteFragmentKind::Expr => {
                    let mut block = quote.block.clone();
                    self.materialize_quote_block(&mut block);
                    if block.last_expr().is_none() {
                        // Treat statement-only quote<expr> blocks as unit expressions.
                        block.push_expr(Expr::value(Value::unit()));
                    }
                    QuotedFragment::Expr(block.into_expr())
                }
                QuoteFragmentKind::Stmt => {
                    let mut block = quote.block.clone();
                    self.materialize_quote_block(&mut block);
                    QuotedFragment::Stmts(block.stmts)
                }
                QuoteFragmentKind::Item
                => match self.collect_items_from_block(&quote.block) {
                    Some(items) => {
                        if self.items_match_kind(&items, kind) {
                            QuotedFragment::Items(items)
                        } else {
                            QuotedFragment::Items(Vec::new())
                        }
                    }
                    None => QuotedFragment::Items(Vec::new()),
                },
                QuoteFragmentKind::Type => {
                    let mut value_expr = match quote.block.last_expr() {
                        Some(expr) => expr.clone(),
                        None => {
                            self.emit_error("quote<type> requires a trailing type expression");
                            return QuotedFragment::Stmts(quote.block.stmts.clone());
                        }
                    };
                    let value = match quote.block.stmts.len() {
                        1 => self.eval_expr(&mut value_expr),
                        _ => {
                            let mut block_expr = Expr::block(quote.block.clone());
                            self.eval_expr(&mut block_expr)
                        }
                    };
                    match value {
                        Value::Type(ty) => QuotedFragment::Type(ty),
                        _ => {
                            self.emit_error("quote<type> requires a type expression");
                            QuotedFragment::Stmts(quote.block.stmts.clone())
                        }
                    }
                }
            };
        }

        // If kind is explicitly provided, we still infer from block shape to build fragment
        let mut block = quote.block.clone();
        self.materialize_quote_block(&mut block);
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

    fn materialize_quote_block(&mut self, block: &mut ExprBlock) {
        let mut new_stmts = Vec::with_capacity(block.stmts.len());
        for stmt in &mut block.stmts {
            match stmt {
                BlockStmt::Expr(expr_stmt) => {
                    if let ExprKind::Splice(splice) = expr_stmt.expr.kind_mut() {
                        if !self.in_const_region()
                            && !matches!(self.mode, InterpreterMode::CompileTime)
                        {
                            self.emit_error("splice is only supported during const evaluation");
                            continue;
                        }
                        let Some(fragments) =
                            self.resolve_splice_fragments(splice.token.as_mut())
                        else {
                            continue;
                        };
                        for fragment in fragments {
                            match fragment {
                                QuotedFragment::Expr(expr) => {
                                    let mut stmt = expr_stmt.clone();
                                    stmt.expr = expr.into();
                                    new_stmts.push(BlockStmt::Expr(stmt));
                                }
                                QuotedFragment::Stmts(stmts) => {
                                    for stmt in stmts {
                                        new_stmts.push(stmt);
                                    }
                                }
                                QuotedFragment::Items(_) => {
                                    self.emit_error(
                                        "splice<item> is not valid inside quote<expr>/quote<stmt>",
                                    );
                                }
                                QuotedFragment::Type(_) => {
                                    self.emit_error(
                                        "splice<type> is not valid inside quote<expr>/quote<stmt>",
                                    );
                                }
                            }
                        }
                        continue;
                    }
                    self.materialize_quote_expr(expr_stmt.expr.as_mut());
                    new_stmts.push(stmt.clone());
                }
                BlockStmt::Let(stmt_let) => {
                    if let Some(init) = stmt_let.init.as_mut() {
                        self.materialize_quote_expr(init);
                    }
                    if let Some(diverge) = stmt_let.diverge.as_mut() {
                        self.materialize_quote_expr(diverge);
                    }
                    new_stmts.push(stmt.clone());
                }
                BlockStmt::Item(_) | BlockStmt::Noop | BlockStmt::Any(_) => {
                    new_stmts.push(stmt.clone());
                }
            }
        }
        block.stmts = new_stmts;
        if let Some(last) = block.last_expr_mut() {
            self.materialize_quote_expr(last);
        }
    }

    fn materialize_quote_expr(&mut self, expr: &mut Expr) {
        if let Some(value) = self.const_fold_expr_value(expr) {
            if let Some(literal) = self.literal_value_from_value(&value) {
                self.replace_expr_with_value(expr, literal);
                return;
            }
        }

        match expr.kind_mut() {
            ExprKind::Value(value) => {
                if let Value::Expr(inner) = value.as_mut() {
                    self.materialize_quote_expr(inner.as_mut());
                }
            }
            ExprKind::Block(block) => self.materialize_quote_block(block),
            ExprKind::If(if_expr) => {
                self.materialize_quote_expr(if_expr.cond.as_mut());
                self.materialize_quote_expr(if_expr.then.as_mut());
                if let Some(elze) = if_expr.elze.as_mut() {
                    self.materialize_quote_expr(elze);
                }
            }
            ExprKind::While(while_expr) => {
                self.materialize_quote_expr(while_expr.cond.as_mut());
                self.materialize_quote_expr(while_expr.body.as_mut());
            }
            ExprKind::For(for_expr) => {
                self.materialize_quote_expr(for_expr.iter.as_mut());
                self.materialize_quote_expr(for_expr.body.as_mut());
            }
            ExprKind::Loop(loop_expr) => self.materialize_quote_expr(loop_expr.body.as_mut()),
            ExprKind::Return(expr_return) => {
                if let Some(value) = expr_return.value.as_mut() {
                    self.materialize_quote_expr(value);
                }
            }
            ExprKind::Break(expr_break) => {
                if let Some(value) = expr_break.value.as_mut() {
                    self.materialize_quote_expr(value);
                }
            }
            ExprKind::Continue(_) => {}
            ExprKind::ConstBlock(const_block) => {
                self.materialize_quote_expr(const_block.expr.as_mut());
            }
            ExprKind::Match(match_expr) => {
                for case in match_expr.cases.iter_mut() {
                    self.materialize_quote_expr(case.cond.as_mut());
                    self.materialize_quote_expr(case.body.as_mut());
                }
            }
            ExprKind::Let(expr_let) => self.materialize_quote_expr(expr_let.expr.as_mut()),
            ExprKind::Invoke(invoke) => {
                match &mut invoke.target {
                    ExprInvokeTarget::Expr(target) => self.materialize_quote_expr(target.as_mut()),
                    ExprInvokeTarget::Method(select) => {
                        self.materialize_quote_expr(select.obj.as_mut())
                    }
                    ExprInvokeTarget::Closure(closure) => {
                        self.materialize_quote_expr(closure.body.as_mut())
                    }
                    ExprInvokeTarget::Function(_)
                    | ExprInvokeTarget::Type(_)
                    | ExprInvokeTarget::BinOp(_) => {}
                }
                for arg in invoke.args.iter_mut() {
                    self.materialize_quote_expr(arg);
                }
            }
            ExprKind::Assign(assign) => {
                self.materialize_quote_expr(assign.target.as_mut());
                self.materialize_quote_expr(assign.value.as_mut());
            }
            ExprKind::Select(select) => self.materialize_quote_expr(select.obj.as_mut()),
            ExprKind::Index(index) => {
                self.materialize_quote_expr(index.obj.as_mut());
                self.materialize_quote_expr(index.index.as_mut());
            }
            ExprKind::BinOp(binop) => {
                self.materialize_quote_expr(binop.lhs.as_mut());
                self.materialize_quote_expr(binop.rhs.as_mut());
            }
            ExprKind::UnOp(unop) => self.materialize_quote_expr(unop.val.as_mut()),
            ExprKind::Paren(paren) => self.materialize_quote_expr(paren.expr.as_mut()),
            ExprKind::Tuple(tuple) => {
                for value in tuple.values.iter_mut() {
                    self.materialize_quote_expr(value);
                }
            }
            ExprKind::Array(array) => {
                for value in array.values.iter_mut() {
                    self.materialize_quote_expr(value);
                }
            }
            ExprKind::Structural(structural) => {
                for field in structural.fields.iter_mut() {
                    if let Some(value) = field.value.as_mut() {
                        self.materialize_quote_expr(value);
                    }
                }
            }
            ExprKind::Struct(strukt) => {
                for field in strukt.fields.iter_mut() {
                    if let Some(value) = field.value.as_mut() {
                        self.materialize_quote_expr(value);
                    }
                }
            }
            ExprKind::FormatString(template) => {
                for arg in template.args.iter_mut() {
                    self.materialize_quote_expr(arg);
                }
                for kwarg in template.kwargs.iter_mut() {
                    self.materialize_quote_expr(&mut kwarg.value);
                }
            }
            ExprKind::IntrinsicCall(call) => match &mut call.payload {
                IntrinsicCallPayload::Args { args } => {
                    for arg in args.iter_mut() {
                        self.materialize_quote_expr(arg);
                    }
                }
                IntrinsicCallPayload::Format { template } => {
                    for arg in template.args.iter_mut() {
                        self.materialize_quote_expr(arg);
                    }
                    for kwarg in template.kwargs.iter_mut() {
                        self.materialize_quote_expr(&mut kwarg.value);
                    }
                }
            },
            ExprKind::Range(range) => {
                if let Some(start) = range.start.as_mut() {
                    self.materialize_quote_expr(start.as_mut());
                }
                if let Some(end) = range.end.as_mut() {
                    self.materialize_quote_expr(end.as_mut());
                }
                if let Some(step) = range.step.as_mut() {
                    self.materialize_quote_expr(step.as_mut());
                }
            }
            ExprKind::ArrayRepeat(repeat) => {
                self.materialize_quote_expr(repeat.elem.as_mut());
                self.materialize_quote_expr(repeat.len.as_mut());
            }
            ExprKind::Reference(reference) => {
                self.materialize_quote_expr(reference.referee.as_mut());
            }
            ExprKind::Dereference(deref) => {
                self.materialize_quote_expr(deref.referee.as_mut());
            }
            ExprKind::Cast(cast) => {
                self.materialize_quote_expr(cast.expr.as_mut());
            }
            ExprKind::IntrinsicContainer(container) => {
                container.for_each_expr_mut(|expr| self.materialize_quote_expr(expr));
            }
            ExprKind::Closured(closured) => {
                self.materialize_quote_expr(closured.expr.as_mut());
            }
            ExprKind::Closure(closure) => {
                self.materialize_quote_expr(closure.body.as_mut());
            }
            ExprKind::Await(await_expr) => {
                self.materialize_quote_expr(await_expr.base.as_mut());
            }
            ExprKind::Async(async_expr) => {
                self.materialize_quote_expr(async_expr.expr.as_mut());
            }
            ExprKind::Try(expr_try) => {
                self.materialize_quote_expr(expr_try.expr.as_mut());
            }
            ExprKind::Splat(splat) => {
                self.materialize_quote_expr(splat.iter.as_mut());
            }
            ExprKind::SplatDict(splat) => {
                self.materialize_quote_expr(splat.dict.as_mut());
            }
            ExprKind::Splice(splice) => {
                if !self.in_const_region() && !matches!(self.mode, InterpreterMode::CompileTime) {
                    self.emit_error("splice is only supported during const evaluation");
                    return;
                }
                let Some(fragments) = self.resolve_splice_fragments(splice.token.as_mut())
                else {
                    return;
                };
                if fragments.len() != 1 {
                    self.emit_error("splice in expression position expects a single fragment");
                    return;
                }
                match fragments.into_iter().next().unwrap() {
                    QuotedFragment::Expr(replacement) => {
                        *expr = replacement;
                    }
                    QuotedFragment::Stmts(_) => {
                        self.emit_error("splice<stmt> is not valid in expression position");
                    }
                    QuotedFragment::Items(_) => {
                        self.emit_error("splice<item> is not valid in expression position");
                    }
                    QuotedFragment::Type(_) => {
                        self.emit_error("splice<type> is not valid in expression position");
                    }
                }
            }
            ExprKind::Item(_) => {}
            ExprKind::Quote(_)
            | ExprKind::Locator(_)
            | ExprKind::Macro(_)
            | ExprKind::Any(_)
            | ExprKind::Id(_) => {}
        }
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

    fn items_match_kind(&mut self, _items: &[Item], _kind: QuoteFragmentKind) -> bool {
        true
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
            ExprKind::Splice(splice) => {
                if !self.in_const_region() {
                    self.emit_error("splice is only supported in const regions");
                    return false;
                }
                let mut token = splice.token.as_ref().clone();
                let Some(fragments) = self.resolve_splice_fragments(&mut token) else {
                    return false;
                };
                for fragment in fragments {
                    match fragment {
                        QuotedFragment::Items(mut nested) => items.append(&mut nested),
                        _ => {
                            self.emit_error(
                                "quote<item> only accepts item fragments when splicing",
                            );
                            return false;
                        }
                    }
                }
                true
            }
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
            Value::QuoteToken(token) => match token.value {
                QuoteTokenValue::Expr(expr) => Some(QuotedFragment::Expr(expr)),
                QuoteTokenValue::Stmts(stmts) => Some(QuotedFragment::Stmts(stmts)),
                QuoteTokenValue::Items(items) => Some(QuotedFragment::Items(items)),
                QuoteTokenValue::Type(ty) => Some(QuotedFragment::Type(ty)),
            },
            Value::Expr(expr) => self.fragment_from_expr(&expr),
            _ => None,
        }
    }
}
