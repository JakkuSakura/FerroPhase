use super::*;

impl<'ctx> AstInterpreter<'ctx> {
    pub(crate) fn build_quoted_fragment(
        &mut self,
        quote: &fp_core::ast::ExprQuote,
    ) -> QuotedFragment {
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
}
