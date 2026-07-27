use fp_core::ast::{BlockStmt, Expr, ExprKind, Item, ItemKind, Node, NodeKind};
use std::collections::HashMap;

pub(crate) struct AstPreProcessor {
    quote_values: HashMap<String, Expr>,
}

impl AstPreProcessor {
    pub(crate) fn new() -> Self {
        Self {
            quote_values: HashMap::new(),
        }
    }

    pub(crate) fn process(&mut self, node: &mut Node) {
        let NodeKind::File(file) = node.kind_mut() else { return };
        self.collect_quotes(&file.items);
        self.resolve_items(&mut file.items);
    }

    fn collect_quotes(&mut self, items: &[Item]) {
        for item in items {
            match item.kind() {
                ItemKind::DefConst(def) if matches!(def.value.kind(), ExprKind::Quote(_)) => {
                    self.quote_values.insert(
                        def.name.as_str().to_string(),
                        (*def.value).clone(),
                    );
                }
                ItemKind::Module(m) => self.collect_quotes(&m.items),
                _ => {}
            }
        }
    }

    fn resolve_items(&mut self, items: &mut [Item]) {
        for item in items {
            match item.kind_mut() {
                ItemKind::DefFunction(func) => self.resolve_in_expr(&mut func.body),
                ItemKind::Module(m) => self.resolve_items(&mut m.items),
                ItemKind::Impl(imp) => self.resolve_items(&mut imp.items),
                ItemKind::DefTrait(t) => self.resolve_items(&mut t.items),
                _ => {}
            }
        }
    }

    fn resolve_in_expr(&mut self, expr: &mut Expr) {
        match expr.kind_mut() {
            ExprKind::Block(block) => {
                let mut new_stmts: Vec<BlockStmt> = Vec::new();
                for stmt in block.stmts.drain(..) {
                    match stmt {
                        BlockStmt::Expr(mut e)
                            if matches!(e.expr.kind(), ExprKind::Splice(_)) =>
                        {
                            let items = self.try_resolve(&e.expr);
                            if items.is_empty() {
                                new_stmts.push(BlockStmt::Expr(e));
                            } else {
                                for item in items {
                                    new_stmts.push(BlockStmt::Item(Box::new(item)));
                                }
                            }
                        }
                        BlockStmt::Expr(mut e) => {
                            self.resolve_in_expr(&mut e.expr);
                            new_stmts.push(BlockStmt::Expr(e));
                        }
                        BlockStmt::Let(mut s) => {
                            if let Some(init) = s.init.as_mut() {
                                self.resolve_in_expr(init);
                            }
                            new_stmts.push(BlockStmt::Let(s));
                        }
                        other => new_stmts.push(other),
                    }
                }
                block.stmts = new_stmts;
            }
            ExprKind::If(e) => {
                self.resolve_in_expr(e.then.as_mut());
                if let Some(elze) = e.elze.as_mut() {
                    self.resolve_in_expr(elze);
                }
            }
            ExprKind::Loop(e) => self.resolve_in_expr(e.body.as_mut()),
            ExprKind::For(e) => self.resolve_in_expr(e.body.as_mut()),
            ExprKind::While(e) => self.resolve_in_expr(e.body.as_mut()),
            ExprKind::Match(e) => {
                for case in &mut e.cases {
                    self.resolve_in_expr(case.body.as_mut());
                }
            }
            _ => {}
        }
    }

    fn try_resolve(&self, expr: &Expr) -> Vec<Item> {
        let ExprKind::Splice(splice) = expr.kind() else { return Vec::new() };
        if let ExprKind::Name(name) = splice.token.kind() {
            if let Some(quote_expr) = self.quote_values.get(&name.to_string()) {
                if let ExprKind::Quote(quote) = quote_expr.kind() {
                    let mut items = Vec::new();
                    for stmt in &quote.block.stmts {
                        if let BlockStmt::Item(item) = stmt {
                            items.push(item.as_ref().clone());
                        }
                    }
                    return items;
                }
            }
        }
        Vec::new()
    }
}
