use fp_core::ast::{
    BlockStmt, BlockStmtExpr, Expr, ExprBlock, ExprFor, ExprIf, ExprInvoke, ExprInvokeTarget,
    ExprKind, ExprLet, ExprMatch, ExprWhile, File, FunctionSignature, Ident, Item, ItemDefFunction,
    ItemKind, Name, Node, NodeKind, Value,
};
use std::collections::HashMap;
use std::path::PathBuf;

pub fn apply(node: &Node, _limit: &[String]) -> Result<Node, String> {
    let mut ctx = Ctx {
        path: PathBuf::new(),
        items: Vec::new(),
        sigs: collect_signatures(node),
    };
    ctx.lower(node)?;
    Ok(Node::file(File {
        path: ctx.path,
        attrs: Vec::new(),
        items: ctx.items,
    }))
}

struct Ctx {
    path: PathBuf,
    items: Vec<Item>,
    sigs: HashMap<String, FunctionSignature>,
}

impl Ctx {
    fn lower(&mut self, node: &Node) -> Result<(), String> {
        let NodeKind::File(file) = node.kind() else { return Ok(()) };
        self.path = file.path.clone();
        for item in &file.items {
            match item.kind() {
                ItemKind::DefFunction(f) if f.name.as_str() == "main" => {
                    self.flatten_main(f)?;
                }
                ItemKind::DefFunction(f) => {
                    self.items.push(item.clone());
                }
                ItemKind::Module(module) => {
                    let mut sub = Vec::new();
                    self.lower_module_items(&module.items, &mut sub, &[])?;
                    self.items.push(Item::from(ItemKind::Module(fp_core::ast::Module {
                        attrs: Vec::new(),
                        name: module.name.clone(),
                        items: sub,
                        visibility: module.visibility.clone(),
                        is_external: module.is_external,
                    })));
                }
                _ => {
                    self.items.push(item.clone());
                }
            }
        }
        Ok(())
    }

    fn lower_module_items(
        &self,
        items: &[Item],
        out: &mut Vec<Item>,
        path: &[String],
    ) -> Result<(), String> {
        for item in items {
            match item.kind() {
                ItemKind::Module(module) => {
                    let mut sub = Vec::new();
                    let mut child = path.to_vec();
                    child.push(module.name.as_str().to_string());
                    self.lower_module_items(&module.items, &mut sub, &child)?;
                    out.push(Item::from(ItemKind::Module(fp_core::ast::Module {
                        attrs: Vec::new(),
                        name: module.name.clone(),
                        items: sub,
                        visibility: module.visibility.clone(),
                        is_external: module.is_external,
                    })));
                }
                _ => out.push(item.clone()),
            }
        }
        Ok(())
    }

    fn flatten_main(&mut self, f: &ItemDefFunction) -> Result<(), String> {
        let body = self.expand_with_blocks((*f.body).clone());
        if let ExprKind::Block(block) = body.kind() {
            for stmt in &block.stmts {
                if let BlockStmt::Expr(e) = stmt {
                    let expr = self.inject_context_args(e.expr.as_ref().clone());
                    self.items.push(Item::from(ItemKind::Expr(expr)));
                }
            }
        }
        Ok(())
    }

    fn expand_with_blocks(&self, expr: Expr) -> Expr {
        let (ty, kind) = expr.into_parts();
        match kind {
            ExprKind::With(w) => Expr::from_parts(ty, ExprKind::Block(ExprBlock {
                span: Default::default(),
                stmts: vec![BlockStmt::Expr(BlockStmtExpr::new(
                    self.expand_with_blocks((*w.body).clone())
                ).with_semicolon(true))],
            })),
            ExprKind::Block(block) => {
                let stmts = block.stmts.into_iter().map(|s| match s {
                    BlockStmt::Expr(e) => BlockStmt::Expr(BlockStmtExpr::new(
                        self.expand_with_blocks(e.expr.as_ref().clone())
                    ).with_semicolon(true)),
                    other => other,
                }).collect();
                Expr::from_parts(ty, ExprKind::Block(ExprBlock { span: block.span, stmts }))
            }
            _ => Expr::from_parts(ty, kind),
        }
    }

    fn inject_context_args(&self, expr: Expr) -> Expr {
        let (ty, kind) = expr.into_parts();
        match kind {
            ExprKind::Invoke(invoke) => {
                let sig = self.sig_for_invoke(&invoke);
                let mut args = invoke.args;
                if let Some(sig) = sig {
                    while args.len() < sig.params.len() {
                        let idx = args.len();
                        if let Some(param) = sig.params.get(idx) {
                            if param.is_context {
                                args.push(Expr::value(Value::string("localhost".into())));
                            } else if let Some(default) = &param.default {
                                args.push(Expr::value(default.clone()));
                            } else {
                                let val = match &param.ty {
                                    fp_core::ast::Ty::Primitive(fp_core::ast::TypePrimitive::Bool) => Value::bool(false),
                                    fp_core::ast::Ty::Primitive(fp_core::ast::TypePrimitive::Int(_)) => Value::int(0),
                                    _ => Value::string(String::new()),
                                };
                                args.push(Expr::value(val));
                            }
                        }
                    }
                }
                Expr::from_parts(ty, ExprKind::Invoke(ExprInvoke {
                    span: invoke.span,
                    target: invoke.target,
                    args,
                    kwargs: invoke.kwargs,
                }))
            }
            ExprKind::Block(block) => {
                let stmts = block.stmts.into_iter().map(|s| match s {
                    BlockStmt::Expr(e) => BlockStmt::Expr(BlockStmtExpr::new(
                        self.inject_context_args(e.expr.as_ref().clone())
                    ).with_semicolon(true)),
                    other => other,
                }).collect();
                Expr::from_parts(ty, ExprKind::Block(ExprBlock { span: block.span, stmts }))
            }
            _ => Expr::from_parts(ty, kind),
        }
    }

    fn sig_for_invoke(&self, invoke: &ExprInvoke) -> Option<&FunctionSignature> {
        let name = match &invoke.target {
            ExprInvokeTarget::Function(name) => {
                name.to_path().segments.iter().map(|s| s.as_str()).collect::<Vec<_>>().join("::")
            }
            _ => return None,
        };
        self.sigs.get(&name)
    }
}

fn collect_signatures(node: &Node) -> HashMap<String, FunctionSignature> {
    let mut sigs = HashMap::new();
    if let NodeKind::File(file) = node.kind() {
        scan_signatures(&file.items, &[], &mut sigs);
    }
    sigs
}

fn scan_signatures(
    items: &[Item],
    path: &[String],
    out: &mut HashMap<String, FunctionSignature>,
) {
    for item in items {
        match item.kind() {
            ItemKind::DefFunction(f) => {
                let name = qualify(path, f.name.as_str());
                out.insert(name, f.sig.clone());
            }
            ItemKind::DeclFunction(f) => {
                let name = qualify(path, f.name.as_str());
                out.insert(name, f.sig.clone());
            }
            ItemKind::Module(module) => {
                let mut child = path.to_vec();
                child.push(module.name.as_str().to_string());
                scan_signatures(&module.items, &child, out);
            }
            _ => {}
        }
    }
}

fn qualify(module: &[String], name: &str) -> String {
    if module.is_empty() {
        name.to_string()
    } else {
        format!("{}::{}", module.join("::"), name)
    }
}
