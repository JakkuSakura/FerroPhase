use fp_core::ast::{
    BlockStmt, Expr, ExprBlock, ExprInvokeTarget, ExprKind, Item, ItemKind, Node, NodeKind, Value,
};
use fp_core::error::Result;
use fp_core::intrinsics::runtime::RuntimeIntrinsicStrategy;
use fp_core::intrinsics::IntrinsicCallPayload;

pub struct NoopIntrinsicStrategy;
impl RuntimeIntrinsicStrategy for NoopIntrinsicStrategy {}

pub fn materialize_runtime_intrinsics(
    ast: &mut Node,
    strategy: &dyn RuntimeIntrinsicStrategy,
) -> Result<()> {
    match ast.kind_mut() {
        NodeKind::File(file) => {
            strategy.prepare_file(file);
            for item in &mut file.items {
                materialize_item(item, strategy)?;
            }
            Ok(())
        }
        NodeKind::Item(item) => materialize_item(item, strategy),
        NodeKind::Expr(expr) => materialize_expr(expr, strategy),
    }
}

fn materialize_item(item: &mut Item, strategy: &dyn RuntimeIntrinsicStrategy) -> Result<()> {
    match item.kind_mut() {
        ItemKind::Module(module) => {
            for child in &mut module.items {
                materialize_item(child, strategy)?;
            }
        }
        ItemKind::Impl(impl_block) => {
            for child in &mut impl_block.items {
                materialize_item(child, strategy)?;
            }
        }
        ItemKind::DefFunction(func) => {
            materialize_expr(func.body.as_mut(), strategy)?;
        }
        ItemKind::DefConst(def) => {
            materialize_expr(def.value.as_mut(), strategy)?;
        }
        ItemKind::DefStatic(def) => {
            materialize_expr(def.value.as_mut(), strategy)?;
        }
        ItemKind::DefStruct(_)
        | ItemKind::DefStructural(_)
        | ItemKind::DefEnum(_)
        | ItemKind::DefType(_)
        | ItemKind::DeclConst(_)
        | ItemKind::DeclStatic(_)
        | ItemKind::DeclFunction(_)
        | ItemKind::DeclType(_)
        | ItemKind::Import(_)
        | ItemKind::DefTrait(_)
        | ItemKind::Any(_) => {}
        ItemKind::Expr(expr) => {
            materialize_expr(expr, strategy)?;
        }
    }

    Ok(())
}

fn materialize_block(block: &mut ExprBlock, strategy: &dyn RuntimeIntrinsicStrategy) -> Result<()> {
    for stmt in &mut block.stmts {
        match stmt {
            BlockStmt::Expr(expr_stmt) => {
                materialize_expr(expr_stmt.expr.as_mut(), strategy)?;
            }
            BlockStmt::Let(stmt_let) => {
                if let Some(init) = stmt_let.init.as_mut() {
                    materialize_expr(init, strategy)?;
                }
                if let Some(diverge) = stmt_let.diverge.as_mut() {
                    materialize_expr(diverge, strategy)?;
                }
            }
            BlockStmt::Item(item) => {
                materialize_item(item.as_mut(), strategy)?;
            }
            BlockStmt::Noop | BlockStmt::Any(_) => {}
        }
    }

    Ok(())
}

fn materialize_expr(expr: &mut Expr, strategy: &dyn RuntimeIntrinsicStrategy) -> Result<()> {
    let expr_ty = expr.ty.clone();
    let mut replacement: Option<Expr> = None;

    match expr.kind_mut() {
        ExprKind::Block(block) => materialize_block(block, strategy)?,
        ExprKind::If(expr_if) => {
            materialize_expr(expr_if.cond.as_mut(), strategy)?;
            materialize_expr(expr_if.then.as_mut(), strategy)?;
            if let Some(else_expr) = expr_if.elze.as_mut() {
                materialize_expr(else_expr, strategy)?;
            }
        }
        ExprKind::Loop(expr_loop) => {
            materialize_expr(expr_loop.body.as_mut(), strategy)?;
        }
        ExprKind::While(expr_while) => {
            materialize_expr(expr_while.cond.as_mut(), strategy)?;
            materialize_expr(expr_while.body.as_mut(), strategy)?;
        }
        ExprKind::Match(match_expr) => {
            for case in &mut match_expr.cases {
                materialize_expr(case.cond.as_mut(), strategy)?;
                materialize_expr(case.body.as_mut(), strategy)?;
            }
        }
        ExprKind::Let(expr_let) => {
            materialize_expr(expr_let.expr.as_mut(), strategy)?;
        }
        ExprKind::Assign(expr_assign) => {
            materialize_expr(expr_assign.target.as_mut(), strategy)?;
            materialize_expr(expr_assign.value.as_mut(), strategy)?;
        }
        ExprKind::Invoke(invoke) => {
            materialize_invoke_target(&mut invoke.target, strategy)?;
            for arg in &mut invoke.args {
                materialize_expr(arg, strategy)?;
            }
        }
        ExprKind::Select(select) => {
            materialize_expr(select.obj.as_mut(), strategy)?;
        }
        ExprKind::Struct(struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    materialize_expr(value, strategy)?;
                }
            }
        }
        ExprKind::Structural(struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    materialize_expr(value, strategy)?;
                }
            }
        }
        ExprKind::Array(array_expr) => {
            for value in &mut array_expr.values {
                materialize_expr(value, strategy)?;
            }
        }
        ExprKind::ArrayRepeat(array_repeat) => {
            materialize_expr(array_repeat.elem.as_mut(), strategy)?;
            materialize_expr(array_repeat.len.as_mut(), strategy)?;
        }
        ExprKind::Tuple(tuple_expr) => {
            for value in &mut tuple_expr.values {
                materialize_expr(value, strategy)?;
            }
        }
        ExprKind::BinOp(binop) => {
            materialize_expr(binop.lhs.as_mut(), strategy)?;
            materialize_expr(binop.rhs.as_mut(), strategy)?;
        }
        ExprKind::UnOp(unop) => {
            materialize_expr(unop.val.as_mut(), strategy)?;
        }
        ExprKind::Reference(reference) => {
            materialize_expr(reference.referee.as_mut(), strategy)?;
        }
        ExprKind::Dereference(expr_deref) => {
            materialize_expr(expr_deref.referee.as_mut(), strategy)?;
        }
        ExprKind::Index(expr_index) => {
            materialize_expr(expr_index.obj.as_mut(), strategy)?;
            materialize_expr(expr_index.index.as_mut(), strategy)?;
        }
        ExprKind::Splat(expr_splat) => {
            materialize_expr(expr_splat.iter.as_mut(), strategy)?;
        }
        ExprKind::SplatDict(expr_splat) => {
            materialize_expr(expr_splat.dict.as_mut(), strategy)?;
        }
        ExprKind::Try(expr_try) => {
            materialize_expr(expr_try.expr.as_mut(), strategy)?;
        }
        ExprKind::Closure(closure) => {
            materialize_expr(closure.body.as_mut(), strategy)?;
        }
        ExprKind::Closured(closured) => {
            materialize_expr(closured.expr.as_mut(), strategy)?;
        }
        ExprKind::Paren(paren) => {
            materialize_expr(paren.expr.as_mut(), strategy)?;
        }
        ExprKind::FormatString(format) => {
            for arg in &mut format.args {
                materialize_expr(arg, strategy)?;
            }
            for kwarg in &mut format.kwargs {
                materialize_expr(&mut kwarg.value, strategy)?;
            }
        }
        ExprKind::Item(item) => {
            materialize_item(item.as_mut(), strategy)?;
        }
        ExprKind::Value(value) => {
            if let Value::Expr(inner) = value.as_mut() {
                materialize_expr(inner.as_mut(), strategy)?;
            }
        }
        ExprKind::IntrinsicCall(call) => {
            match &mut call.payload {
                IntrinsicCallPayload::Format { template } => {
                    for arg in &mut template.args {
                        materialize_expr(arg, strategy)?;
                    }
                    for kwarg in template.kwargs.iter_mut() {
                        materialize_expr(&mut kwarg.value, strategy)?;
                    }
                }
                IntrinsicCallPayload::Args { args } => {
                    for arg in args {
                        materialize_expr(arg, strategy)?;
                    }
                }
            }

            if let Some(expr) = strategy.rewrite_intrinsic(call, &expr_ty)? {
                replacement = Some(expr);
            }
        }
        _ => {}
    }

    if let Some(new_expr) = replacement {
        *expr = new_expr;
    }

    Ok(())
}

fn materialize_invoke_target(
    target: &mut ExprInvokeTarget,
    strategy: &dyn RuntimeIntrinsicStrategy,
) -> Result<()> {
    match target {
        ExprInvokeTarget::Method(select) => materialize_expr(select.obj.as_mut(), strategy)?,
        ExprInvokeTarget::Expr(expr) => materialize_expr(expr.as_mut(), strategy)?,
        ExprInvokeTarget::Closure(closure) => materialize_expr(closure.body.as_mut(), strategy)?,
        ExprInvokeTarget::Function(_) | ExprInvokeTarget::Type(_) | ExprInvokeTarget::BinOp(_) => {}
    }
    Ok(())
}
