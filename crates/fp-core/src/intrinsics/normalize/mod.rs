use crate::ast::{
    BlockStmt, Expr, ExprBlock, ExprFormatString, ExprIntrinsicCall, ExprIntrinsicContainer,
    ExprInvoke, ExprInvokeTarget, ExprKind, Item, ItemKind, Node, NodeKind, Ty, Value,
};
use crate::error::Result;
use crate::intrinsics::{IntrinsicNormalizer, NoopIntrinsicNormalizer, NormalizeOutcome};

mod bootstrap;
mod format;

/// Normalize intrinsic expressions into a canonical AST form so that typing and
/// downstream passes can assume consistent structures.
pub fn normalize_intrinsics(node: &mut Node) -> Result<()> {
    normalize_intrinsics_with(node, &NoopIntrinsicNormalizer)
}

pub fn normalize_intrinsics_with(
    node: &mut Node,
    strategy: &dyn IntrinsicNormalizer,
) -> Result<()> {
    normalize_node(node, strategy)
}

fn normalize_node(node: &mut Node, strategy: &dyn IntrinsicNormalizer) -> Result<()> {
    match node.kind_mut() {
        NodeKind::File(file) => {
            for item in &mut file.items {
                normalize_item(item, strategy)?;
            }
        }
        NodeKind::Item(item) => normalize_item(item, strategy)?,
        NodeKind::Expr(expr) => normalize_expr(expr, strategy)?,
        NodeKind::Schema(_) | NodeKind::Query(_) | NodeKind::Workspace(_) => {}
    }
    Ok(())
}

fn normalize_item(item: &mut Item, strategy: &dyn IntrinsicNormalizer) -> Result<()> {
    match item.kind_mut() {
        ItemKind::Macro(_) => {}
        ItemKind::Module(module) => {
            for child in &mut module.items {
                normalize_item(child, strategy)?;
            }
        }
        ItemKind::Impl(impl_block) => {
            for child in &mut impl_block.items {
                normalize_item(child, strategy)?;
            }
        }
        ItemKind::DefFunction(function) => {
            if bootstrap::maybe_rewrite_cli_main(function) {
                return Ok(());
            }
            for param in &mut function.sig.params {
                if let Some(ty) = param.ty_annotation.as_mut() {
                    normalize_ty(ty, strategy)?;
                }
                normalize_ty(&mut param.ty, strategy)?;
            }
            if let Some(ret_ty) = function.sig.ret_ty.as_mut() {
                normalize_ty(ret_ty, strategy)?;
            }
            normalize_expr(function.body.as_mut(), strategy)?
        }
        ItemKind::DefConst(def) => {
            if let Some(ty) = def.ty_annotation.as_mut() {
                normalize_ty(ty, strategy)?;
            }
            if let Some(ty) = def.ty.as_mut() {
                normalize_ty(ty, strategy)?;
            }
            normalize_expr(def.value.as_mut(), strategy)?;
        }
        ItemKind::DefStatic(def) => {
            if let Some(ty) = def.ty_annotation.as_mut() {
                normalize_ty(ty, strategy)?;
            }
            normalize_ty(&mut def.ty, strategy)?;
            normalize_expr(def.value.as_mut(), strategy)?;
        }
        ItemKind::DefStruct(_)
        | ItemKind::DefStructural(_)
        | ItemKind::DefEnum(_)
        | ItemKind::DeclConst(_)
        | ItemKind::DeclStatic(_)
        | ItemKind::DeclFunction(_)
        | ItemKind::DeclType(_)
        | ItemKind::Import(_)
        | ItemKind::Any(_) => {}
        ItemKind::DefTrait(def_trait) => {
            for child in &mut def_trait.items {
                normalize_item(child, strategy)?;
            }
        }
        ItemKind::DefType(def) => normalize_ty(&mut def.value, strategy)?,
        ItemKind::Expr(expr) => normalize_expr(expr, strategy)?,
    }
    Ok(())
}

fn normalize_block(block: &mut ExprBlock, strategy: &dyn IntrinsicNormalizer) -> Result<()> {
    for stmt in &mut block.stmts {
        match stmt {
            BlockStmt::Expr(expr_stmt) => normalize_expr(expr_stmt.expr.as_mut(), strategy)?,
            BlockStmt::Let(stmt_let) => {
                if let Some(init) = stmt_let.init.as_mut() {
                    normalize_expr(init, strategy)?;
                }
                if let Some(diverge) = stmt_let.diverge.as_mut() {
                    normalize_expr(diverge, strategy)?;
                }
            }
            BlockStmt::Item(item) => normalize_item(item.as_mut(), strategy)?,
            BlockStmt::Noop | BlockStmt::Any(_) => {}
        }
    }
    Ok(())
}

fn normalize_expr(expr: &mut Expr, strategy: &dyn IntrinsicNormalizer) -> Result<()> {
    loop {
        let mut replacement: Option<Expr> = None;

        let strat_outcome = match expr.kind() {
            ExprKind::Macro(_) => {
                Some(strategy.normalize_macro(std::mem::replace(expr, Expr::unit()))?)
            }
            ExprKind::IntrinsicCall(_) => {
                Some(strategy.normalize_call(std::mem::replace(expr, Expr::unit()))?)
            }
            ExprKind::IntrinsicContainer(_) => {
                Some(strategy.normalize_container(std::mem::replace(expr, Expr::unit()))?)
            }
            ExprKind::Struct(_) => {
                Some(strategy.normalize_struct(std::mem::replace(expr, Expr::unit()))?)
            }
            ExprKind::Structural(_) => {
                Some(strategy.normalize_structural(std::mem::replace(expr, Expr::unit()))?)
            }
            _ => None,
        };

        if let Some(outcome) = strat_outcome {
            match outcome {
                NormalizeOutcome::Ignored(expr_back) => {
                    *expr = expr_back;
                }
                NormalizeOutcome::Normalized(expr_new) => {
                    *expr = expr_new;
                    continue;
                }
            }
        }

        match expr.kind_mut() {
            ExprKind::Block(block) => normalize_block(block, strategy)?,
            ExprKind::If(expr_if) => {
                normalize_expr(expr_if.cond.as_mut(), strategy)?;
                normalize_expr(expr_if.then.as_mut(), strategy)?;
                if let Some(else_expr) = expr_if.elze.as_mut() {
                    normalize_expr(else_expr, strategy)?;
                }
            }
            ExprKind::Loop(expr_loop) => normalize_expr(expr_loop.body.as_mut(), strategy)?,
            ExprKind::For(expr_for) => {
                normalize_expr(expr_for.iter.as_mut(), strategy)?;
                normalize_expr(expr_for.body.as_mut(), strategy)?;
            }
            ExprKind::While(expr_while) => {
                normalize_expr(expr_while.cond.as_mut(), strategy)?;
                normalize_expr(expr_while.body.as_mut(), strategy)?;
            }
            ExprKind::Match(expr_match) => {
                for case in &mut expr_match.cases {
                    normalize_expr(case.cond.as_mut(), strategy)?;
                    normalize_expr(case.body.as_mut(), strategy)?;
                }
            }
            ExprKind::Let(expr_let) => normalize_expr(expr_let.expr.as_mut(), strategy)?,
            ExprKind::Macro(_) => {}
            ExprKind::Assign(assign) => {
                normalize_expr(assign.target.as_mut(), strategy)?;
                normalize_expr(assign.value.as_mut(), strategy)?;
            }
            ExprKind::Cast(cast) => {
                normalize_expr(cast.expr.as_mut(), strategy)?;
            }
            ExprKind::Invoke(invoke) => {
                normalize_invoke(invoke, strategy)?;

                if let Some(intrinsic_call) = crate::ast::intrinsic_call_from_invoke(invoke) {
                    replacement = Some(Expr::new(ExprKind::IntrinsicCall(intrinsic_call)));
                } else if let Some(repl) = bootstrap::maybe_bootstrap_invoke_replacement(invoke) {
                    replacement = Some(repl);
                } else if let Some(mut collection) = ExprIntrinsicContainer::from_invoke(invoke) {
                    let new_expr = apply_intrinsic_collection(&mut collection, strategy)?;
                    replacement = Some(new_expr);
                } else if bootstrap::is_bootstrap_cli_side_effect_call(invoke) {
                    replacement = Some(Expr::new(ExprKind::Value(Box::new(Value::unit()))));
                }
            }
            ExprKind::Await(await_expr) => normalize_expr(await_expr.base.as_mut(), strategy)?,
            ExprKind::Select(select) => normalize_expr(select.obj.as_mut(), strategy)?,
            ExprKind::Struct(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        normalize_expr(value, strategy)?;
                    }
                }
            }
            ExprKind::Structural(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        normalize_expr(value, strategy)?;
                    }
                }
            }
            ExprKind::Array(array_expr) => {
                for value in &mut array_expr.values {
                    normalize_expr(value, strategy)?;
                }
            }
            ExprKind::ArrayRepeat(repeat) => {
                normalize_expr(repeat.elem.as_mut(), strategy)?;
                normalize_expr(repeat.len.as_mut(), strategy)?;
            }
            ExprKind::Tuple(tuple_expr) => {
                for value in &mut tuple_expr.values {
                    normalize_expr(value, strategy)?;
                }
            }
            ExprKind::BinOp(binop) => {
                normalize_expr(binop.lhs.as_mut(), strategy)?;
                normalize_expr(binop.rhs.as_mut(), strategy)?;
            }
            ExprKind::UnOp(unop) => normalize_expr(unop.val.as_mut(), strategy)?,
            ExprKind::Reference(reference) => normalize_expr(reference.referee.as_mut(), strategy)?,
            ExprKind::Dereference(deref) => normalize_expr(deref.referee.as_mut(), strategy)?,
            ExprKind::Index(index) => {
                normalize_expr(index.obj.as_mut(), strategy)?;
                normalize_expr(index.index.as_mut(), strategy)?;
            }
            ExprKind::Splat(splat) => normalize_expr(splat.iter.as_mut(), strategy)?,
            ExprKind::SplatDict(splat) => normalize_expr(splat.dict.as_mut(), strategy)?,
            ExprKind::Try(expr_try) => normalize_expr(expr_try.expr.as_mut(), strategy)?,
            ExprKind::Async(async_expr) => normalize_expr(async_expr.expr.as_mut(), strategy)?,
            ExprKind::Closure(closure) => normalize_expr(closure.body.as_mut(), strategy)?,
            ExprKind::Closured(closured) => normalize_expr(closured.expr.as_mut(), strategy)?,
            ExprKind::Paren(paren) => normalize_expr(paren.expr.as_mut(), strategy)?,
            ExprKind::FormatString(format_expr) => normalize_format_string(format_expr, strategy)?,
            ExprKind::Item(item) => normalize_item(item.as_mut(), strategy)?,
            ExprKind::Value(value) => normalize_value(value, strategy)?,
            ExprKind::IntrinsicCall(call) => {
                normalize_intrinsic_call(call, strategy)?;
            }
            ExprKind::IntrinsicContainer(collection) => {
                match collection {
                    ExprIntrinsicContainer::VecElements { elements } => {
                        for element in elements {
                            normalize_expr(element, strategy)?;
                        }
                    }
                    ExprIntrinsicContainer::VecRepeat { elem, len } => {
                        normalize_expr(elem.as_mut(), strategy)?;
                        normalize_expr(len.as_mut(), strategy)?;
                    }
                    ExprIntrinsicContainer::HashMapEntries { entries } => {
                        for entry in entries {
                            normalize_expr(&mut entry.key, strategy)?;
                            normalize_expr(&mut entry.value, strategy)?;
                        }
                    }
                }
            }
            ExprKind::Range(range) => {
                if let Some(start) = range.start.as_mut() {
                    normalize_expr(start, strategy)?;
                }
                if let Some(end) = range.end.as_mut() {
                    normalize_expr(end, strategy)?;
                }
                if let Some(step) = range.step.as_mut() {
                    normalize_expr(step, strategy)?;
                }
            }
            ExprKind::Quote(q) => normalize_block(&mut q.block, strategy)?,
            ExprKind::Splice(s) => normalize_expr(s.token.as_mut(), strategy)?,
            ExprKind::Id(_) | ExprKind::Locator(_) | ExprKind::Any(_) => {}
        }
        if let Some(new_expr) = replacement {
            let old_ty = expr.ty.clone();
            *expr = new_expr.with_ty_slot(old_ty);
            continue;
        }

        break;
    }

    Ok(())
}

fn normalize_ty(ty: &mut Ty, strategy: &dyn IntrinsicNormalizer) -> Result<()> {
    match ty {
        Ty::Expr(expr) => normalize_expr(expr.as_mut(), strategy)?,
        Ty::Array(array) => {
            normalize_ty(array.elem.as_mut(), strategy)?;
            normalize_expr(array.len.as_mut(), strategy)?;
        }
        Ty::Vec(vec) => normalize_ty(vec.ty.as_mut(), strategy)?,
        Ty::Tuple(tuple) => {
            for entry in &mut tuple.types {
                normalize_ty(entry, strategy)?;
            }
        }
        Ty::Struct(struct_ty) => {
            for field in &mut struct_ty.fields {
                normalize_ty(&mut field.value, strategy)?;
            }
        }
        Ty::Structural(structural) => {
            for field in &mut structural.fields {
                normalize_ty(&mut field.value, strategy)?;
            }
        }
        Ty::Enum(enum_ty) => {
            for variant in &mut enum_ty.variants {
                normalize_ty(&mut variant.value, strategy)?;
                if let Some(discriminant) = variant.discriminant.as_mut() {
                    normalize_expr(discriminant.as_mut(), strategy)?;
                }
            }
        }
        Ty::Function(function) => {
            for param in &mut function.params {
                normalize_ty(param, strategy)?;
            }
            if let Some(ret) = function.ret_ty.as_mut() {
                normalize_ty(ret.as_mut(), strategy)?;
            }
        }
        Ty::ImplTraits(impl_traits) => {
            for bound in &mut impl_traits.bounds.bounds {
                normalize_expr(bound, strategy)?;
            }
        }
        Ty::TypeBounds(bounds) => {
            for bound in &mut bounds.bounds {
                normalize_expr(bound, strategy)?;
            }
        }
        Ty::Reference(reference) => normalize_ty(reference.ty.as_mut(), strategy)?,
        Ty::Slice(slice) => normalize_ty(slice.elem.as_mut(), strategy)?,
        Ty::Value(value) => {
            if let Value::Expr(expr) = value.value.as_mut() {
                normalize_expr(expr.as_mut(), strategy)?;
            }
        }
        Ty::QuoteToken(token) => {
            if let Some(inner) = token.inner.as_mut() {
                normalize_ty(inner.as_mut(), strategy)?;
            }
        }
        Ty::TypeBinaryOp(op) => {
            normalize_ty(op.lhs.as_mut(), strategy)?;
            normalize_ty(op.rhs.as_mut(), strategy)?;
        }
        Ty::Primitive(_)
        | Ty::Any(_)
        | Ty::Unit(_)
        | Ty::Unknown(_)
        | Ty::Nothing(_)
        | Ty::Type(_)
        | Ty::AnyBox(_) => {}
    }
    Ok(())
}

fn normalize_invoke(invoke: &mut ExprInvoke, strategy: &dyn IntrinsicNormalizer) -> Result<()> {
    match &mut invoke.target {
        ExprInvokeTarget::Expr(inner) => normalize_expr(inner.as_mut(), strategy)?,
        ExprInvokeTarget::Method(select) => normalize_expr(select.obj.as_mut(), strategy)?,
        ExprInvokeTarget::Closure(closure) => normalize_expr(closure.body.as_mut(), strategy)?,
        ExprInvokeTarget::Function(_) | ExprInvokeTarget::Type(_) | ExprInvokeTarget::BinOp(_) => {}
    }
    for arg in &mut invoke.args {
        normalize_expr(arg, strategy)?;
    }
    Ok(())
}

fn normalize_format_string(
    format_expr: &mut ExprFormatString,
    strategy: &dyn IntrinsicNormalizer,
) -> Result<()> {
    for arg in &mut format_expr.args {
        normalize_expr(arg, strategy)?;
    }
    for kwarg in &mut format_expr.kwargs {
        normalize_expr(&mut kwarg.value, strategy)?;
    }
    Ok(())
}

fn normalize_value(value: &mut Value, strategy: &dyn IntrinsicNormalizer) -> Result<()> {
    match value {
        Value::Expr(expr) => normalize_expr(expr.as_mut(), strategy),
        Value::Function(function) => normalize_expr(function.body.as_mut(), strategy),
        _ => Ok(()),
    }
}

fn apply_intrinsic_collection(
    collection: &mut ExprIntrinsicContainer,
    strategy: &dyn IntrinsicNormalizer,
) -> Result<Expr> {
    match collection {
        ExprIntrinsicContainer::VecElements { elements } => {
            for element in elements {
                normalize_expr(element, strategy)?;
            }
        }
        ExprIntrinsicContainer::VecRepeat { elem, len } => {
            normalize_expr(elem.as_mut(), strategy)?;
            normalize_expr(len.as_mut(), strategy)?;
        }
        ExprIntrinsicContainer::HashMapEntries { entries } => {
            for entry in entries {
                normalize_expr(&mut entry.key, strategy)?;
                normalize_expr(&mut entry.value, strategy)?;
            }
        }
    }

    Ok(Expr::new(ExprKind::IntrinsicContainer(collection.clone())))
}

fn normalize_intrinsic_call(
    call: &mut ExprIntrinsicCall,
    strategy: &dyn IntrinsicNormalizer,
) -> Result<()> {
    match &mut call.payload {
        crate::intrinsics::IntrinsicCallPayload::Format { template } => {
            normalize_format_string(template, strategy)?;
        }
        crate::intrinsics::IntrinsicCallPayload::Args { args } => {
            for arg in args.iter_mut() {
                normalize_expr(arg, strategy)?;
            }
            if matches!(
                call.kind,
                crate::intrinsics::IntrinsicCallKind::Print
                    | crate::intrinsics::IntrinsicCallKind::Println
            ) {
                if let Some(template) = format::convert_print_args_to_format(args) {
                    call.payload = crate::intrinsics::IntrinsicCallPayload::Format { template };
                }
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::intrinsics::normalize::bootstrap as b;
    use crate::ast::{FormatTemplatePart, Ident, Locator, Path};

    #[test]
    fn test_convert_print_args_to_format() {
        let lit = Expr::new(ExprKind::Value(Box::new(Value::string("hello".into()))));
        let out = crate::intrinsics::normalize::format::convert_print_args_to_format(&[lit])
            .expect("format");
        assert_eq!(out.parts.len(), 1);
        match &out.parts[0] {
            FormatTemplatePart::Literal(s) => assert_eq!(s, "hello"),
            _ => panic!("expected literal"),
        }
    }

    #[test]
    fn test_bootstrap_env_replacement() {
        std::env::set_var("FERROPHASE_BOOTSTRAP", "1");
        let path = Path::new(vec![
            Ident::new("std".to_string()),
            Ident::new("env".to_string()),
            Ident::new("var".to_string()),
        ]);
        let loc = Locator::Path(path);
        let invoke = ExprInvoke {
            target: ExprInvokeTarget::Function(loc),
            args: vec![],
        };
        let out = b::maybe_bootstrap_invoke_replacement(&invoke).expect("some");
        match out.kind() {
            ExprKind::Value(v) => match v.as_ref() {
                Value::String(s) => assert!(s.value.is_empty()),
                _ => panic!("expected string"),
            },
            _ => panic!("expected value"),
        }
        std::env::remove_var("FERROPHASE_BOOTSTRAP");
    }
}
