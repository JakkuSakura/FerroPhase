use fp_core::ast::{
    BlockStmt, Expr, ExprBlock, ExprFormatString, ExprIntrinsicCall, ExprIntrinsicCollection,
    ExprInvoke, ExprInvokeTarget, ExprKind, FormatTemplatePart, Item, ItemKind, Node, NodeKind,
    Value,
};
use fp_core::error::Result;
use fp_core::intrinsics::IntrinsicNormalizer;
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};

/// Normalize intrinsic expressions into a canonical AST form so that typing and
/// downstream passes can assume consistent structures.
pub fn normalize_intrinsics(node: &mut Node) -> Result<()> {
    normalize_intrinsics_with(node, None)
}

pub fn normalize_intrinsics_with(
    node: &mut Node,
    strategy: Option<&dyn IntrinsicNormalizer>,
) -> Result<()> {
    normalize_node(node, strategy)
}

/// Default normalizer that applies the shared intrinsic-normalization pass.
#[derive(Debug, Default, Clone, Copy)]
pub struct DefaultIntrinsicNormalizer;

impl IntrinsicNormalizer for DefaultIntrinsicNormalizer {
    fn normalize(&self, node: &mut Node) -> Result<()> {
        normalize_intrinsics_with(node, Some(self))
    }
}

fn normalize_node(node: &mut Node, strategy: Option<&dyn IntrinsicNormalizer>) -> Result<()> {
    match node.kind_mut() {
        NodeKind::File(file) => {
            for item in &mut file.items {
                normalize_item(item, strategy)?;
            }
        }
        NodeKind::Item(item) => normalize_item(item, strategy)?,
        NodeKind::Expr(expr) => normalize_expr(expr, strategy)?,
        NodeKind::Query(_) => {}
    }
    Ok(())
}

fn normalize_item(item: &mut Item, strategy: Option<&dyn IntrinsicNormalizer>) -> Result<()> {
    match item.kind_mut() {
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
        ItemKind::DefFunction(function) => normalize_expr(function.body.as_mut(), strategy)?,
        ItemKind::DefConst(def) => normalize_expr(def.value.as_mut(), strategy)?,
        ItemKind::DefStatic(def) => normalize_expr(def.value.as_mut(), strategy)?,
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
        ItemKind::Expr(expr) => normalize_expr(expr, strategy)?,
    }
    Ok(())
}

fn normalize_block(
    block: &mut ExprBlock,
    strategy: Option<&dyn IntrinsicNormalizer>,
) -> Result<()> {
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

fn normalize_expr(expr: &mut Expr, strategy: Option<&dyn IntrinsicNormalizer>) -> Result<()> {
    let mut replacement: Option<Expr> = None;

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
        ExprKind::Assign(assign) => {
            normalize_expr(assign.target.as_mut(), strategy)?;
            normalize_expr(assign.value.as_mut(), strategy)?;
        }
        ExprKind::Cast(cast) => {
            normalize_expr(cast.expr.as_mut(), strategy)?;
        }
        ExprKind::Invoke(invoke) => {
            normalize_invoke(invoke, strategy)?;
            if let Some(mut collection) = ExprIntrinsicCollection::from_invoke(invoke) {
                let new_expr = apply_intrinsic_collection(&mut collection, strategy)?;
                replacement = Some(new_expr);
            }
        }
        ExprKind::Select(select) => normalize_expr(select.obj.as_mut(), strategy)?,
        ExprKind::Struct(struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    normalize_expr(value, strategy)?;
                }
            }
            if let Some(strat) = strategy {
                if let Some(new_expr) = strat.normalize_struct(struct_expr)? {
                    replacement = Some(new_expr);
                }
            }
        }
        ExprKind::Structural(struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    normalize_expr(value, strategy)?;
                }
            }
            if let Some(strat) = strategy {
                if let Some(new_expr) = strat.normalize_structural(struct_expr)? {
                    replacement = Some(new_expr);
                }
            }
        }
        ExprKind::Array(array_expr) => {
            for value in &mut array_expr.values {
                normalize_expr(value, strategy)?;
            }
        }
        ExprKind::ArrayRepeat(array_repeat) => {
            normalize_expr(array_repeat.elem.as_mut(), strategy)?;
            normalize_expr(array_repeat.len.as_mut(), strategy)?;
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
        ExprKind::Closure(closure) => normalize_expr(closure.body.as_mut(), strategy)?,
        ExprKind::Closured(closured) => normalize_expr(closured.expr.as_mut(), strategy)?,
        ExprKind::Paren(paren) => normalize_expr(paren.expr.as_mut(), strategy)?,
        ExprKind::FormatString(format) => normalize_format_string(format, strategy)?,
        ExprKind::Item(item) => normalize_item(item.as_mut(), strategy)?,
        ExprKind::Value(value) => normalize_value(value, strategy)?,
        ExprKind::IntrinsicCall(call) => {
            if let Some(new_expr) = normalize_intrinsic_call(call, strategy)? {
                replacement = Some(new_expr);
            }
        }
        ExprKind::IntrinsicCollection(collection) => {
            let new_expr = apply_intrinsic_collection(collection, strategy)?;
            replacement = Some(new_expr);
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
        ExprKind::Id(_) | ExprKind::Locator(_) | ExprKind::Any(_) => {}
    }
    if let Some(new_expr) = replacement {
        *expr = new_expr;
    }
    Ok(())
}

fn normalize_invoke(
    invoke: &mut ExprInvoke,
    strategy: Option<&dyn IntrinsicNormalizer>,
) -> Result<()> {
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
    format: &mut ExprFormatString,
    strategy: Option<&dyn IntrinsicNormalizer>,
) -> Result<()> {
    for arg in &mut format.args {
        normalize_expr(arg, strategy)?;
    }
    for kwarg in &mut format.kwargs {
        normalize_expr(&mut kwarg.value, strategy)?;
    }
    Ok(())
}

fn normalize_value(value: &mut Value, strategy: Option<&dyn IntrinsicNormalizer>) -> Result<()> {
    match value {
        Value::Expr(expr) => normalize_expr(expr.as_mut(), strategy),
        Value::Function(function) => normalize_expr(function.body.as_mut(), strategy),
        _ => Ok(()),
    }
}

fn apply_intrinsic_collection(
    collection: &mut ExprIntrinsicCollection,
    strategy: Option<&dyn IntrinsicNormalizer>,
) -> Result<Expr> {
    match collection {
        ExprIntrinsicCollection::VecElements { elements } => {
            for element in elements {
                normalize_expr(element, strategy)?;
            }
        }
        ExprIntrinsicCollection::VecRepeat { elem, len } => {
            normalize_expr(elem.as_mut(), strategy)?;
            normalize_expr(len.as_mut(), strategy)?;
        }
        ExprIntrinsicCollection::HashMapEntries { entries } => {
            for entry in entries {
                normalize_expr(&mut entry.key, strategy)?;
                normalize_expr(&mut entry.value, strategy)?;
            }
        }
    }

    if let Some(strat) = strategy {
        if let Some(expr) = strat.normalize_collection(collection)? {
            return Ok(expr);
        }
    }

    Ok(collection.clone().into_const_expr())
}

fn normalize_intrinsic_call(
    call: &mut ExprIntrinsicCall,
    strategy: Option<&dyn IntrinsicNormalizer>,
) -> Result<Option<Expr>> {
    match &mut call.payload {
        IntrinsicCallPayload::Format { template } => {
            normalize_format_string(template, strategy)?;
        }
        IntrinsicCallPayload::Args { args } => {
            for arg in args.iter_mut() {
                normalize_expr(arg, strategy)?;
            }

            if matches!(
                call.kind,
                IntrinsicCallKind::Print | IntrinsicCallKind::Println
            ) {
                if let Some(template) = convert_print_args_to_format(args) {
                    call.payload = IntrinsicCallPayload::Format { template };
                }
            }
        }
    }

    if let Some(strat) = strategy {
        strat.normalize_call(call)
    } else {
        Ok(None)
    }
}

fn convert_print_args_to_format(args: &[Expr]) -> Option<ExprFormatString> {
    match args.len() {
        0 => Some(ExprFormatString {
            parts: vec![FormatTemplatePart::Literal(String::new())],
            args: Vec::new(),
            kwargs: Vec::new(),
        }),
        1 => {
            if let Some(literal) = extract_string_literal(&args[0]) {
                Some(ExprFormatString {
                    parts: vec![FormatTemplatePart::Literal(literal)],
                    args: Vec::new(),
                    kwargs: Vec::new(),
                })
            } else {
                None
            }
        }
        _ => None,
    }
}

fn extract_string_literal(expr: &Expr) -> Option<String> {
    match expr.kind() {
        ExprKind::Value(value) => match value.as_ref() {
            Value::String(string) => Some(string.value.clone()),
            _ => None,
        },
        _ => None,
    }
}
