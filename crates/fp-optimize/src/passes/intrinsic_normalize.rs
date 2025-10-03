use fp_core::ast::{
    BlockStmt, Expr, ExprBlock, ExprFormatString, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget,
    ExprKind, FormatTemplatePart, Item, ItemKind, Node, NodeKind, Value,
};
use fp_core::error::Result;
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};

/// Normalize intrinsic expressions into a canonical AST form so that typing and
/// downstream passes can assume consistent structures.
pub fn normalize_intrinsics(node: &mut Node) -> Result<()> {
    normalize_node(node)
}

fn normalize_node(node: &mut Node) -> Result<()> {
    match node.kind_mut() {
        NodeKind::File(file) => {
            for item in &mut file.items {
                normalize_item(item)?;
            }
        }
        NodeKind::Item(item) => normalize_item(item)?,
        NodeKind::Expr(expr) => normalize_expr(expr)?,
    }
    Ok(())
}

fn normalize_item(item: &mut Item) -> Result<()> {
    match item.kind_mut() {
        ItemKind::Module(module) => {
            for child in &mut module.items {
                normalize_item(child)?;
            }
        }
        ItemKind::Impl(impl_block) => {
            for child in &mut impl_block.items {
                normalize_item(child)?;
            }
        }
        ItemKind::DefFunction(function) => normalize_expr(function.body.as_mut())?,
        ItemKind::DefConst(def) => normalize_expr(def.value.as_mut())?,
        ItemKind::DefStatic(def) => normalize_expr(def.value.as_mut())?,
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
        ItemKind::Expr(expr) => normalize_expr(expr)?,
    }
    Ok(())
}

fn normalize_block(block: &mut ExprBlock) -> Result<()> {
    for stmt in &mut block.stmts {
        match stmt {
            BlockStmt::Expr(expr_stmt) => normalize_expr(expr_stmt.expr.as_mut())?,
            BlockStmt::Let(stmt_let) => {
                if let Some(init) = stmt_let.init.as_mut() {
                    normalize_expr(init)?;
                }
                if let Some(diverge) = stmt_let.diverge.as_mut() {
                    normalize_expr(diverge)?;
                }
            }
            BlockStmt::Item(item) => normalize_item(item.as_mut())?,
            BlockStmt::Noop | BlockStmt::Any(_) => {}
        }
    }
    Ok(())
}

fn normalize_expr(expr: &mut Expr) -> Result<()> {
    match expr.kind_mut() {
        ExprKind::Block(block) => normalize_block(block)?,
        ExprKind::If(expr_if) => {
            normalize_expr(expr_if.cond.as_mut())?;
            normalize_expr(expr_if.then.as_mut())?;
            if let Some(else_expr) = expr_if.elze.as_mut() {
                normalize_expr(else_expr)?;
            }
        }
        ExprKind::Loop(expr_loop) => normalize_expr(expr_loop.body.as_mut())?,
        ExprKind::While(expr_while) => {
            normalize_expr(expr_while.cond.as_mut())?;
            normalize_expr(expr_while.body.as_mut())?;
        }
        ExprKind::Match(expr_match) => {
            for case in &mut expr_match.cases {
                normalize_expr(case.cond.as_mut())?;
                normalize_expr(case.body.as_mut())?;
            }
        }
        ExprKind::Let(expr_let) => normalize_expr(expr_let.expr.as_mut())?,
        ExprKind::Assign(assign) => {
            normalize_expr(assign.target.as_mut())?;
            normalize_expr(assign.value.as_mut())?;
        }
        ExprKind::Invoke(invoke) => normalize_invoke(invoke)?,
        ExprKind::Select(select) => normalize_expr(select.obj.as_mut())?,
        ExprKind::Struct(struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    normalize_expr(value)?;
                }
            }
        }
        ExprKind::Structural(struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    normalize_expr(value)?;
                }
            }
        }
        ExprKind::Array(array_expr) => {
            for value in &mut array_expr.values {
                normalize_expr(value)?;
            }
        }
        ExprKind::ArrayRepeat(array_repeat) => {
            normalize_expr(array_repeat.elem.as_mut())?;
            normalize_expr(array_repeat.len.as_mut())?;
        }
        ExprKind::Tuple(tuple_expr) => {
            for value in &mut tuple_expr.values {
                normalize_expr(value)?;
            }
        }
        ExprKind::BinOp(binop) => {
            normalize_expr(binop.lhs.as_mut())?;
            normalize_expr(binop.rhs.as_mut())?;
        }
        ExprKind::UnOp(unop) => normalize_expr(unop.val.as_mut())?,
        ExprKind::Reference(reference) => normalize_expr(reference.referee.as_mut())?,
        ExprKind::Dereference(deref) => normalize_expr(deref.referee.as_mut())?,
        ExprKind::Index(index) => {
            normalize_expr(index.obj.as_mut())?;
            normalize_expr(index.index.as_mut())?;
        }
        ExprKind::Splat(splat) => normalize_expr(splat.iter.as_mut())?,
        ExprKind::SplatDict(splat) => normalize_expr(splat.dict.as_mut())?,
        ExprKind::Try(expr_try) => normalize_expr(expr_try.expr.as_mut())?,
        ExprKind::Closure(closure) => normalize_expr(closure.body.as_mut())?,
        ExprKind::Closured(closured) => normalize_expr(closured.expr.as_mut())?,
        ExprKind::Paren(paren) => normalize_expr(paren.expr.as_mut())?,
        ExprKind::FormatString(format) => normalize_format_string(format)?,
        ExprKind::Item(item) => normalize_item(item.as_mut())?,
        ExprKind::Value(value) => normalize_value(value)?,
        ExprKind::IntrinsicCall(call) => normalize_intrinsic_call(call)?,
        ExprKind::Range(range) => {
            if let Some(start) = range.start.as_mut() {
                normalize_expr(start)?;
            }
            if let Some(end) = range.end.as_mut() {
                normalize_expr(end)?;
            }
            if let Some(step) = range.step.as_mut() {
                normalize_expr(step)?;
            }
        }
        ExprKind::Id(_) | ExprKind::Locator(_) | ExprKind::Any(_) => {}
    }
    Ok(())
}

fn normalize_invoke(invoke: &mut ExprInvoke) -> Result<()> {
    match &mut invoke.target {
        ExprInvokeTarget::Expr(inner) => normalize_expr(inner.as_mut())?,
        ExprInvokeTarget::Method(select) => normalize_expr(select.obj.as_mut())?,
        ExprInvokeTarget::Closure(closure) => normalize_expr(closure.body.as_mut())?,
        ExprInvokeTarget::Function(_) | ExprInvokeTarget::Type(_) | ExprInvokeTarget::BinOp(_) => {}
    }

    for arg in &mut invoke.args {
        normalize_expr(arg)?;
    }
    Ok(())
}

fn normalize_format_string(format: &mut ExprFormatString) -> Result<()> {
    for arg in &mut format.args {
        normalize_expr(arg)?;
    }
    for kwarg in &mut format.kwargs {
        normalize_expr(&mut kwarg.value)?;
    }
    Ok(())
}

fn normalize_value(value: &mut Value) -> Result<()> {
    match value {
        Value::Expr(expr) => normalize_expr(expr.as_mut()),
        Value::Function(function) => normalize_expr(function.body.as_mut()),
        _ => Ok(()),
    }
}

fn normalize_intrinsic_call(call: &mut ExprIntrinsicCall) -> Result<()> {
    match &mut call.payload {
        IntrinsicCallPayload::Format { template } => normalize_format_string(template),
        IntrinsicCallPayload::Args { args } => {
            for arg in args.iter_mut() {
                normalize_expr(arg)?;
            }

            if matches!(
                call.kind,
                IntrinsicCallKind::Print | IntrinsicCallKind::Println
            ) {
                if let Some(template) = convert_print_args_to_format(args) {
                    call.payload = IntrinsicCallPayload::Format { template };
                }
            }
            Ok(())
        }
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
