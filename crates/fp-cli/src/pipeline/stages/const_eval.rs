use super::super::*;
use fp_core::ast::Node;
use fp_core::ast::{
    BlockStmt, Expr, ExprBlock, ExprKind, ItemKind, NodeKind, Ty, Value,
};

impl Pipeline {
    pub(crate) fn stage_const_eval(
        &mut self,
        ast: &mut Node,
        options: &PipelineOptions,
        manager: &DiagnosticManager,
    ) -> Result<ConstEvalOutcome, CliError> {
        let serializer = self.serializer.clone().ok_or_else(|| {
            CliError::Compilation("No serializer registered for const-eval".to_string())
        })?;
        register_threadlocal_serializer(serializer.clone());

        let shared_context = SharedScopedContext::new();
        let mut orchestrator = ConstEvaluationOrchestrator::new(serializer);
        orchestrator.set_debug_assertions(!options.release);
        orchestrator.set_execute_main(options.execute_main);

        let outcome = match orchestrator.evaluate(ast, &shared_context) {
            Ok(outcome) => outcome,
            Err(e) => {
                let diagnostic = Diagnostic::error(format!("Const evaluation failed: {}", e))
                    .with_source_context(STAGE_CONST_EVAL);
                manager.add_diagnostic(diagnostic);
                return Err(Self::stage_failure(STAGE_CONST_EVAL));
            }
        };

        manager.add_diagnostics(outcome.diagnostics.clone());
        if outcome.has_errors {
            return Err(Self::stage_failure(STAGE_CONST_EVAL));
        }

        // Quote tokens are compile-time only; remove const items that would
        // otherwise leak quote values/types into runtime lowering.
        strip_quote_consts(ast);

        Ok(outcome)
    }
}

fn strip_quote_consts(node: &mut Node) {
    match node.kind_mut() {
        NodeKind::File(file) => {
            file.items.retain_mut(|item| strip_quote_consts_in_item(item));
        }
        NodeKind::Item(item) => {
            let _ = strip_quote_consts_in_item(item);
        }
        NodeKind::Expr(_) | NodeKind::Query(_) | NodeKind::Schema(_) | NodeKind::Workspace(_) => {}
    }
}

fn strip_quote_consts_in_item(item: &mut fp_core::ast::Item) -> bool {
    match item.kind_mut() {
        ItemKind::DefConst(def) => {
            if is_quote_type(def.ty.as_ref()) || is_quote_type(def.ty_annotation.as_ref()) {
                return false;
            }
            if is_quote_expr(&def.value) {
                return false;
            }
            true
        }
        ItemKind::DefFunction(func) => {
            strip_quote_consts_in_expr(func.body.as_mut());
            true
        }
        ItemKind::Impl(impl_block) => {
            for member in &mut impl_block.items {
                let _ = strip_quote_consts_in_item(member);
            }
            true
        }
        ItemKind::Module(module) => {
            module.items.retain_mut(|child| strip_quote_consts_in_item(child));
            true
        }
        _ => true,
    }
}

fn strip_quote_consts_in_block(block: &mut ExprBlock) {
    block.stmts.retain_mut(|stmt| match stmt {
        BlockStmt::Item(item) => strip_quote_consts_in_item(item),
        BlockStmt::Expr(expr_stmt) => {
            strip_quote_consts_in_expr(expr_stmt.expr.as_mut());
            true
        }
        BlockStmt::Let(stmt_let) => {
            if let Some(init) = stmt_let.init.as_mut() {
                strip_quote_consts_in_expr(init);
            }
            if let Some(diverge) = stmt_let.diverge.as_mut() {
                strip_quote_consts_in_expr(diverge);
            }
            true
        }
        BlockStmt::Noop | BlockStmt::Any(_) => true,
    });
    if let Some(last) = block.last_expr_mut() {
        strip_quote_consts_in_expr(last);
    }
}

fn strip_quote_consts_in_expr(expr: &mut Expr) {
    match expr.kind_mut() {
        ExprKind::Block(block) => strip_quote_consts_in_block(block),
        ExprKind::If(expr_if) => {
            strip_quote_consts_in_expr(expr_if.cond.as_mut());
            strip_quote_consts_in_expr(expr_if.then.as_mut());
            if let Some(otherwise) = expr_if.elze.as_mut() {
                strip_quote_consts_in_expr(otherwise.as_mut());
            }
        }
        ExprKind::Loop(expr_loop) => strip_quote_consts_in_expr(expr_loop.body.as_mut()),
        ExprKind::While(expr_while) => {
            strip_quote_consts_in_expr(expr_while.cond.as_mut());
            strip_quote_consts_in_expr(expr_while.body.as_mut());
        }
        ExprKind::For(expr_for) => {
            strip_quote_consts_in_expr(expr_for.iter.as_mut());
            strip_quote_consts_in_expr(expr_for.body.as_mut());
        }
        ExprKind::Match(expr_match) => {
            for case in &mut expr_match.cases {
                strip_quote_consts_in_expr(case.cond.as_mut());
                strip_quote_consts_in_expr(case.body.as_mut());
            }
        }
        ExprKind::Invoke(invoke) => {
            if let fp_core::ast::ExprInvokeTarget::Expr(target) = &mut invoke.target {
                strip_quote_consts_in_expr(target.as_mut());
            }
            for arg in &mut invoke.args {
                strip_quote_consts_in_expr(arg);
            }
        }
        ExprKind::Select(select) => strip_quote_consts_in_expr(select.obj.as_mut()),
        ExprKind::Assign(assign) => {
            strip_quote_consts_in_expr(assign.target.as_mut());
            strip_quote_consts_in_expr(assign.value.as_mut());
        }
        ExprKind::Paren(paren) => strip_quote_consts_in_expr(paren.expr.as_mut()),
        ExprKind::Array(array) => {
            for value in &mut array.values {
                strip_quote_consts_in_expr(value);
            }
        }
        ExprKind::ArrayRepeat(repeat) => {
            strip_quote_consts_in_expr(repeat.elem.as_mut());
            strip_quote_consts_in_expr(repeat.len.as_mut());
        }
        ExprKind::Struct(struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    strip_quote_consts_in_expr(value);
                }
            }
        }
        ExprKind::Quote(quote) => {
            strip_quote_consts_in_block(&mut quote.block);
        }
        ExprKind::Splice(splice) => strip_quote_consts_in_expr(splice.token.as_mut()),
        ExprKind::Let(expr_let) => strip_quote_consts_in_expr(expr_let.expr.as_mut()),
        ExprKind::Try(expr_try) => strip_quote_consts_in_expr(expr_try.expr.as_mut()),
        ExprKind::Index(index_expr) => {
            strip_quote_consts_in_expr(index_expr.obj.as_mut());
            strip_quote_consts_in_expr(index_expr.index.as_mut());
        }
        ExprKind::Cast(cast) => strip_quote_consts_in_expr(cast.expr.as_mut()),
        ExprKind::UnOp(unop) => strip_quote_consts_in_expr(unop.val.as_mut()),
        ExprKind::BinOp(binop) => {
            strip_quote_consts_in_expr(binop.lhs.as_mut());
            strip_quote_consts_in_expr(binop.rhs.as_mut());
        }
        ExprKind::IntrinsicCall(call) => match &mut call.payload {
            fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                for arg in args {
                    strip_quote_consts_in_expr(arg);
                }
            }
            fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                for arg in &mut template.args {
                    strip_quote_consts_in_expr(arg);
                }
                for kwarg in &mut template.kwargs {
                    strip_quote_consts_in_expr(&mut kwarg.value);
                }
            }
        },
        ExprKind::Value(_)
        | ExprKind::Locator(_)
        | ExprKind::Macro(_)
        | ExprKind::Any(_)
        | ExprKind::Id(_)
        | ExprKind::Closure(_)
        | ExprKind::Closured(_)
        | ExprKind::Splat(_)
        | ExprKind::SplatDict(_) => {}
        _ => {}
    }
}

fn is_quote_type(ty: Option<&Ty>) -> bool {
    matches!(
        ty,
        Some(
            Ty::QuoteExpr(_)
                | Ty::QuoteStmt(_)
                | Ty::QuoteItem(_)
                | Ty::QuoteFn(_)
                | Ty::QuoteStruct(_)
                | Ty::QuoteEnum(_)
                | Ty::QuoteTrait(_)
                | Ty::QuoteImpl(_)
                | Ty::QuoteConst(_)
                | Ty::QuoteStatic(_)
                | Ty::QuoteMod(_)
                | Ty::QuoteUse(_)
                | Ty::QuoteMacro(_)
                | Ty::QuoteType(_)
                | Ty::QuoteToken(_)
        )
    )
}

fn is_quote_expr(expr: &fp_core::ast::Expr) -> bool {
    match expr.kind() {
        ExprKind::Quote(_) => true,
        ExprKind::Value(value) => matches!(value.as_ref(), Value::QuoteToken(_)),
        _ => false,
    }
}
