use super::super::*;
use fp_core::ast;
use fp_core::error::Result as CoreResult;
use fp_core::intrinsics::IntrinsicMaterializer;
use fp_llvm::runtime::LlvmRuntimeIntrinsicMaterializer;

impl Pipeline {
    pub(crate) fn stage_materialize_runtime_intrinsics(
        &self,
        ast: &mut Node,
        target: &PipelineTarget,
        options: &PipelineOptions,
        manager: &DiagnosticManager,
    ) -> Result<(), CliError> {
        if options.bootstrap_mode {
            return Ok(());
        }

        let materializer = IntrinsicsMaterializer::for_target(target);
        let result = materializer.materialize(ast);

        match result {
            Ok(()) => Ok(()),
            Err(err) => {
                manager.add_diagnostic(
                    Diagnostic::error(format!("Failed to materialize runtime intrinsics: {}", err))
                        .with_source_context(STAGE_RUNTIME_MATERIALIZE),
                );
                Err(Self::stage_failure(STAGE_RUNTIME_MATERIALIZE))
            }
        }
    }
}

struct IntrinsicsMaterializer {
    strategy: Box<dyn IntrinsicMaterializer>,
}

struct NoopIntrinsicMaterializer;
impl IntrinsicMaterializer for NoopIntrinsicMaterializer {}

impl IntrinsicsMaterializer {
    fn for_target(target: &PipelineTarget) -> Self {
        match target {
            PipelineTarget::Llvm | PipelineTarget::Binary => Self {
                strategy: Box::new(LlvmRuntimeIntrinsicMaterializer),
            },
            _ => Self {
                strategy: Box::new(NoopIntrinsicMaterializer),
            },
        }
    }

    fn materialize(&self, ast: &mut Node) -> CoreResult<()> {
        let updated = materialize_node(ast.clone(), self.strategy.as_ref())?;
        *ast = updated;
        Ok(())
    }
}

fn materialize_node(node: Node, strategy: &dyn IntrinsicMaterializer) -> CoreResult<Node> {
    let Node { ty, kind } = node;
    let new_kind = match kind {
        NodeKind::File(file) => NodeKind::File(materialize_file(file, strategy)?),
        NodeKind::Item(item) => NodeKind::Item(materialize_item(item, strategy)?),
        NodeKind::Expr(expr) => NodeKind::Expr(materialize_expr(expr, strategy)?),
        NodeKind::Query(query) => NodeKind::Query(query),
        NodeKind::Schema(schema) => NodeKind::Schema(schema),
        NodeKind::Workspace(workspace) => NodeKind::Workspace(workspace),
    };
    Ok(Node { ty, kind: new_kind })
}

fn materialize_file(mut file: ast::File, strategy: &dyn IntrinsicMaterializer) -> CoreResult<ast::File> {
    strategy.prepare_file(&mut file);
    let mut items = Vec::with_capacity(file.items.len());
    for item in file.items {
        items.push(materialize_item(item, strategy)?);
    }
    file.items = items;
    Ok(file)
}

fn materialize_item(item: ast::Item, strategy: &dyn IntrinsicMaterializer) -> CoreResult<ast::Item> {
    let ast::Item { ty, kind } = item;
    let new_kind = match kind {
        ast::ItemKind::Macro(item) => ast::ItemKind::Macro(item),
        ast::ItemKind::Module(mut module) => {
            let mut items = Vec::with_capacity(module.items.len());
            for child in module.items {
                items.push(materialize_item(child, strategy)?);
            }
            module.items = items;
            ast::ItemKind::Module(module)
        }
        ast::ItemKind::Impl(mut impl_block) => {
            let mut items = Vec::with_capacity(impl_block.items.len());
            for child in impl_block.items {
                items.push(materialize_item(child, strategy)?);
            }
            impl_block.items = items;
            ast::ItemKind::Impl(impl_block)
        }
        ast::ItemKind::DefFunction(mut func) => {
            func.body = Box::new(materialize_expr(*func.body, strategy)?);
            ast::ItemKind::DefFunction(func)
        }
        ast::ItemKind::DefConst(mut def) => {
            def.value = Box::new(materialize_expr(*def.value, strategy)?);
            ast::ItemKind::DefConst(def)
        }
        ast::ItemKind::DefStatic(mut def) => {
            def.value = Box::new(materialize_expr(*def.value, strategy)?);
            ast::ItemKind::DefStatic(def)
        }
        ast::ItemKind::Expr(expr) => {
            ast::ItemKind::Expr(materialize_expr(expr, strategy)?)
        }
        ast::ItemKind::DefStruct(_)
        | ast::ItemKind::DefStructural(_)
        | ast::ItemKind::DefEnum(_)
        | ast::ItemKind::DefType(_)
        | ast::ItemKind::DeclConst(_)
        | ast::ItemKind::DeclStatic(_)
        | ast::ItemKind::DeclFunction(_)
        | ast::ItemKind::DeclType(_)
        | ast::ItemKind::Import(_)
        | ast::ItemKind::DefTrait(_)
        | ast::ItemKind::Any(_) => kind,
    };
    Ok(ast::Item { ty, kind: new_kind })
}

fn materialize_block(block: ast::ExprBlock, strategy: &dyn IntrinsicMaterializer) -> CoreResult<ast::ExprBlock> {
    let mut stmts = Vec::with_capacity(block.stmts.len());
    for stmt in block.stmts {
        stmts.push(materialize_stmt(stmt, strategy)?);
    }
    Ok(ast::ExprBlock { stmts, ..block })
}

fn materialize_stmt(stmt: ast::BlockStmt, strategy: &dyn IntrinsicMaterializer) -> CoreResult<ast::BlockStmt> {
    match stmt {
        ast::BlockStmt::Expr(mut expr_stmt) => {
            expr_stmt.expr = Box::new(materialize_expr(*expr_stmt.expr, strategy)?);
            Ok(ast::BlockStmt::Expr(expr_stmt))
        }
        ast::BlockStmt::Let(mut stmt_let) => {
            if let Some(init) = stmt_let.init {
                stmt_let.init = Some(materialize_expr(init, strategy)?);
            }
            if let Some(diverge) = stmt_let.diverge {
                stmt_let.diverge = Some(materialize_expr(diverge, strategy)?);
            }
            Ok(ast::BlockStmt::Let(stmt_let))
        }
        ast::BlockStmt::Item(item) => {
            Ok(ast::BlockStmt::Item(Box::new(materialize_item(*item, strategy)?)))
        }
        ast::BlockStmt::Noop => Ok(ast::BlockStmt::Noop),
        ast::BlockStmt::Any(stmt) => Ok(ast::BlockStmt::Any(stmt)),
    }
}

fn materialize_expr(expr: ast::Expr, strategy: &dyn IntrinsicMaterializer) -> CoreResult<ast::Expr> {
    let ast::Expr { ty, kind } = expr;
    let expr_ty = ty.clone();
    let new_expr = match kind {
        ast::ExprKind::Block(block) => ast::Expr::with_ty(
            ast::ExprKind::Block(materialize_block(block, strategy)?),
            ty,
        ),
        ast::ExprKind::If(mut expr_if) => {
            expr_if.cond = Box::new(materialize_expr(*expr_if.cond, strategy)?);
            expr_if.then = Box::new(materialize_expr(*expr_if.then, strategy)?);
            if let Some(elze) = expr_if.elze {
                expr_if.elze = Some(Box::new(materialize_expr(*elze, strategy)?));
            }
            ast::Expr::with_ty(ast::ExprKind::If(expr_if), ty)
        }
        ast::ExprKind::Loop(mut expr_loop) => {
            expr_loop.body = Box::new(materialize_expr(*expr_loop.body, strategy)?);
            ast::Expr::with_ty(ast::ExprKind::Loop(expr_loop), ty)
        }
        ast::ExprKind::While(mut expr_while) => {
            expr_while.cond = Box::new(materialize_expr(*expr_while.cond, strategy)?);
            expr_while.body = Box::new(materialize_expr(*expr_while.body, strategy)?);
            ast::Expr::with_ty(ast::ExprKind::While(expr_while), ty)
        }
        ast::ExprKind::Match(mut match_expr) => {
            if let Some(scrutinee) = match_expr.scrutinee {
                match_expr.scrutinee = Some(Box::new(materialize_expr(*scrutinee, strategy)?));
            }
            let mut cases = Vec::with_capacity(match_expr.cases.len());
            for mut case in match_expr.cases {
                case.cond = Box::new(materialize_expr(*case.cond, strategy)?);
                if let Some(guard) = case.guard {
                    case.guard = Some(Box::new(materialize_expr(*guard, strategy)?));
                }
                case.body = Box::new(materialize_expr(*case.body, strategy)?);
                cases.push(case);
            }
            match_expr.cases = cases;
            ast::Expr::with_ty(ast::ExprKind::Match(match_expr), ty)
        }
        ast::ExprKind::Let(mut expr_let) => {
            expr_let.expr = Box::new(materialize_expr(*expr_let.expr, strategy)?);
            ast::Expr::with_ty(ast::ExprKind::Let(expr_let), ty)
        }
        ast::ExprKind::Assign(mut expr_assign) => {
            expr_assign.target = Box::new(materialize_expr(*expr_assign.target, strategy)?);
            expr_assign.value = Box::new(materialize_expr(*expr_assign.value, strategy)?);
            ast::Expr::with_ty(ast::ExprKind::Assign(expr_assign), ty)
        }
        ast::ExprKind::Invoke(mut invoke) => {
            invoke.target = materialize_invoke_target(invoke.target, strategy)?;
            let mut args = Vec::with_capacity(invoke.args.len());
            for arg in invoke.args {
                args.push(materialize_expr(arg, strategy)?);
            }
            invoke.args = args;
            ast::Expr::with_ty(ast::ExprKind::Invoke(invoke), ty)
        }
        ast::ExprKind::Select(mut select) => {
            select.obj = Box::new(materialize_expr(*select.obj, strategy)?);
            ast::Expr::with_ty(ast::ExprKind::Select(select), ty)
        }
        ast::ExprKind::Struct(mut struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.take() {
                    field.value = Some(materialize_expr(value, strategy)?);
                }
            }
            if let Some(new_expr) = strategy.materialize_struct(&mut struct_expr, &expr_ty)? {
                new_expr
            } else {
                ast::Expr::with_ty(ast::ExprKind::Struct(struct_expr), ty)
            }
        }
        ast::ExprKind::Structural(mut struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.take() {
                    field.value = Some(materialize_expr(value, strategy)?);
                }
            }
            if let Some(new_expr) = strategy.materialize_structural(&mut struct_expr, &expr_ty)? {
                new_expr
            } else {
                ast::Expr::with_ty(ast::ExprKind::Structural(struct_expr), ty)
            }
        }
        ast::ExprKind::Array(mut array_expr) => {
            let mut values = Vec::with_capacity(array_expr.values.len());
            for value in array_expr.values {
                values.push(materialize_expr(value, strategy)?);
            }
            array_expr.values = values;
            ast::Expr::with_ty(ast::ExprKind::Array(array_expr), ty)
        }
        ast::ExprKind::ArrayRepeat(mut array_repeat) => {
            array_repeat.elem = Box::new(materialize_expr(*array_repeat.elem, strategy)?);
            array_repeat.len = Box::new(materialize_expr(*array_repeat.len, strategy)?);
            ast::Expr::with_ty(ast::ExprKind::ArrayRepeat(array_repeat), ty)
        }
        ast::ExprKind::Tuple(mut tuple_expr) => {
            let mut values = Vec::with_capacity(tuple_expr.values.len());
            for value in tuple_expr.values {
                values.push(materialize_expr(value, strategy)?);
            }
            tuple_expr.values = values;
            ast::Expr::with_ty(ast::ExprKind::Tuple(tuple_expr), ty)
        }
        ast::ExprKind::BinOp(mut binop) => {
            binop.lhs = Box::new(materialize_expr(*binop.lhs, strategy)?);
            binop.rhs = Box::new(materialize_expr(*binop.rhs, strategy)?);
            ast::Expr::with_ty(ast::ExprKind::BinOp(binop), ty)
        }
        ast::ExprKind::UnOp(mut unop) => {
            unop.val = Box::new(materialize_expr(*unop.val, strategy)?);
            ast::Expr::with_ty(ast::ExprKind::UnOp(unop), ty)
        }
        ast::ExprKind::Reference(mut reference) => {
            reference.referee = Box::new(materialize_expr(*reference.referee, strategy)?);
            ast::Expr::with_ty(ast::ExprKind::Reference(reference), ty)
        }
        ast::ExprKind::Dereference(mut expr_deref) => {
            expr_deref.referee = Box::new(materialize_expr(*expr_deref.referee, strategy)?);
            ast::Expr::with_ty(ast::ExprKind::Dereference(expr_deref), ty)
        }
        ast::ExprKind::Index(mut expr_index) => {
            expr_index.obj = Box::new(materialize_expr(*expr_index.obj, strategy)?);
            expr_index.index = Box::new(materialize_expr(*expr_index.index, strategy)?);
            ast::Expr::with_ty(ast::ExprKind::Index(expr_index), ty)
        }
        ast::ExprKind::Splat(mut expr_splat) => {
            expr_splat.iter = Box::new(materialize_expr(*expr_splat.iter, strategy)?);
            ast::Expr::with_ty(ast::ExprKind::Splat(expr_splat), ty)
        }
        ast::ExprKind::SplatDict(mut expr_splat) => {
            expr_splat.dict = Box::new(materialize_expr(*expr_splat.dict, strategy)?);
            ast::Expr::with_ty(ast::ExprKind::SplatDict(expr_splat), ty)
        }
        ast::ExprKind::Try(mut expr_try) => {
            expr_try.expr = Box::new(materialize_expr(*expr_try.expr, strategy)?);
            ast::Expr::with_ty(ast::ExprKind::Try(expr_try), ty)
        }
        ast::ExprKind::Closure(mut closure) => {
            closure.body = Box::new(materialize_expr(*closure.body, strategy)?);
            ast::Expr::with_ty(ast::ExprKind::Closure(closure), ty)
        }
        ast::ExprKind::Closured(mut closured) => {
            closured.expr = Box::new(materialize_expr(*closured.expr, strategy)?);
            ast::Expr::with_ty(ast::ExprKind::Closured(closured), ty)
        }
        ast::ExprKind::Paren(mut paren) => {
            paren.expr = Box::new(materialize_expr(*paren.expr, strategy)?);
            ast::Expr::with_ty(ast::ExprKind::Paren(paren), ty)
        }
        ast::ExprKind::FormatString(mut format) => {
            let mut args = Vec::with_capacity(format.args.len());
            for arg in format.args {
                args.push(materialize_expr(arg, strategy)?);
            }
            format.args = args;
            for kwarg in &mut format.kwargs {
                let value = std::mem::replace(
                    &mut kwarg.value,
                    ast::Expr::value(ast::Value::unit()),
                );
                kwarg.value = materialize_expr(value, strategy)?;
            }
            ast::Expr::with_ty(ast::ExprKind::FormatString(format), ty)
        }
        ast::ExprKind::Item(item) => {
            ast::Expr::with_ty(ast::ExprKind::Item(Box::new(materialize_item(*item, strategy)?)), ty)
        }
        ast::ExprKind::Value(value) => {
            let value = materialize_value(*value, strategy)?;
            ast::Expr::with_ty(ast::ExprKind::Value(Box::new(value)), ty)
        }
        ast::ExprKind::IntrinsicCall(mut call) => {
            match &mut call.payload {
                fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                    let mut args = Vec::with_capacity(template.args.len());
                    for arg in template.args.drain(..) {
                        args.push(materialize_expr(arg, strategy)?);
                    }
                    template.args = args;
                    for kwarg in &mut template.kwargs {
                        let value = std::mem::replace(
                            &mut kwarg.value,
                            ast::Expr::value(ast::Value::unit()),
                        );
                        kwarg.value = materialize_expr(value, strategy)?;
                    }
                }
                fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                    let mut next = Vec::with_capacity(args.len());
                    for arg in args.drain(..) {
                        next.push(materialize_expr(arg, strategy)?);
                    }
                    *args = next;
                }
            }

            if let Some(expr) = strategy.materialize_call(&mut call, &expr_ty)? {
                expr
            } else {
                ast::Expr::with_ty(ast::ExprKind::IntrinsicCall(call), ty)
            }
        }
        ast::ExprKind::IntrinsicContainer(mut collection) => {
            match &mut collection {
                ast::ExprIntrinsicContainer::VecElements { elements } => {
                    let mut next = Vec::with_capacity(elements.len());
                    for element in elements.drain(..) {
                        next.push(materialize_expr(element, strategy)?);
                    }
                    *elements = next;
                }
                ast::ExprIntrinsicContainer::VecRepeat { elem, len } => {
                    let elem_value = std::mem::replace(
                        elem,
                        Box::new(ast::Expr::value(ast::Value::unit())),
                    );
                    let len_value = std::mem::replace(
                        len,
                        Box::new(ast::Expr::value(ast::Value::unit())),
                    );
                    *elem = Box::new(materialize_expr(*elem_value, strategy)?);
                    *len = Box::new(materialize_expr(*len_value, strategy)?);
                }
                ast::ExprIntrinsicContainer::HashMapEntries { entries } => {
                    for entry in entries.iter_mut() {
                        let key = std::mem::replace(
                            &mut entry.key,
                            ast::Expr::value(ast::Value::unit()),
                        );
                        let value = std::mem::replace(
                            &mut entry.value,
                            ast::Expr::value(ast::Value::unit()),
                        );
                        entry.key = materialize_expr(key, strategy)?;
                        entry.value = materialize_expr(value, strategy)?;
                    }
                }
            }

            if let Some(new_expr) = strategy.materialize_container(&mut collection, &expr_ty)? {
                new_expr
            } else {
                ast::Expr::with_ty(ast::ExprKind::IntrinsicContainer(collection), ty)
            }
        }
        other => ast::Expr::with_ty(other, ty),
    };
    Ok(new_expr)
}

fn materialize_value(value: ast::Value, strategy: &dyn IntrinsicMaterializer) -> CoreResult<ast::Value> {
    match value {
        ast::Value::Expr(expr) => Ok(ast::Value::Expr(Box::new(materialize_expr(*expr, strategy)?))),
        ast::Value::Function(mut func) => {
            func.body = Box::new(materialize_expr(*func.body, strategy)?);
            Ok(ast::Value::Function(func))
        }
        other => Ok(other),
    }
}

fn materialize_invoke_target(
    target: ast::ExprInvokeTarget,
    strategy: &dyn IntrinsicMaterializer,
) -> CoreResult<ast::ExprInvokeTarget> {
    match target {
        ast::ExprInvokeTarget::Method(mut select) => {
            select.obj = Box::new(materialize_expr(*select.obj, strategy)?);
            Ok(ast::ExprInvokeTarget::Method(select))
        }
        ast::ExprInvokeTarget::Expr(expr) => {
            Ok(ast::ExprInvokeTarget::Expr(Box::new(materialize_expr(*expr, strategy)?)))
        }
        ast::ExprInvokeTarget::Closure(mut closure) => {
            closure.body = Box::new(materialize_expr(*closure.body, strategy)?);
            Ok(ast::ExprInvokeTarget::Closure(closure))
        }
        other => Ok(other),
    }
}
