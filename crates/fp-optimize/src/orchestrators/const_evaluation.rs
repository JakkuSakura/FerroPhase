// Orchestrator: 3-Phase Const Evaluation System
// Implements the comprehensive const evaluation system from ConstEval.md

use crate::error::optimization_error;
use crate::orchestrators::InterpretationOrchestrator;
use crate::queries::{DependencyQueries, TypeQueries};
use crate::utils::{ConstEval, ConstEvalTracker, EvaluationContext, IntrinsicEvaluationContext};
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::error::Result;
use fp_core::id::{Ident, Locator};
use std::sync::Arc;
use tracing::{debug, info};

/// 3-Phase Const Evaluation Orchestrator
///
/// This is an orchestrator that coordinates the entire const evaluation process
/// following the 3-phase system described in ConstEval.md:
///
/// Phase 1: Basic Type Checking
/// Phase 2: Const Evaluation & Metaprogramming  
/// Phase 3: Final Type Checking
///
/// This orchestrator coordinates the 3-phase const evaluation process
/// but delegates actual expression evaluation to the InterpretationOrchestrator.
/// It does NOT implement OptimizePass because it's a coordinator that manages
/// the overall process and modifies the AST in-place.
pub struct ConstEvaluationOrchestrator {
    /// Type queries for introspection
    type_queries: TypeQueries,

    /// Dependency analysis
    dependency_queries: DependencyQueries,

    /// Const-eval mutation staging and application
    const_eval_tracker: ConstEvalTracker,

    /// Context for intrinsic evaluation
    intrinsic_context: IntrinsicEvaluationContext,

    /// Orchestrator for interpretation
    interpreter: InterpretationOrchestrator,

    /// Evaluation context for tracking const blocks
    evaluation_context: EvaluationContext,
}

impl ConstEvaluationOrchestrator {
    pub fn new(serializer: Arc<dyn AstSerializer>) -> Self {
        let type_registry = Arc::new(fp_core::ctx::ty::TypeRegistry::new());
        let intrinsic_context = IntrinsicEvaluationContext::new(type_registry.clone());

        Self {
            type_queries: TypeQueries::new(type_registry.clone()),
            dependency_queries: DependencyQueries::new(),
            const_eval_tracker: ConstEvalTracker::new(),
            intrinsic_context,
            interpreter: InterpretationOrchestrator::new(serializer.clone()),
            evaluation_context: EvaluationContext::new(),
        }
    }

    /// Execute the complete 3-phase const evaluation
    pub fn evaluate(&mut self, ast: &mut Node, ctx: &SharedScopedContext) -> Result<()> {
        info!("Starting 3-phase const evaluation");

        // Phase 1: Basic Type Checking
        self.phase1_basic_type_checking(ast, ctx)?;

        // Phase 2: Const Evaluation & Metaprogramming
        self.phase2_const_evaluation_metaprogramming(ast, ctx)?;

        // Phase 3: Final Type Checking
        self.phase3_final_type_checking(ast, ctx)?;

        info!("3-phase const evaluation completed successfully");
        Ok(())
    }

    /// Phase 1: Basic Type Checking
    /// Parse AST and build initial type registry
    /// Type check non-const code (functions, structs, regular expressions)
    /// Validate basic type references and struct definitions
    /// Establish baseline type system that const evaluation can query
    /// Skip const blocks - treat them as opaque for now
    fn phase1_basic_type_checking(&mut self, ast: &Node, ctx: &SharedScopedContext) -> Result<()> {
        debug!("Phase 1: Basic Type Checking");

        // Register basic types from the AST
        self.type_queries.register_basic_types(ast)?;

        // Make type definitions available to the interpretation context
        self.register_type_bindings(ast, ctx);

        // Validate non-const type references
        self.type_queries.validate_basic_references(ast, ctx)?;

        debug!("Phase 1 completed: Basic type system established");
        Ok(())
    }

    /// Phase 2: Const Evaluation & Metaprogramming
    /// Discover const blocks and build dependency graph
    /// Evaluate const expressions in topological order
    /// Execute intrinsics like sizeof!, create_struct!, addfield!
    /// Collect const-eval mutations (generated fields, methods, new types)
    /// Apply metaprogramming changes to AST
    /// Query established types from Phase 1 as needed
    fn phase2_const_evaluation_metaprogramming(
        &mut self,
        ast: &mut Node,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        debug!("Phase 2: Const Evaluation & Metaprogramming");

        // Discover const blocks
        self.evaluation_context.discover_const_blocks(ast)?;

        // Build dependency graph
        let dependencies = self
            .dependency_queries
            .analyze_dependencies(&self.evaluation_context.get_const_blocks())?;
        self.evaluation_context.set_dependencies(dependencies);

        // Evaluate const blocks in dependency order
        let evaluation_order = self
            .dependency_queries
            .compute_topological_order(&self.evaluation_context.get_dependencies())?;

        for block_id in evaluation_order {
            self.evaluate_const_block(block_id, ctx)?;
        }

        // Collect const-eval mutations from intrinsic context
        let intrinsic_ops = self.intrinsic_context.take_const_eval_ops();
        for op in intrinsic_ops {
            self.const_eval_tracker.record(op);
        }

        // Apply accumulated mutations to AST
        let changes_made = self.const_eval_tracker.apply(ast)?;

        // Only attempt inlining when we have const blocks or recorded mutations.
        // This avoids interpreting regular runtime code (e.g., println! with runtime values).
        if changes_made || !self.evaluation_context.get_const_blocks().is_empty() {
            self.inline_constant_expressions(ast, ctx)?;
        }

        debug!("Phase 2 completed: Const evaluation operations applied");
        Ok(())
    }

    /// Phase 3: Final Type Checking
    /// Type check generated code (new fields, methods, types)
    /// Validate all type references including generated ones
    /// Check const block results against expected types
    /// Ensure type system consistency after metaprogramming
    /// Generate final optimized AST
    fn phase3_final_type_checking(&mut self, ast: &Node, ctx: &SharedScopedContext) -> Result<()> {
        debug!("Phase 3: Final Type Checking");

        // Register generated types
        self.type_queries.register_generated_types(ast)?;

        // Validate all type references including generated ones
        self.type_queries.validate_all_references(ast, ctx)?;

        // Validate const block results
        self.evaluation_context
            .validate_results(&self.type_queries)?;

        debug!("Phase 3 completed: Final type checking passed");
        Ok(())
    }

    /// Evaluate a single const block
    /// Delegates to the InterpretationOrchestrator for actual evaluation
    fn evaluate_const_block(&mut self, block_id: u64, ctx: &SharedScopedContext) -> Result<()> {
        let (expr, name) = {
            let const_block = self.evaluation_context.get_const_block(block_id)?;
            (const_block.expr.clone(), const_block.name.clone())
        };

        self.evaluation_context.begin_evaluation(block_id)?;

        // Delegate actual expression evaluation to the interpretation orchestrator
        // The interpretation orchestrator knows how to handle const expressions and
        // intrinsics that may request AST mutations
        let result =
            match self
                .interpreter
                .evaluate_const_expression(&expr, ctx, &self.intrinsic_context)
            {
                Ok(value) => value,
                Err(err) => {
                    self.evaluation_context
                        .set_block_error(block_id, err.to_string())?;
                    return Err(err);
                }
            };

        // Store the result for later queries
        self.evaluation_context
            .set_block_result(block_id, result.clone())?;

        // Expose evaluated consts through the shared context so dependent blocks resolve
        if let Some(name) = name {
            ctx.insert_value_with_ctx(name, result.clone());
        }

        Ok(())
    }

    /// Populate the shared context with structural and alias type bindings so the
    /// interpreter can resolve identifiers like `Config` during const evaluation.
    fn register_type_bindings(&self, node: &Node, ctx: &SharedScopedContext) {
        match node {
            Node::Item(item) => self.register_type_in_item(item, ctx),
            Node::Expr(expr) => self.register_type_in_expr(expr, ctx),
            Node::File(file) => {
                for item in &file.items {
                    self.register_type_in_item(item, ctx);
                }
            }
        }
    }

    fn inline_constant_expressions(
        &self,
        node: &mut Node,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        match node {
            Node::Item(item) => self.inline_constants_in_item(item, ctx)?,
            Node::Expr(expr) => self.inline_constants_in_expr(expr, ctx)?,
            Node::File(file) => {
                for item in &mut file.items {
                    self.inline_constants_in_item(item, ctx)?;
                }
            }
        }
        Ok(())
    }

    fn inline_constants_in_item(&self, item: &mut Item, ctx: &SharedScopedContext) -> Result<()> {
        match item {
            Item::DefFunction(def_fn) => {
                self.inline_constants_in_expr(def_fn.body.as_mut(), ctx)?
            }
            Item::Module(module) => {
                for child in &mut module.items {
                    self.inline_constants_in_item(child, ctx)?;
                }
            }
            Item::Impl(item_impl) => {
                for child in &mut item_impl.items {
                    self.inline_constants_in_item(child, ctx)?;
                }
            }
            Item::Expr(expr) => self.inline_constants_in_expr(expr, ctx)?,
            _ => {}
        }
        Ok(())
    }

    fn inline_constants_in_expr(&self, expr: &mut Expr, ctx: &SharedScopedContext) -> Result<()> {
        use fp_core::ast::Expr as E;

        match expr {
            E::Block(block) => {
                for stmt in &mut block.stmts {
                    match stmt {
                        BlockStmt::Item(item) => {
                            self.inline_constants_in_item(item.as_mut(), ctx)?
                        }
                        BlockStmt::Expr(inner) => {
                            self.inline_constants_in_expr(inner.expr.as_mut(), ctx)?;
                        }
                        BlockStmt::Let(let_stmt) => {
                            if let Some(init) = let_stmt.init.as_mut() {
                                self.inline_constants_in_expr(init, ctx)?;
                            }
                            if let Some(diverge) = let_stmt.diverge.as_mut() {
                                self.inline_constants_in_expr(diverge, ctx)?;
                            }
                        }
                        _ => {}
                    }
                }
            }
            E::If(if_expr) => {
                self.inline_constants_in_expr(if_expr.cond.as_mut(), ctx)?;
                self.inline_constants_in_expr(if_expr.then.as_mut(), ctx)?;
                if let Some(elze) = if_expr.elze.as_mut() {
                    self.inline_constants_in_expr(elze, ctx)?;
                }
            }
            E::Invoke(invoke) => {
                if let ExprInvokeTarget::Expr(target) = &mut invoke.target {
                    self.inline_constants_in_expr(target.as_mut(), ctx)?;
                }
                for arg in &mut invoke.args {
                    self.inline_constants_in_expr(arg, ctx)?;
                }
                if let Some(new_invoke) = self.render_const_format_call(invoke, ctx)? {
                    *expr = Expr::Invoke(new_invoke);
                    return Ok(());
                }

                return Ok(());
            }
            E::StdIoPrintln(println_expr) => {
                for arg in &mut println_expr.format.args {
                    self.inline_constants_in_expr(arg, ctx)?;
                }
                if let Some(rendered) = self.render_const_std_println(println_expr, ctx)? {
                    *expr = Expr::StdIoPrintln(rendered);
                    return Ok(());
                }

                return Ok(());
            }
            E::Select(select) => {
                self.inline_constants_in_expr(select.obj.as_mut(), ctx)?;
            }
            E::Struct(struct_expr) => {
                self.inline_constants_in_expr(struct_expr.name.as_mut(), ctx)?;
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.inline_constants_in_expr(value, ctx)?;
                    }
                }
            }
            E::Tuple(tuple) => {
                for value in &mut tuple.values {
                    self.inline_constants_in_expr(value, ctx)?;
                }
            }
            E::Array(array) => {
                for value in &mut array.values {
                    self.inline_constants_in_expr(value, ctx)?;
                }
            }
            E::Assign(assign) => {
                self.inline_constants_in_expr(assign.target.as_mut(), ctx)?;
                self.inline_constants_in_expr(assign.value.as_mut(), ctx)?;
            }
            E::Let(let_expr) => {
                self.inline_constants_in_expr(let_expr.expr.as_mut(), ctx)?;
            }
            E::BinOp(binop) => {
                self.inline_constants_in_expr(binop.lhs.as_mut(), ctx)?;
                self.inline_constants_in_expr(binop.rhs.as_mut(), ctx)?;
            }
            E::UnOp(unop) => self.inline_constants_in_expr(unop.val.as_mut(), ctx)?,
            E::FormatString(format_str) => {
                for arg in &mut format_str.args {
                    self.inline_constants_in_expr(arg, ctx)?;
                }
            }
            E::Value(_) | E::Locator(_) | E::Any(_) => {}
            _ => {}
        }

        self.try_fold_expr(expr, ctx)?;
        Ok(())
    }

    fn try_fold_expr(&self, expr: &mut Expr, ctx: &SharedScopedContext) -> Result<()> {
        // Avoid folding calls or other effectful expressions
        if matches!(
            expr,
            Expr::Invoke(_) | Expr::Assign(_) | Expr::Loop(_) | Expr::While(_)
        ) {
            return Ok(());
        }

        let cloned = expr.clone();
        match self
            .interpreter
            .evaluate_const_expression(&cloned, ctx, &self.intrinsic_context)
        {
            Ok(val @ (Value::Int(_) | Value::Bool(_) | Value::Decimal(_) | Value::String(_))) => {
                *expr = Expr::value(val);
            }
            Ok(_) => {}
            Err(err) => return Err(err),
        }
        Ok(())
    }

    fn render_const_format_call(
        &self,
        invoke: &ExprInvoke,
        ctx: &SharedScopedContext,
    ) -> Result<Option<ExprInvoke>> {
        // Only handle println!/println
        let target_name = match &invoke.target {
            ExprInvokeTarget::Function(Locator::Ident(ident)) => ident.as_str(),
            ExprInvokeTarget::Function(Locator::Path(path)) if path.segments.len() == 1 => {
                path.segments[0].as_str()
            }
            _ => return Ok(None),
        };

        if target_name != "println" && target_name != "println!" {
            return Ok(None);
        }

        let format_expr = &invoke.args[0];
        if !matches!(format_expr, Expr::FormatString(_)) {
            return Ok(None);
        }

        if let Expr::FormatString(fmt) = format_expr {
            if !fmt.args.is_empty() || !fmt.kwargs.is_empty() {
                return Ok(None);
            }
        }

        let evaluated = self
            .interpreter
            .interpret_expr(format_expr, ctx)
            .and_then(|value| match value {
                Value::String(s) => Ok(s.value),
                _ => Err(optimization_error("format string did not produce string")),
            });

        if let Ok(rendered) = evaluated {
            // Replace invoke args with single literal string
            let new_args = vec![Expr::value(Value::string(rendered))];
            let mut new_invoke = invoke.clone();
            new_invoke.args = new_args;
            return Ok(Some(new_invoke));
        }

        Ok(None)
    }

    fn render_const_std_println(
        &self,
        println_expr: &ExprStdIoPrintln,
        ctx: &SharedScopedContext,
    ) -> Result<Option<ExprStdIoPrintln>> {
        if !println_expr.format.args.is_empty() || !println_expr.format.kwargs.is_empty() {
            return Ok(None);
        }

        let format_expr = Expr::FormatString(println_expr.format.clone());
        let evaluated = self
            .interpreter
            .interpret_expr(&format_expr, ctx)
            .and_then(|value| match value {
                Value::String(s) => Ok(s.value),
                _ => Err(optimization_error("format string did not produce string")),
            });

        if let Ok(rendered) = evaluated {
            let simplified = ExprStdIoPrintln {
                format: ExprFormatString {
                    parts: vec![FormatTemplatePart::Literal(rendered)],
                    args: Vec::new(),
                    kwargs: Vec::new(),
                },
                newline: println_expr.newline,
            };
            return Ok(Some(simplified));
        }

        Ok(None)
    }

    fn register_type_in_item(&self, item: &Item, ctx: &SharedScopedContext) {
        match item {
            Item::DefStruct(def_struct) => {
                ctx.insert_value_with_ctx(
                    def_struct.name.clone(),
                    Value::Type(Ty::Struct(def_struct.value.clone())),
                );
            }
            Item::DefType(def_type) => {
                ctx.insert_value_with_ctx(
                    def_type.name.clone(),
                    Value::Type(def_type.value.clone()),
                );
            }
            Item::Module(module) => {
                for child in &module.items {
                    self.register_type_in_item(child, ctx);
                }
            }
            Item::Impl(item_impl) => {
                if let Expr::Locator(Locator::Ident(type_ident)) = &item_impl.self_ty {
                    if let Some(type_value) = ctx.get_value(type_ident.clone()) {
                        ctx.insert_value_with_ctx(Ident::new("Self"), type_value);
                    }
                    for child in &item_impl.items {
                        if let Item::DefFunction(func_def) = child {
                            let method_key = Ident::new(&format!(
                                "{}::{}",
                                type_ident.as_str(),
                                func_def.name.as_str()
                            ));
                            ctx.insert_value_with_ctx(method_key, Value::Function(func_def._to_value()));
                        }
                    }
                }
            }
            Item::DefFunction(def_fn) => {
                ctx.insert_value_with_ctx(def_fn.name.clone(), Value::Function(def_fn._to_value()));
            }
            Item::Expr(expr) => self.register_type_in_expr(expr, ctx),
            _ => {}
        }
    }

    fn register_type_in_expr(&self, expr: &Expr, ctx: &SharedScopedContext) {
        match expr {
            Expr::Block(block) => {
                for stmt in &block.stmts {
                    match stmt {
                        BlockStmt::Item(item) => self.register_type_in_item(item.as_ref(), ctx),
                        BlockStmt::Let(let_stmt) => {
                            if let Some(init) = &let_stmt.init {
                                self.register_type_in_expr(init, ctx);
                            }
                            if let Some(diverge) = &let_stmt.diverge {
                                self.register_type_in_expr(diverge, ctx);
                            }
                        }
                        BlockStmt::Expr(inner) => {
                            self.register_type_in_expr(inner.expr.as_ref(), ctx);
                        }
                        _ => {}
                    }
                }
            }
            Expr::If(if_expr) => {
                self.register_type_in_expr(&if_expr.cond, ctx);
                self.register_type_in_expr(&if_expr.then, ctx);
                if let Some(elze) = &if_expr.elze {
                    self.register_type_in_expr(elze, ctx);
                }
            }
            Expr::Invoke(invoke) => {
                for arg in &invoke.args {
                    self.register_type_in_expr(arg, ctx);
                }
                if let ExprInvokeTarget::Expr(target) = &invoke.target {
                    self.register_type_in_expr(target.as_ref(), ctx);
                }
            }
            Expr::Let(let_expr) => {
                self.register_type_in_expr(let_expr.expr.as_ref(), ctx);
            }
            Expr::Assign(assign) => {
                self.register_type_in_expr(assign.target.as_ref(), ctx);
                self.register_type_in_expr(assign.value.as_ref(), ctx);
            }
            Expr::Struct(struct_expr) => {
                self.register_type_in_expr(struct_expr.name.as_ref(), ctx);
                for field in &struct_expr.fields {
                    if let Some(value) = &field.value {
                        self.register_type_in_expr(value, ctx);
                    }
                }
            }
            Expr::Tuple(tuple) => {
                for value in &tuple.values {
                    self.register_type_in_expr(value, ctx);
                }
            }
            Expr::Array(array) => {
                for value in &array.values {
                    self.register_type_in_expr(value, ctx);
                }
            }
            Expr::Value(value) => self.register_type_in_value(value.as_ref(), ctx),
            Expr::Item(item) => self.register_type_in_item(item, ctx),
            _ => {}
        }
    }

    fn register_type_in_value(&self, value: &Value, ctx: &SharedScopedContext) {
        match value {
            Value::Expr(expr) => self.register_type_in_expr(expr, ctx),
            Value::Struct(value_struct) => {
                for field in &value_struct.structural.fields {
                    self.register_type_in_value(&field.value, ctx);
                }
            }
            Value::Structural(value_structural) => {
                for field in &value_structural.fields {
                    self.register_type_in_value(&field.value, ctx);
                }
            }
            Value::Tuple(tuple) => {
                for value in &tuple.values {
                    self.register_type_in_value(value, ctx);
                }
            }
            Value::Type(ty) => {
                // Ensure nested types are also registered
                self.register_nested_type(ty, ctx);
            }
            _ => {}
        }
    }

    fn register_nested_type(&self, ty: &Ty, ctx: &SharedScopedContext) {
        match ty {
            Ty::Struct(struct_ty) => {
                ctx.insert_value_with_ctx(
                    struct_ty.name.clone(),
                    Value::Type(Ty::Struct(struct_ty.clone())),
                );
                for field in &struct_ty.fields {
                    self.register_nested_type(&field.value, ctx);
                }
            }
            Ty::Tuple(tuple) => {
                for ty in &tuple.types {
                    self.register_nested_type(ty, ctx);
                }
            }
            Ty::Vec(vec_ty) => self.register_nested_type(&vec_ty.ty, ctx),
            Ty::Reference(reference) => self.register_nested_type(&reference.ty, ctx),
            Ty::Expr(expr) => self.register_type_in_expr(expr, ctx),
            _ => {}
        }
    }

    /// Get the results of const evaluation
    pub fn get_results(&self) -> std::collections::HashMap<String, Value> {
        self.evaluation_context.get_all_results()
    }

    /// Get accumulated const-eval operations without consuming them
    pub fn get_const_eval_ops(&self) -> Vec<ConstEval> {
        self.const_eval_tracker.pending()
    }

    /// Manually queue a const-eval operation (mainly used by tests harnesses)
    pub fn record_const_eval(&mut self, op: ConstEval) {
        self.const_eval_tracker.record(op);
    }

    /// Drop any queued const-eval operations without applying them
    pub fn clear_const_eval_ops(&mut self) {
        self.const_eval_tracker.clear();
    }

    /// Apply queued const-eval operations to a module in-place
    pub fn apply_const_eval_ops_to_module(&mut self, module: &mut Module) -> Result<bool> {
        let mut node = Node::Item(Item::Module(module.clone()));
        let changed = self.const_eval_tracker.apply(&mut node)?;
        if changed {
            match node {
                Node::Item(Item::Module(updated)) => {
                    *module = updated;
                }
                _ => unreachable!("const-eval tracker should yield a module node"),
            }
        }
        Ok(changed)
    }

    /// Evaluate only const items (const declarations, structs) in an AST expression,
    /// leaving function call expressions for runtime execution
    pub fn evaluate_const_items_only(
        &mut self,
        ast: &mut Expr,
        context: &SharedScopedContext,
    ) -> Result<()> {
        use fp_core::ast::{BlockStmt, Expr};

        // Process items and format string expressions
        match ast {
            Expr::Block(block) => {
                // Process Item statements for const evaluation
                for stmt in block.stmts.iter_mut() {
                    match stmt {
                        BlockStmt::Item(item_box) => {
                            // Evaluate const items - dereference the Box to get the Item
                            let mut item_node = Node::Item((**item_box).clone());
                            self.evaluate(&mut item_node, context)?;

                            // Update the item in the AST with the evaluated result
                            if let Node::Item(evaluated_item) = item_node {
                                **item_box = evaluated_item;
                            }
                        }
                        BlockStmt::Expr(expr_stmt) => {
                            // Process expression statements that contain format strings
                            self.evaluate_expr_const_parts(&mut expr_stmt.expr, context)?;
                        }
                        _ => {
                            // For other statement types, recursively process if they contain expressions
                        }
                    }
                }
            }
            _ => {
                // For non-block expressions, skip const evaluation completely
                // This preserves function calls and other runtime expressions
            }
        }

        Ok(())
    }

    /// Recursively evaluate const parts within expressions (like format strings)
    fn evaluate_expr_const_parts(
        &mut self,
        expr: &mut BExpr,
        context: &SharedScopedContext,
    ) -> Result<()> {
        use fp_core::ast::Expr;

        match &mut **expr {
            Expr::FormatString(_) => {
                // Format strings are now resolved during AST→HIR transformation
                // No const evaluation needed here
                debug!("Format string found - will be resolved during HIR transformation");
            }
            Expr::Invoke(invoke) => {
                // Recursively process arguments that might contain format strings
                for arg in invoke.args.iter_mut() {
                    let mut boxed_arg = Box::new(arg.clone());
                    self.evaluate_expr_const_parts(&mut boxed_arg, context)?;
                    *arg = *boxed_arg;
                }
            }
            Expr::Block(block) => {
                // Recursively process nested blocks
                for stmt in block.stmts.iter_mut() {
                    if let BlockStmt::Expr(expr_stmt) = stmt {
                        self.evaluate_expr_const_parts(&mut expr_stmt.expr, context)?;
                    }
                }
            }
            _ => {
                // For other expression types, we don't need to evaluate them as const
                // since we're focusing on format strings
            }
        }

        Ok(())
    }
}
