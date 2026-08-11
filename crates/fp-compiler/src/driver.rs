use fp_backend::transformations::{HirGenerator, HirLoweringConfig, LirGenerator, MirLowering};
use fp_core::ast::{Ty, TypeStruct, TypeType, Value};
use fp_core::hir;
use fp_core::mir;
use fp_core::mir::ty::{FloatTy, IntTy, TyKind, UintTy};
use fp_core::module::path::QualifiedPath;
use fp_core::package::PackageId;
use fp_core::span::Span;
use fp_interpret::LirInterpreter;
use fp_lang::FerroIntrinsicNormalizer;
use fp_typing::{HirTypeChecker, TypingContext};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::future::Future;
use std::task::{Context, Poll, Waker};
use std::rc::Rc;

use crate::{
    CompilerDriverError, CompilerState, ConstValueId, ExecutorHandle, FullyQualifiedPath, HirId,
    LirId, MirId, RuntimeValueId,
};

pub struct CompilerDriver {
    pub state: CompilerState,
    interpreter: LirInterpreter,
    building_packages: HashSet<PackageId>,
    compiled_packages: HashMap<PackageId, Rc<RefCell<fp_core::package::CompiledPackage>>>,
    next_hir_def_id: u32,
    pub pipeline: PipelineMode,
}

/// Controls how far the compiler pipeline runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PipelineMode {
    /// Full native compilation: AST → HIR → MIR → LIR
    Native,
    /// Stop after parsing: resolve modules, parse sources, return AST items
    NominalTranspile,
    /// HIR typing + lift back to AST: AST → HIR → typing → AST
    TypecheckedTranspile,
}

impl CompilerDriver {
    fn module_state_key(&self, path: &QualifiedPath) -> String {
        let package_id = self
            .state
            .typing_ctx
            .env_ctx
            .current_package()
            .map(|package_id| package_id.as_str().to_string());
        match package_id {
            Some(package_id) => format!("{package_id}:{}", path.to_key()),
            None => path.to_key(),
        }
    }

    pub fn new(data_layout: fp_core::lir::LirDataLayout, tasks: ExecutorHandle) -> Self {
        Self::with_workspace(
            data_layout,
            tasks,
            Rc::new(fp_core::workspace::WorkspaceContext::new()),
        )
    }

    pub fn with_workspace(
        data_layout: fp_core::lir::LirDataLayout,
        tasks: ExecutorHandle,
        workspace: Rc<fp_core::workspace::WorkspaceContext>,
    ) -> Self {
        let mut state = CompilerState::new(data_layout.clone(), tasks);
        state.typing_ctx = Rc::new(TypingContext::new(data_layout, workspace));
        Self {
            state,
            interpreter: LirInterpreter::new(),
            building_packages: HashSet::new(),
            compiled_packages: HashMap::new(),
            next_hir_def_id: 0,
            pipeline: PipelineMode::Native,
        }
    }

    pub fn with_state(state: CompilerState) -> Self {
        Self {
            state,
            interpreter: LirInterpreter::new(),
            building_packages: HashSet::new(),
            compiled_packages: HashMap::new(),
            next_hir_def_id: 0,
            pipeline: PipelineMode::Native,
        }
    }

    pub async fn compile_native(
        &mut self,
        package_id: &PackageId,
    ) -> Result<Rc<RefCell<fp_core::package::CompiledPackage>>, CompilerDriverError> {
        self.compile_package(package_id).await
    }

    pub async fn compile_bytecode(
        &mut self,
        package_id: &PackageId,
    ) -> Result<fp_bytecode::BytecodeProgram, CompilerDriverError> {
        let package = self.compile_package(package_id).await?;
        let mir = package.borrow().mir_program.clone().ok_or_else(|| {
            CompilerDriverError::InternalCompilerError(format!(
                "package {package_id} has no MIR program"
            ))
        })?;
        fp_bytecode::lower_program(&mir).map_err(CompilerDriverError::from)
    }

    pub fn compile_native_sync(
        &mut self,
        package_id: &PackageId,
    ) -> Result<Rc<RefCell<fp_core::package::CompiledPackage>>, CompilerDriverError> {
        let executor = self.state.tasks.clone();
        executor.run(self.compile_native(package_id))
    }

    pub fn compile_bytecode_sync(
        &mut self,
        package_id: &PackageId,
    ) -> Result<fp_bytecode::BytecodeProgram, CompilerDriverError> {
        let executor = self.state.tasks.clone();
        executor.run(self.compile_bytecode(package_id))
    }

    /// Focus subsequent module work on an already compiled package. The
    /// package itself and its imported dependencies remain shared through
    /// `Rc`; only the lookup context becomes package-local.
    pub fn focus_package(&mut self, package_id: PackageId) -> Result<(), CompilerDriverError> {
        let parent_context = self.state.typing_ctx.clone();
        let package_workspace = parent_context.env_ctx.for_package(package_id.clone());
        for (dependency_id, package) in parent_context.env_ctx.crates().iter() {
            if dependency_id != &package_id {
                package_workspace.import_package(dependency_id.clone(), package.clone());
            }
        }
        if let Some(std_package) = package_workspace.compiled_package(&PackageId::new("std")) {
            package_workspace.install_prelude(std_package);
        }
        let package = parent_context
            .env_ctx
            .compiled_package(&package_id)
            .ok_or_else(|| CompilerDriverError::UnresolvablePackage(package_id.to_string()))?;
        package_workspace.import_package(package_id, package);
        self.state.typing_ctx = Rc::new(TypingContext::new(
            parent_context.data_layout.clone(),
            Rc::new(package_workspace),
        ));
        Ok(())
    }

    pub fn execute_runtime(
        &mut self,
        lir_id: &LirId,
    ) -> Result<fp_core::ast::Value, CompilerDriverError> {
        let lir = self.state.lir(lir_id)?.clone();
        self.interpreter = LirInterpreter::new();
        let resolved = self.collect_resolved_const_values();
        self.interpreter
            .inject_globals(&resolved)
            .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        let entrypoint = self.state.runtime_entrypoint(lir_id)?;
        let value = self.interpreter.run_entrypoint(&lir, entrypoint)?;
        let value_id = RuntimeValueId::new(format!("runtime_value:{}", lir_id.as_str()));
        self.state.insert_runtime_value(value_id, value.clone());
        Ok(value)
    }

    pub fn select_entrypoint(
        &mut self,
        package_id: &PackageId,
        module_path: &QualifiedPath,
        function_name: &str,
    ) -> Result<LirId, CompilerDriverError> {
        let package = self
            .state
            .typing_ctx
            .env_ctx
            .compiled_package(package_id)
            .ok_or_else(|| CompilerDriverError::UnresolvablePackage(package_id.to_string()))?;
        let function = package
            .borrow()
            .hir_program
            .as_ref()
            .and_then(|program| {
                program.items.iter().find_map(|item| match &item.kind {
                    hir::ItemKind::Function(function)
                        if function.sig.name.as_str() == function_name =>
                    {
                        Some(item.def_id)
                    }
                    _ => None,
                })
            })
            .ok_or_else(|| {
                CompilerDriverError::Interpreter(format!(
                    "package `{package_id}` module `{}` has no `{function_name}` entrypoint",
                    module_path.to_key()
                ))
            })?;
        let lir_id = Self::package_module_lir_id(package_id, module_path);
        let lir = package.borrow().lir_workspace.to_program();
        self.state.insert_lir(lir_id.clone(), lir);
        self.state.lir(&lir_id)?;
        self.state
            .insert_runtime_entrypoint(lir_id.clone(), function);
        Ok(lir_id)
    }

    pub async fn compile_package_module_native(
        &mut self,
        package_id: &PackageId,
        module_path: &QualifiedPath,
        function_name: &str,
    ) -> Result<(), CompilerDriverError> {
        let lir_id = self.select_entrypoint(package_id, module_path, function_name)?;
        self.evaluate_comptime_lir(&lir_id, &FullyQualifiedPath::new(module_path.clone()))
            .await?;
        Ok(())
    }

    /// Compile a package after recursively compiling its declared
    /// dependencies. Dependency resolution and version selection happen in
    /// the provider; the driver only consumes the concrete package IDs it is
    /// given by metadata.
    pub async fn compile_package(
        &mut self,
        package_id: &PackageId,
    ) -> Result<Rc<RefCell<fp_core::package::CompiledPackage>>, CompilerDriverError> {
        let parent_context = self.state.typing_ctx.clone();
        if let Some(package) = self.compiled_packages.get(package_id).cloned() {
            parent_context
                .env_ctx
                .import_package(package_id.clone(), package.clone());
            return Ok(package);
        }
        if let Some(package) = parent_context.env_ctx.compiled_package(package_id) {
            return Ok(package);
        }
        if !self.building_packages.insert(package_id.clone()) {
            return Err(CompilerDriverError::UnresolvablePackage(format!(
                "dependency cycle involving {package_id}"
            )));
        }

        let std_package = if matches!(package_id.as_str(), "std" | "libc") {
            None
        } else {
            match Box::pin(self.compile_package(&PackageId::new("std"))).await {
                Ok(package) => Some(package),
                Err(error) => {
                    self.building_packages.remove(package_id);
                    return Err(error);
                }
            }
        };

        let package_workspace = parent_context.env_ctx.for_package(package_id.clone());
        if let Some(std_package) = std_package {
            package_workspace.import_package(PackageId::new("std"), std_package);
            if let Some(std_package) = package_workspace.compiled_package(&PackageId::new("std")) {
                package_workspace.install_prelude(std_package);
            }
        }
        self.state.typing_ctx = Rc::new(TypingContext::new(
            parent_context.data_layout.clone(),
            Rc::new(package_workspace),
        ));

        let result: Result<Rc<RefCell<fp_core::package::CompiledPackage>>, CompilerDriverError> =
            async {
                let provider = self
                    .state
                    .typing_ctx
                    .env_ctx
                    .provider_for(package_id)
                    .ok_or_else(|| {
                        CompilerDriverError::UnresolvablePackage(package_id.to_string())
                    })?;
                let metadata = provider
                    .load_package_metadata(package_id)
                    .map_err(|error| {
                        CompilerDriverError::UnresolvablePackage(format!("{package_id}: {error}"))
                    })?;
                if metadata.id != *package_id {
                    return Err(CompilerDriverError::UnresolvablePackage(format!(
                        "provider returned metadata for {}, requested {package_id}",
                        metadata.id
                    )));
                }

                for dependency in &metadata.metadata.dependencies {
                    let dependency_id =
                        dependency.resolved_package_id.clone().ok_or_else(|| {
                            CompilerDriverError::UnresolvablePackage(format!(
                            "dependency `{}` of package `{package_id}` has no selected package ID",
                            dependency.package
                        ))
                        })?;
                    let dependency_package = Box::pin(self.compile_package(&dependency_id)).await?;
                    if dependency_id.as_str() == "std" {
                        self.state
                            .typing_ctx
                            .env_ctx
                            .install_prelude(dependency_package);
                    }
                }

                let source = provider.load_package_source(package_id).map_err(|error| {
                    CompilerDriverError::UnresolvablePackage(format!("{package_id}: {error}"))
                })?;
                if source.package_id != *package_id {
                    return Err(CompilerDriverError::UnresolvablePackage(format!(
                        "provider returned source for {}, requested {package_id}",
                        source.package_id
                    )));
                }
                let package = self.state.typing_ctx.env_ctx.begin_package(
                    package_id.clone(),
                    source,
                    self.state.typing_ctx.data_layout.clone(),
                );
                if self.pipeline == PipelineMode::Native {
                    let units = self.compile_items_to_lir_units(&package).await?;
                    Self::publish_lir_units(&package, package_id, &units)?;
                }
                Ok(package)
            }
            .await;

        self.building_packages.remove(package_id);
        self.state.typing_ctx = parent_context.clone();
        let package = result?;
        self.compiled_packages
            .insert(package_id.clone(), package.clone());
        parent_context
            .env_ctx
            .import_package(package_id.clone(), package.clone());
        Ok(package)
    }

    fn publish_lir_units(
        package: &Rc<RefCell<fp_core::package::CompiledPackage>>,
        package_id: &PackageId,
        units: &[fp_core::lir::LirCompileUnit],
    ) -> Result<(), CompilerDriverError> {
        let layout = package.borrow().lir_workspace.data_layout.clone();
        let mut workspace = fp_core::lir::LirWorkspace::new(layout);
        for unit in units {
            workspace
                .add_program(
                    package_id.clone(),
                    unit.module_path.clone(),
                    unit.program.clone(),
                )
                .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        }
        let mut package = package.borrow_mut();
        package.lir_units = units.to_vec();
        package.lir_workspace = workspace;
        Ok(())
    }

    async fn compile_items_to_lir_units(
        &mut self,
        package: &Rc<RefCell<fp_core::package::CompiledPackage>>,
    ) -> Result<Vec<fp_core::lir::LirCompileUnit>, CompilerDriverError> {
        let hir_package_id = self
            .state
            .typing_ctx
            .env_ctx
            .current_package()
            .and_then(|package_id| {
                self.state
                    .typing_ctx
                    .env_ctx
                    .compiled_package(package_id)
                    .map(|package| package.borrow().package_id)
            })
            .unwrap_or_default();
        let mut generator = HirGenerator::new()
            .with_intrinsic_normalizer(FerroIntrinsicNormalizer::new(
                fp_core::intrinsics::IntrinsicNormalizationMode::Compile,
            ))
            .with_package_id(hir_package_id)
            .with_def_id_start(self.next_hir_def_id)
            .with_lowering_config(HirLoweringConfig)
            .with_workspace(self.state.typing_ctx.env_ctx.clone());
        let package_source = package.borrow().clone();
        let hir_program = generator.transform_package(&package_source)?;
        self.next_hir_def_id = self.next_hir_def_id.max(generator.next_def_id_value());
        let package_exports = generator.exported_symbols();
        if let Some(package_id) = self.state.typing_ctx.env_ctx.current_package().cloned() {
            if let Some(package) = self.state.typing_ctx.env_ctx.compiled_package(&package_id) {
                package.borrow_mut().hir_exports.extend(package_exports);
            }
        }
        let (hir_program, typeck_results) = self
            .type_check_program(hir_program)
            .await
            .map_err(|error| {
                CompilerDriverError::InternalCompilerError(format!(
                    "package HIR type checking failed: {error}"
                ))
            })?;
        let current_package_id = self
            .state
            .typing_ctx
            .env_ctx
            .current_package()
            .cloned()
            .ok_or_else(|| {
                CompilerDriverError::UnresolvablePackage(
                    "package compilation requires a focused package workspace".to_string(),
                )
            })?;
        let package_path = QualifiedPath::new(Vec::new());
        let hir_id = HirId::new(format!("hir:{}", self.module_state_key(&package_path)));
        self.state.insert_hir(hir_id.clone(), hir_program);
        self.state.insert_hir_typeck(hir_id.clone(), typeck_results);
        if let Some(package) = self
            .state
            .typing_ctx
            .env_ctx
            .compiled_package(&current_package_id)
        {
            package.borrow_mut().hir_program = Some(self.state.hir(&hir_id)?.clone());
        }

        // TypecheckedTranspile: lift typed HIR back to AST, skip MIR/LIR
        if self.pipeline == PipelineMode::TypecheckedTranspile {
            let hir = self.state.hir(&hir_id)?;
            let lifted = fp_backend::transforms::lift_program(hir, std::path::PathBuf::new())
                .map_err(|e| {
                    CompilerDriverError::InternalCompilerError(format!("HIR→AST lift: {e}"))
                })?;
            if let Some(pkg) = self
                .state
                .typing_ctx
                .env_ctx
                .compiled_package(&current_package_id)
            {
                pkg.borrow_mut().lifted_ast = Some(lifted);
            }
            return Ok(Vec::new()); // No LIR units needed
        }

        let fqp = FullyQualifiedPath::new(package_path.clone());
        let (mir_id, struct_layouts, full_layouts, adt_defs) =
            self.lower_to_mir(&hir_id, &fqp).await?;
        if let Some(package) = self
            .state
            .typing_ctx
            .env_ctx
            .compiled_package(&current_package_id)
        {
            package.borrow_mut().mir_program = Some(self.state.mir(&mir_id)?.clone());
            package
                .borrow_mut()
                .mir_struct_fields
                .extend(struct_layouts);
            package.borrow_mut().mir_adt_defs.extend(adt_defs.clone());
        }
        let mut all_adt_defs = HashMap::new();
        for (_dep_id, dep_package) in self.state.typing_ctx.env_ctx.crates().iter() {
            all_adt_defs.extend(dep_package.borrow().mir_adt_defs.clone());
        }
        let lir_id = self.lower_to_lir(
            &mir_id,
            &fqp,
            &current_package_id,
            &full_layouts,
            &all_adt_defs,
        )?;

        let lir = self.state.lir(&lir_id)?.clone();
        Ok(vec![fp_core::lir::LirCompileUnit {
            package_id: hir_package_id,
            module_path: package_path,
            program: lir,
        }])
    }

    async fn type_check_program(
        &mut self,
        program: hir::Program,
    ) -> fp_core::Result<(hir::Program, fp_typing::TypeckResults)> {
        let mut future = Box::pin(
            HirTypeChecker::new(program)
                .with_context(self.state.typing_ctx.clone())
                .check(),
        );
        loop {
            let poll = {
                let waker = Waker::noop();
                let mut cx = Context::from_waker(waker);
                future.as_mut().poll(&mut cx)
            };
            match poll {
                Poll::Ready(result) => return result,
                Poll::Pending => {
                    let requests = self.state.typing_ctx.take_comptime_requests();
                    if requests.is_empty() {
                        return Err(fp_core::error::Error::from(
                            "HIR type checking suspended without a comptime request",
                        ));
                    }
                    for request in requests {
                        let result = self.evaluate_hir_const_request(request.request());
                        request.complete(result);
                    }
                }
            }
        }
    }

    fn evaluate_hir_const_request(
        &self,
        request: &fp_typing::ComptimeRequest,
    ) -> fp_core::Result<Value> {
        let expr = request
            .block
            .expr
            .as_deref()
            .ok_or_else(|| fp_core::error::Error::from("const block has no value"))?;
        Self::evaluate_hir_const_expr(&request.program, expr)
    }

    fn evaluate_hir_const_expr(
        program: &hir::Program,
        expr: &hir::Expr,
    ) -> fp_core::Result<Value> {
        match &expr.kind {
            hir::ExprKind::Literal(literal) => Ok(match literal {
                hir::Lit::Bool(value) => Value::bool(*value),
                hir::Lit::Integer(value) => Value::int(*value),
                hir::Lit::Float(value) => Value::decimal(*value),
                hir::Lit::Str(value) => Value::string(value.clone()),
                hir::Lit::Char(value) => Value::string(value.to_string()),
                hir::Lit::Null => Value::null(),
            }),
            hir::ExprKind::Path(path) => {
                let name = path
                    .segments
                    .last()
                    .map(|segment| segment.name.as_str())
                    .ok_or_else(|| fp_core::error::Error::from("empty const path"))?;
                let item = program.def_map.values().find(|item| match &item.kind {
                    hir::ItemKind::Const(constant) => constant.name.as_str().ends_with(name),
                    _ => false,
                });
                let Some(item) = item else {
                    return Err(fp_core::error::Error::from(format!(
                        "const path `{name}` is not available during comptime evaluation"
                    )));
                };
                let hir::ItemKind::Const(constant) = &item.kind else {
                    unreachable!();
                };
                Self::evaluate_hir_const_expr(program, &constant.body.value)
            }
            hir::ExprKind::Binary(op, lhs, rhs) => {
                let lhs = Self::evaluate_hir_const_expr(program, lhs)?;
                let rhs = Self::evaluate_hir_const_expr(program, rhs)?;
                Self::evaluate_hir_binary(op, lhs, rhs)
            }
            hir::ExprKind::If(condition, then_expr, else_expr) => {
                let condition = Self::evaluate_hir_const_expr(program, condition)?;
                let Value::Bool(condition) = condition else {
                    return Err(fp_core::error::Error::from(
                        "const-if condition must evaluate to bool",
                    ));
                };
                if condition.value {
                    Self::evaluate_hir_const_expr(program, then_expr)
                } else if let Some(else_expr) = else_expr {
                    Self::evaluate_hir_const_expr(program, else_expr)
                } else {
                    Ok(Value::unit())
                }
            }
            hir::ExprKind::Block(block) => block
                .expr
                .as_deref()
                .map(|expr| Self::evaluate_hir_const_expr(program, expr))
                .unwrap_or_else(|| Ok(Value::unit())),
            _ => Err(fp_core::error::Error::from(
                "const expression is not supported by the type-check-time evaluator",
            )),
        }
    }

    fn evaluate_hir_binary(
        op: &hir::BinOp,
        lhs: Value,
        rhs: Value,
    ) -> fp_core::Result<Value> {
        let lhs_int = || match lhs.clone() {
            Value::Int(value) => Ok(value.value),
            Value::UInt(value) => i64::try_from(value.value)
                .map_err(|_| fp_core::error::Error::from("integer overflow during comptime")),
            _ => Err(fp_core::error::Error::from("integer operands required")),
        };
        let rhs_int = || match rhs.clone() {
            Value::Int(value) => Ok(value.value),
            Value::UInt(value) => i64::try_from(value.value)
                .map_err(|_| fp_core::error::Error::from("integer overflow during comptime")),
            _ => Err(fp_core::error::Error::from("integer operands required")),
        };
        match op {
            hir::BinOp::Add => Ok(Value::int(lhs_int()? + rhs_int()?)),
            hir::BinOp::Sub => Ok(Value::int(lhs_int()? - rhs_int()?)),
            hir::BinOp::Mul => Ok(Value::int(lhs_int()? * rhs_int()?)),
            hir::BinOp::Div => Ok(Value::int(lhs_int()? / rhs_int()?)),
            hir::BinOp::Rem => Ok(Value::int(lhs_int()? % rhs_int()?)),
            hir::BinOp::Eq => Ok(Value::bool(lhs == rhs)),
            hir::BinOp::Ne => Ok(Value::bool(lhs != rhs)),
            hir::BinOp::Gt | hir::BinOp::Ge | hir::BinOp::Lt | hir::BinOp::Le => {
                let lhs = lhs_int()?;
                let rhs = rhs_int()?;
                Ok(Value::bool(match op {
                    hir::BinOp::Gt => lhs > rhs,
                    hir::BinOp::Ge => lhs >= rhs,
                    hir::BinOp::Lt => lhs < rhs,
                    hir::BinOp::Le => lhs <= rhs,
                    _ => unreachable!(),
                }))
            }
            _ => Err(fp_core::error::Error::from(
                "binary operator is not supported during comptime",
            )),
        }
    }

    async fn evaluate_comptime_lir(
        &mut self,
        lir_id: &LirId,
        path: &FullyQualifiedPath,
    ) -> Result<usize, CompilerDriverError> {
        let lir = self.state.lir(lir_id)?.clone();
        // Only `comptime_entries` is needed after `lir` itself is moved into
        // `all_units` below — clone just that (much smaller than the whole
        // program) rather than cloning the entire `LirProgram` a second time.
        let comptime_entries = lir.comptime_entries.clone();

        let package_id = self
            .state
            .typing_ctx
            .env_ctx
            .current_package()
            .cloned()
            .ok_or_else(|| {
                CompilerDriverError::UnresolvablePackage(
                    "comptime evaluation requires a focused package workspace".to_string(),
                )
            })?;
        let value_id = ConstValueId::new(format!("const_value:{}", path.to_key()));
        if comptime_entries.is_empty() {
            self.state
                .insert_const_value(value_id.clone(), Value::unit());
            return Ok(0);
        }
        let mut execution_workspace = fp_core::lir::LirWorkspace::new(lir.data_layout.clone());
        for (_dependency_id, package) in self.state.typing_ctx.env_ctx.crates().iter() {
            execution_workspace
                .add_workspace(&package.borrow().lir_workspace)
                .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        }
        if execution_workspace
            .find_function(package_id.clone(), &comptime_entries[0].function)
            .is_none()
        {
            execution_workspace
                .add_program(package_id.clone(), path.path().clone(), lir)
                .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        }

        let mut count = 0usize;
        let mut last = Value::unit();
        self.interpreter = LirInterpreter::new();
        let resolved = self.collect_resolved_const_values();
        self.interpreter
            .inject_globals(&resolved)
            .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        for entry in &comptime_entries {
            let result = self.interpreter.run_function_named_in_workspace(
                &execution_workspace,
                &package_id,
                &entry.function,
            );
            let mut value =
                result.map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
            let entry_lir_ty = execution_workspace
                .find_function(package_id.clone(), &entry.function)
                .map(|function| function.signature.return_type.clone())
                .ok_or_else(|| {
                    CompilerDriverError::UnsupportedWork(format!(
                        "missing LIR result type for comptime entry {}",
                        entry.function
                    ))
                })?;
            value = self
                .interpreter
                .read_typed_const_value(value, &entry_lir_ty)
                .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
            // Store struct types from ALL entries, not just the last one.
            // Each const-block type alias produces its own struct.
            let entry_struct = Self::extract_struct_type(&value);
            if let Some(ref struct_ty) = entry_struct {
                let name = struct_ty.name.as_str().to_string();
                self.state
                    .typing_ctx
                    .resolved_types
                    .borrow_mut()
                    .insert(name.clone(), struct_ty.clone());
                self.state.typing_ctx.wake_comptime(&name);
            }
            let constant = self
                .value_to_mir_constant(&value, &entry.ty)
                .ok_or_else(|| {
                    CompilerDriverError::UnsupportedWork(format!(
                        "unsupported comptime result for {}",
                        entry.key
                    ))
                })?;
            self.state
                .insert_resolved_const_value(entry.key.clone(), constant);
            self.state
                .insert_typing_const(entry.key.clone(), value.clone());
            last = value;
            count += 1;
        }

        // Store the final value for const evaluation purposes

        self.state.insert_const_value(value_id.clone(), last);
        Ok(count)
    }

    fn extract_struct_type(value: &Value) -> Option<TypeStruct> {
        match value {
            Value::Type(Ty::Struct(s)) => Some(s.clone()),
            Value::Type(Ty::Type(TypeType {
                inner: Some(inner), ..
            })) => match inner.as_ref() {
                Ty::Struct(s) => Some(s.clone()),
                _ => None,
            },
            _ => None,
        }
    }

    async fn lower_to_mir(
        &mut self,
        hir_id: &HirId,
        path: &FullyQualifiedPath,
    ) -> Result<
        (
            MirId,
            HashMap<fp_core::mir::DefId, Vec<fp_core::mir::Ty>>,
            HashMap<(fp_core::mir::DefId, Vec<fp_core::mir::Ty>), Vec<fp_core::mir::Ty>>,
            HashMap<fp_core::hir::DefId, fp_core::mir::ty::AdtDef>,
        ),
        CompilerDriverError,
    > {
        // HIR has already passed type checking at this boundary. Lowering is
        // therefore strict: a failure is an internal compiler error, never a
        // recoverable source diagnostic.
        let mut hir = self.state.hir(hir_id)?.clone();
        let typeck_results = self.state.hir_typeck(hir_id)?.clone();
        let mut lowering = MirLowering::new()
            .with_typeck_results(&typeck_results)
            .map_err(|error| {
                CompilerDriverError::InternalCompilerError(format!(
                    "HIR-to-MIR setup failed for {}: {error}",
                    path.to_key()
                ))
            })?;
        for (key, value) in self.state.resolved_const_values() {
            lowering.seed_resolved_const(key.to_string(), value.clone());
        }
        let result = lowering.transform_async(hir).await;
        let (diagnostics, had_errors) = lowering.take_diagnostics();
            let details = diagnostics
                .iter()
                .map(|diagnostic| diagnostic.message.as_str())
                .collect::<Vec<_>>()
                .join("; ");
        let mir = match result {
            Ok(mir) => mir,
            Err(error) if details.is_empty() => {
                return Err(CompilerDriverError::InternalCompilerError(format!(
                    "HIR-to-MIR lowering failed: {error}"
                )));
            }
            Err(error) => {
                return Err(CompilerDriverError::InternalCompilerError(format!(
                    "HIR-to-MIR lowering failed: {error}; diagnostics: {details}"
                )));
            }
        };
        if had_errors {
            return Err(CompilerDriverError::InternalCompilerError(format!(
                "HIR-to-MIR lowering reported diagnostics: {details}"
            )));
        }
        lowering.walk_program_types_for_layouts(&mir);
        let adt_defs: HashMap<fp_core::hir::DefId, fp_core::mir::ty::AdtDef> =
            lowering.take_adt_defs();
        let struct_layouts: HashMap<fp_core::mir::DefId, Vec<fp_core::mir::Ty>> =
            lowering.all_adt_field_tys().into_iter().collect();
        let full_layouts: HashMap<
            (fp_core::mir::DefId, Vec<fp_core::mir::Ty>),
            Vec<fp_core::mir::Ty>,
        > = lowering
            .struct_layout_map()
                .iter()
                .map(|(key, layout)| ((key.def_id, key.args.clone()), layout.field_tys.clone()))
                .collect();
        let mir_id = MirId::new(format!("mir:{}", self.module_state_key(path.path())));
        self.state.insert_mir(mir_id.clone(), mir);
        Ok((mir_id, struct_layouts, full_layouts, adt_defs))
    }

    fn lower_to_lir(
        &mut self,
        mir_id: &MirId,
        path: &FullyQualifiedPath,
        package_id: &PackageId,
        full_layouts: &HashMap<(fp_core::mir::DefId, Vec<fp_core::mir::Ty>), Vec<fp_core::mir::Ty>>,
        adt_defs: &HashMap<fp_core::hir::DefId, fp_core::mir::ty::AdtDef>,
    ) -> Result<LirId, CompilerDriverError> {
        let mir = self.state.mir(mir_id)?.clone();
        let mut all_layouts: HashMap<fp_core::mir::DefId, Vec<fp_core::mir::Ty>> = HashMap::new();
        for (_dep_id, dep_package) in self.state.typing_ctx.env_ctx.crates().iter() {
            all_layouts.extend(
                dep_package
                    .borrow()
                    .mir_struct_fields
                    .iter()
                    .map(|(k, v)| (*k, v.clone())),
            );
        }
        let mut lowering = LirGenerator::new(self.state.typing_ctx.data_layout.clone())
            .with_package_id(package_id.clone())
            .with_module_path(path.path().to_key())
            .with_mir_layouts(all_layouts)
            .with_full_layouts(full_layouts.clone())
            .with_adt_defs(adt_defs.clone());
        let lir = lowering.transform(mir).map_err(|error| {
            CompilerDriverError::InternalCompilerError(format!(
                "MIR-to-LIR lowering failed for {}: {error}",
                path.to_key()
            ))
        })?;
        let lir_id = Self::package_module_lir_id(package_id, path.path());
        self.state.insert_lir(lir_id.clone(), lir);
        Ok(lir_id)
    }

    fn package_module_lir_id(package_id: &PackageId, path: &QualifiedPath) -> LirId {
        LirId::new(format!("lir:{}:{}", package_id.as_str(), path.to_key()))
    }

    fn collect_resolved_const_values(&self) -> HashMap<String, Value> {
        Self::resolved_const_values_snapshot(&self.state.typing_ctx)
    }

    /// Same as `collect_resolved_const_values`, but against a bare
    /// `Rc<TypingContext>` for compiler hooks that do not hold a driver.
    fn resolved_const_values_snapshot(typing_ctx: &TypingContext) -> HashMap<String, Value> {
        typing_ctx.resolved_consts.borrow().clone()
    }

    fn value_to_mir_constant(&self, value: &Value, ty: &mir::Ty) -> Option<mir::Constant> {
        let literal = match value {
            Value::Bool(value) => mir::ConstantKind::Bool(value.value),
            Value::Int(value) => mir::ConstantKind::Int(value.value),
            Value::UInt(value) => mir::ConstantKind::UInt(value.value),
            Value::Decimal(value) => mir::ConstantKind::Float(value.value),
            Value::String(value) => mir::ConstantKind::Str(value.value.clone()),
            Value::Bytes(bytes) => {
                let s = String::from_utf8_lossy(&bytes.value)
                    .trim_end_matches('\0')
                    .to_string();
                mir::ConstantKind::Str(s)
            }
            Value::Null(_) => mir::ConstantKind::Null,
            _ => mir::ConstantKind::Val(self.value_to_const_value(value, ty)?),
        };
        Some(mir::Constant {
            span: Span::null(),
            ty: ty.clone(),
            user_ty: None,
            literal,
        })
    }

    fn value_to_const_value(&self, value: &Value, ty: &mir::Ty) -> Option<mir::ConstValue> {
        match value {
            Value::Unit(_) => Some(mir::ConstValue::Unit),
            Value::Bool(value) => Some(mir::ConstValue::Bool(value.value)),
            Value::Int(value) => Some(match ty.kind {
                TyKind::Uint(UintTy::Usize)
                | TyKind::Uint(UintTy::U8)
                | TyKind::Uint(UintTy::U16)
                | TyKind::Uint(UintTy::U32)
                | TyKind::Uint(UintTy::U64)
                | TyKind::Uint(UintTy::U128) => mir::ConstValue::UInt(value.value as u64),
                _ => mir::ConstValue::Int(value.value),
            }),
            Value::UInt(value) => Some(match ty.kind {
                TyKind::Int(IntTy::Isize)
                | TyKind::Int(IntTy::I8)
                | TyKind::Int(IntTy::I16)
                | TyKind::Int(IntTy::I32)
                | TyKind::Int(IntTy::I64)
                | TyKind::Int(IntTy::I128) => mir::ConstValue::Int(value.value as i64),
                _ => mir::ConstValue::UInt(value.value),
            }),
            Value::Decimal(value) => Some(match ty.kind {
                TyKind::Float(FloatTy::F32) | TyKind::Float(FloatTy::F64) => {
                    mir::ConstValue::Float(value.value)
                }
                _ => return None,
            }),
            Value::String(value) => Some(mir::ConstValue::Str(value.value.clone())),
            Value::Bytes(bytes) => {
                let s = String::from_utf8_lossy(&bytes.value)
                    .trim_end_matches('\0')
                    .to_string();
                Some(mir::ConstValue::Str(s))
            }
            Value::Null(_) => Some(mir::ConstValue::Null),
            Value::Tuple(tuple) => {
                let TyKind::Tuple(fields) = &ty.kind else {
                    return None;
                };
                if tuple.values.len() != fields.len() {
                    return None;
                }
                let values = tuple
                    .values
                    .iter()
                    .zip(fields.iter())
                    .map(|(value, field_ty)| self.value_to_const_value(value, field_ty))
                    .collect::<Option<Vec<_>>>()?;
                Some(mir::ConstValue::Tuple(values))
            }
            Value::List(list) => match &ty.kind {
                TyKind::Array(elem_ty, _) => {
                    let values = list
                        .values
                        .iter()
                        .map(|value| self.value_to_const_value(value, elem_ty))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Array(values))
                }
                TyKind::Slice(elem_ty) => {
                    let values = list
                        .values
                        .iter()
                        .map(|value| self.value_to_const_value(value, elem_ty))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::List {
                        elements: values,
                        elem_ty: elem_ty.as_ref().clone(),
                    })
                }
                _ => None,
            },
            Value::Struct(value_struct) => match &ty.kind {
                TyKind::Tuple(fields) => {
                    if value_struct.structural.fields.len() != fields.len() {
                        return None;
                    }
                    let values = value_struct
                        .structural
                        .fields
                        .iter()
                        .zip(fields.iter())
                        .map(|(field, field_ty)| self.value_to_const_value(&field.value, field_ty))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Struct(values))
                }
                TyKind::Adt(adt_def, _substs) => {
                    let variant = adt_def.variants.first()?;
                    if value_struct.structural.fields.len() != variant.fields.len() {
                        return None;
                    }
                    let values = value_struct
                        .structural
                        .fields
                        .iter()
                        .map(|field| self.value_to_untyped_const_value(&field.value))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Struct(values))
                }
                _ => return None,
            },
            Value::Structural(structural) => match &ty.kind {
                TyKind::Tuple(fields) => {
                    if structural.fields.len() != fields.len() {
                        return None;
                    }
                    let values = structural
                        .fields
                        .iter()
                        .zip(fields.iter())
                        .map(|(field, field_ty)| self.value_to_const_value(&field.value, field_ty))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Struct(values))
                }
                TyKind::Adt(adt_def, _substs) => {
                    let variant = adt_def.variants.first()?;
                    if structural.fields.len() != variant.fields.len() {
                        return None;
                    }
                    let values = structural
                        .fields
                        .iter()
                        .map(|field| self.value_to_untyped_const_value(&field.value))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Struct(values))
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn value_to_untyped_const_value(&self, value: &Value) -> Option<mir::ConstValue> {
        match value {
            Value::Unit(_) => Some(mir::ConstValue::Unit),
            Value::Bool(value) => Some(mir::ConstValue::Bool(value.value)),
            Value::Int(value) => Some(mir::ConstValue::Int(value.value)),
            Value::UInt(value) => Some(mir::ConstValue::UInt(value.value)),
            Value::Decimal(value) => Some(mir::ConstValue::Float(value.value)),
            Value::String(value) => Some(mir::ConstValue::Str(value.value.clone())),
            Value::Bytes(bytes) => {
                let s = String::from_utf8_lossy(&bytes.value)
                    .trim_end_matches('\0')
                    .to_string();
                Some(mir::ConstValue::Str(s))
            }
            Value::Null(_) => Some(mir::ConstValue::Null),
            Value::Tuple(tuple) => Some(mir::ConstValue::Tuple(
                tuple
                    .values
                    .iter()
                    .map(|value| self.value_to_untyped_const_value(value))
                    .collect::<Option<Vec<_>>>()?,
            )),
            Value::List(list) => Some(mir::ConstValue::Array(
                list.values
                    .iter()
                    .map(|value| self.value_to_untyped_const_value(value))
                    .collect::<Option<Vec<_>>>()?,
            )),
            Value::Struct(value_struct) => Some(mir::ConstValue::Struct(
                value_struct
                    .structural
                    .fields
                    .iter()
                    .map(|field| self.value_to_untyped_const_value(&field.value))
                    .collect::<Option<Vec<_>>>()?,
            )),
            Value::Structural(structural) => Some(mir::ConstValue::Struct(
                structural
                    .fields
                    .iter()
                    .map(|field| self.value_to_untyped_const_value(&field.value))
                    .collect::<Option<Vec<_>>>()?,
            )),
            _ => None,
        }
    }

}
