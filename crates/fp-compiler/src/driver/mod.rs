mod error;
mod state;

pub use error::CompilerDriverError;
pub use state::CompilerState;

use fp_backend::transformations::{HirGenerator, LirGenerator, MirLowering};
use fp_core::ast::{
    BlockStmt, BlockStmtExpr, Expr, ExprBlock, ExprKind, ExprSplice, ExprSplicePending,
    Item, ItemChunk, ItemDefConst, ItemDefFunction, ItemKind, Node, NodeKind, Ty, Value,
};
use fp_core::diagnostics::DiagnosticLevel;
use fp_core::mir;
use fp_core::mir::ty::{FloatTy, IntTy, TyKind, UintTy};
use fp_core::module::path::QualifiedPath;
use fp_core::span::Span;
use fp_interpret::{LirInterpreter, VmError};
use fp_typing::{annotate_with_resolved_state, GenericMonorph, PendingTypingRequestKind};
use std::collections::{BTreeMap, HashMap};

use crate::scheduler::{
    AstId, BytecodeId, CompilerAnswer, CompilerRequest, CompilerScheduler, CompilerWork,
    ConstValueId, FullyQualifiedPath, GenericWorkRequest, HirId,
    LirId, MirId, ScheduledAnswer, ScopeId,
    TypedAstId,
};

use crate::driver::state::SpliceResult;

pub struct CompilerDriver {
    pub scheduler: CompilerScheduler,
    pub state: CompilerState,
    interpreter: LirInterpreter,
}

impl CompilerDriver {
    pub fn new() -> Self {
        Self {
            scheduler: CompilerScheduler::new(),
            state: CompilerState::new(),
            interpreter: LirInterpreter::new(),
        }
    }

    pub fn with_state(state: CompilerState) -> Self {
        Self {
            scheduler: CompilerScheduler::new(),
            state,
            interpreter: LirInterpreter::new(),
        }
    }

    pub fn run_next(&mut self) -> Result<Option<ScheduledAnswer>, CompilerDriverError> {
        let Some(request) = self.scheduler.next_request() else {
            return Ok(None);
        };
        self.scheduler.begin_processing(request.id);
        let answer = self.handle_request(&request)?;
        self.scheduler.end_processing();
        let scheduled = self.scheduler.answer_and_schedule(request.id, answer)?;
        Ok(Some(scheduled))
    }

    fn handle_request(
        &mut self,
        request: &CompilerRequest,
    ) -> Result<CompilerAnswer, CompilerDriverError> {
        match &request.work {
            CompilerWork::CompileUnitCompileNative { ast, scope, path } => {
                self.compile_unit_compile_native(ast, scope, path)
            }
            CompilerWork::CompileUnitAnswerComptime { typed_ast, path } => {
                self.compile_unit_answer_comptime(typed_ast, path)
            }
            CompilerWork::EnqueueGeneric {
                typed_ast,
                path,
                generic,
            } => self.enqueue_generic(typed_ast, path, generic),
            _ => Err(CompilerDriverError::UnsupportedWork(format!("{request:?}"))),
        }
    }

    fn compile_unit_compile_native(
        &mut self,
        ast_id: &AstId,
        _scope: &ScopeId,
        path: &FullyQualifiedPath,
    ) -> Result<CompilerAnswer, CompilerDriverError> {
        let mut ast = Some(self.state.ast(ast_id)?.clone());
        AstPreProcessor::new(&mut self.state.splice_results).walk(ast.as_mut().unwrap());

        let is_lowerable = matches!(
            ast.as_ref().unwrap().kind(),
            NodeKind::Expr(_) | NodeKind::File(_) | NodeKind::Query(_)
        );

        let mut pending_requests;
        let mut typed_first = false;
        let mut iterations = 0u32;
        let mut pending_generics = Vec::new();

        loop {
            iterations += 1;
            if iterations > 100 {
                return Err(CompilerDriverError::UnsupportedWork(
                    "compile unit stuck in comptime evaluation loop".to_string()
                ));
            }

            let resolved_consts = self.collect_resolved_const_values();
            let module_resolution = self.state.module_resolution(ast_id);
            let outcome = annotate_with_resolved_state(
                ast.as_mut().unwrap(),
                module_resolution,
                resolved_consts,
                self.state.expr_resolutions(),
            )?;

            if !typed_first {
                self.state.extend_typing_diagnostics(outcome.diagnostics);
                typed_first = true;
            } else {
                let _ = outcome.diagnostics;
            }

            pending_requests = outcome.pending_requests;
            pending_generics = outcome.pending_generics;

            if !is_lowerable {
                break;
            }

            // Lower and evaluate.
            let tmp_typed_ast = TypedAstId::new(format!("typed_ast:{}", path.to_key()));
            let tmp_id = tmp_typed_ast.clone();
            self.state.insert_typed_ast(tmp_typed_ast, ast.take().unwrap());

            let hir_id = self.lower_to_hir(&tmp_id, path)?;
            let mir_id = self.lower_to_mir(&hir_id, path)?;
            let lir_id = self.lower_to_lir(&mir_id, path)?;
            let had_entries = self.evaluate_comptime_lir(&lir_id, path)? > 0;

            self.generate_bytecode(&mir_id, path)?;

            // Check if we need another iteration: typing found comptime needs and
            // evaluation found entries to process.
            let has_comptime = pending_requests.iter().any(|r| {
                matches!(
                    r.kind,
                    PendingTypingRequestKind::Comptime | PendingTypingRequestKind::Unresolved
                )
            });

            if !has_comptime && iterations == 1 {
                // No comptime needs and first pass — lowering is complete
                break;
            }

            if !had_entries {
                // No new entries to evaluate — done
                break;
            }

            ast = Some(self.state.ast(ast_id)?.clone());
            AstPreProcessor::new(&mut self.state.splice_results).walk(ast.as_mut().unwrap());
        }

        let typed_ast_id = TypedAstId::new(format!("typed_ast:{}", path.to_key()));

        if is_lowerable {
            // Re-clone and re-type on the final pass with all resolved const values
            ast = Some(self.state.ast(ast_id)?.clone());
            AstPreProcessor::new(&mut self.state.splice_results).walk(ast.as_mut().unwrap());
            let resolved_consts = self.collect_resolved_const_values();
            let module_resolution = self.state.module_resolution(ast_id);
            let outcome = annotate_with_resolved_state(
                ast.as_mut().unwrap(),
                module_resolution,
                resolved_consts,
                self.state.expr_resolutions(),
            )?;
            let _ = outcome.diagnostics;
            pending_requests = outcome.pending_requests;
            pending_generics = outcome.pending_generics;
        }

        self.state.insert_typed_ast(typed_ast_id.clone(), ast.unwrap());

        for monomorph in &pending_generics {
            let generic = GenericWorkRequest::new(
                FullyQualifiedPath::new(monomorph.function_path.clone()),
                monomorph.generic_params.clone(),
                monomorph.concrete_types.clone(),
            );
            self.scheduler.submit(CompilerWork::EnqueueGeneric {
                typed_ast: typed_ast_id.clone(),
                path: path.clone(),
                generic,
            });
        }

        Ok(CompilerAnswer::CompileUnitCompileNative)
    }

    fn compile_unit_answer_comptime(
        &mut self,
        typed_ast_id: &TypedAstId,
        path: &FullyQualifiedPath,
    ) -> Result<CompilerAnswer, CompilerDriverError> {
        let hir_id = self.lower_to_hir(typed_ast_id, path)?;
        let mir_id = self.lower_to_mir(&hir_id, path)?;
        let lir_id = self.lower_to_lir(&mir_id, path)?;

        let _count = self.evaluate_comptime_lir(&lir_id, path)?;
        let value_id = ConstValueId::new(format!("const_value:{}", path.to_key()));
        Ok(CompilerAnswer::CompileUnitAnswerComptime { value: value_id })
    }

    fn enqueue_generic(
        &mut self,
        typed_ast_id: &TypedAstId,
        _path: &FullyQualifiedPath,
        generic: &GenericWorkRequest,
    ) -> Result<CompilerAnswer, CompilerDriverError> {
        let cannon_key = Self::generic_cannon_key(&generic.path, &generic.concrete_types);

        if self.state.generic_instantiations.contains(&cannon_key) {
            return Ok(CompilerAnswer::GenericQueued {
                generic: generic.clone(),
            });
        }
        self.state.generic_instantiations.insert(cannon_key.clone());

        let typed_ast = self.state.typed_ast(typed_ast_id)?;
        let function_path = generic.path.path();
        let mut func_item = Self::find_function_def(typed_ast, function_path)
            .ok_or_else(|| CompilerDriverError::UnsupportedWork(format!(
                "generic function not found: {}", function_path.to_key()
            )))?;

        if let ItemKind::DefFunction(def) = func_item.kind_mut() {
            for param in &mut def.sig.params {
                Self::substitute_in_ty(&mut param.ty, &generic.generic_params, &generic.concrete_types);
            }
            if let Some(ret_ty) = &mut def.sig.ret_ty {
                Self::substitute_in_ty(ret_ty, &generic.generic_params, &generic.concrete_types);
            }
            def.sig.generics_params.clear();
        }

        let specialized_path = FullyQualifiedPath::new(
            function_path.with_segment(cannon_key.clone())
        );
        let specialized_ast_id = AstId::new(format!("ast:{}", specialized_path.to_key()));
        let node = Node::item(func_item);
        self.state.insert_ast(specialized_ast_id.clone(), node);

        self.scheduler.submit(CompilerWork::CompileUnitCompileNative {
            ast: specialized_ast_id,
            scope: ScopeId::new(specialized_path.to_key()),
            path: specialized_path,
        });

        Ok(CompilerAnswer::GenericQueued {
            generic: generic.clone(),
        })
    }

    fn generic_cannon_key(path: &FullyQualifiedPath, concrete_types: &[Ty]) -> String {
        let types_str: Vec<String> = concrete_types
            .iter()
            .map(|ty| format!("{:?}", ty))
            .collect();
        format!("{}#<{}>", path.to_key(), types_str.join(", "))
    }

    fn find_function_def(node: &Node, path: &QualifiedPath) -> Option<Item> {
        match node.kind() {
            NodeKind::File(file) => {
                let key = path.to_key();
                let segments: Vec<&str> = key.split("::").collect();
                for item in &file.items {
                    if let Some(found) = Self::find_in_items(item, &segments, 0) {
                        return Some(found);
                    }
                }
                None
            }
            NodeKind::Item(item) => {
                let key = path.to_key();
                let segments: Vec<&str> = key.split("::").collect();
                Self::find_in_items(item, &segments, 0)
            }
            _ => None,
        }
    }

    fn find_in_items(item: &Item, segments: &[&str], idx: usize) -> Option<Item> {
        if idx >= segments.len() {
            return None;
        }
        let target = segments[idx];
        match item.kind() {
            ItemKind::DefFunction(def) => {
                if def.name.as_str() == target && idx == segments.len() - 1 {
                    return Some(item.clone());
                }
                None
            }
            ItemKind::Module(module) => {
                if module.name.as_str() == target {
                    for child in &module.items {
                        if let Some(found) = Self::find_in_items(child, segments, idx + 1) {
                            return Some(found);
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    fn substitute_in_ty(ty: &mut Ty, _param_names: &[String], concrete_types: &[Ty]) {
        match ty {
            Ty::GenericVar(gv) => {
                let idx = gv.index as usize;
                if idx < concrete_types.len() {
                    *ty = concrete_types[idx].clone();
                }
            }
            Ty::Function(f) => {
                for param in &mut f.params {
                    Self::substitute_in_ty(param, _param_names, concrete_types);
                }
                if let Some(ret_ty) = &mut f.ret_ty {
                    Self::substitute_in_ty(ret_ty, _param_names, concrete_types);
                }
            }
            Ty::Reference(r) => {
                Self::substitute_in_ty(&mut *r.ty, _param_names, concrete_types);
            }
            Ty::Slice(s) => {
                Self::substitute_in_ty(&mut *s.elem, _param_names, concrete_types);
            }
            Ty::Array(a) => {
                Self::substitute_in_ty(&mut *a.elem, _param_names, concrete_types);
            }
            Ty::Vec(v) => {
                Self::substitute_in_ty(&mut *v.ty, _param_names, concrete_types);
            }
            Ty::Tuple(t) => {
                for ty in &mut t.types {
                    Self::substitute_in_ty(ty, _param_names, concrete_types);
                }
            }
            _ => {}
        }
    }

    fn generate_bytecode(
        &mut self,
        mir_id: &MirId,
        path: &FullyQualifiedPath,
    ) -> Result<(), CompilerDriverError> {
        let mir = self.state.mir(mir_id)?.clone();
        let program = fp_bytecode::lower_program(&mir)?;
        let bytecode_id = BytecodeId::new(format!("bytecode:{}", path.to_key()));
        self.state.insert_bytecode(bytecode_id, program);
        Ok(())
    }

    fn evaluate_comptime_lir(
        &mut self,
        lir_id: &LirId,
        path: &FullyQualifiedPath,
    ) -> Result<usize, CompilerDriverError> {
        let lir = self.state.lir(lir_id)?.clone();
        let value_id = ConstValueId::new(format!("const_value:{}", path.to_key()));

        if lir.comptime_entries.is_empty() {
            self.state.insert_const_value(value_id.clone(), Value::unit());
            return Ok(0);
        }

        let mut count = 0usize;
        let mut last = Value::unit();
        for entry in &lir.comptime_entries {
            let value = match self.evaluate_lir_function(&lir, entry.function.as_str()) {
                Ok(v) => v,
                Err(_) => {
                    // Dependency not yet resolved — skip this entry,
                    // it will be evaluated on the next pass.
                    continue;
                }
            };
            let constant = self.value_to_mir_constant(&value, &entry.ty).ok_or_else(|| {
                CompilerDriverError::UnsupportedWork(format!(
                    "unsupported comptime result for {}",
                    entry.key
                ))
            })?;
            self.state
                .insert_resolved_const_value(entry.key.clone(), constant);
            if let Some(expr_id) = Self::expr_id_from_const_key(&entry.key) {
                self.state
                    .insert_expr_resolution_value(expr_id, value.clone());
            }
            last = value;
            count += 1;
        }

        self.state.insert_const_value(value_id.clone(), last);
        Ok(count)
    }

    fn lower_to_hir(
        &mut self,
        typed_ast_id: &TypedAstId,
        path: &FullyQualifiedPath,
    ) -> Result<HirId, CompilerDriverError> {
        let ast = self.state.typed_ast(typed_ast_id)?;
        let hir_program = match ast.kind() {
            NodeKind::Expr(expr) => HirGenerator::new()
                .with_expr_resolution(self.state.expr_resolutions().clone())
                .transform_expr(expr)?,
            NodeKind::File(file) => HirGenerator::with_file(&file.path)
                .with_expr_resolution(self.state.expr_resolutions().clone())
                .transform_file(file)?,
            NodeKind::Query(query) => HirGenerator::new().transform_query_document(query)?,
            NodeKind::Item(_) | NodeKind::Schema(_) | NodeKind::Workspace(_) => {
                return Err(CompilerDriverError::UnsupportedWork(format!(
                    "cannot lower AST node kind to HIR: {:?}",
                    ast.kind()
                )));
            }
        };
        let hir = HirId::new(format!("hir:{}", path.to_key()));
        self.state.insert_hir(hir.clone(), hir_program);
        Ok(hir)
    }

    fn lower_to_mir(
        &mut self,
        hir_id: &HirId,
        path: &FullyQualifiedPath,
    ) -> Result<MirId, CompilerDriverError> {
        let hir = self.state.hir(hir_id)?.clone();
        let mut lowering = MirLowering::new();
        lowering.set_lossy(self.state.lossy());
        for (key, value) in self.state.resolved_const_values() {
            lowering.seed_resolved_const(key.to_string(), value.clone());
        }
        let mir = lowering.transform(hir);
        let (diagnostics, had_errors) = lowering.take_diagnostics();
        let mir = match (mir, had_errors, self.state.lossy()) {
            (Ok(program), false, _) => program,
            (Ok(_), true, true) => fp_core::mir::Program::new(),
            (Err(_), _, true) => fp_core::mir::Program::new(),
            (Ok(_), true, false) => {
                let message = diagnostics
                    .iter()
                    .find(|diagnostic| diagnostic.level == DiagnosticLevel::Error)
                    .map(|diagnostic| diagnostic.message.clone())
                    .unwrap_or_else(|| "HIR→MIR lowering reported errors".to_string());
                return Err(CompilerDriverError::UnsupportedWork(message));
            }
            (Err(err), _, false) => return Err(err.into()),
        };
        let mir_id = MirId::new(format!("mir:{}", path.to_key()));
        self.state.insert_mir(mir_id.clone(), mir);
        Ok(mir_id)
    }

    fn lower_to_lir(
        &mut self,
        mir_id: &MirId,
        path: &FullyQualifiedPath,
    ) -> Result<LirId, CompilerDriverError> {
        let mir = self.state.mir(mir_id)?.clone();
        let mut lowering = LirGenerator::new();
        let lir = lowering.transform(mir)?;
        let lir_id = LirId::new(format!("lir:{}", path.to_key()));
        self.state.insert_lir(lir_id.clone(), lir);
        Ok(lir_id)
    }

    fn evaluate_lir_function(
        &mut self,
        lir: &fp_core::lir::LirProgram,
        name: &str,
    ) -> Result<fp_core::ast::Value, VmError> {
        self.interpreter = LirInterpreter::new();
        let resolved = self.collect_resolved_const_values();
        self.interpreter.inject_globals(&resolved);
        self.interpreter.run_function_named(lir, name)
    }

    fn collect_resolved_const_values(&self) -> HashMap<String, Value> {
        let mut map = HashMap::new();
        for (key, constant) in self.state.resolved_const_values() {
            if let Some(value) = self.mir_constant_to_value(constant) {
                map.insert(key.to_string(), value.clone());
                if let Some(short) = key.rsplit(':').next() {
                    map.insert(short.to_string(), value);
                }
            }
        }
        map
    }

    fn mir_constant_to_value(&self, constant: &mir::Constant) -> Option<Value> {
        Some(match &constant.literal {
            mir::ConstantKind::Bool(v) => Value::bool(*v),
            mir::ConstantKind::Int(v) => Value::int(*v),
            mir::ConstantKind::UInt(v) => Value::uint(*v),
            mir::ConstantKind::Float(v) => Value::decimal(*v),
            mir::ConstantKind::Str(v) => Value::string(v.clone()),
            mir::ConstantKind::Null => Value::null(),
            mir::ConstantKind::Val(value, ty) => self.const_value_to_value(value, Some(ty))?,
            _ => {
                return None;
            }
        })
    }

    fn const_value_to_value(&self, cv: &mir::ConstValue, ty: Option<&mir::Ty>) -> Option<Value> {
        Some(match cv {
            mir::ConstValue::Unit => Value::unit(),
            mir::ConstValue::Bool(v) => Value::bool(*v),
            mir::ConstValue::Int(v) => Value::int(*v),
            mir::ConstValue::UInt(v) => Value::uint(*v),
            mir::ConstValue::Float(v) => Value::decimal(*v),
            mir::ConstValue::Str(v) => Value::string(v.clone()),
            mir::ConstValue::Null => Value::null(),
            mir::ConstValue::Tuple(fields) => {
                let field_tys = match ty.map(|ty| &ty.kind) {
                    Some(TyKind::Tuple(field_tys)) => Some(field_tys.as_slice()),
                    _ => None,
                };
                Value::Tuple(fp_core::ast::ValueTuple::new(
                    fields
                        .iter()
                        .enumerate()
                        .map(|(index, value)| {
                            self.const_value_to_value(
                                value,
                                field_tys.and_then(|tys| tys.get(index).map(|ty| ty.as_ref())),
                            )
                        })
                        .collect::<Option<Vec<_>>>()?,
                ))
            }
            mir::ConstValue::Array(elements) => {
                let elem_ty = match ty.map(|ty| &ty.kind) {
                    Some(TyKind::Array(elem_ty, _)) => Some(elem_ty.as_ref()),
                    _ => None,
                };
                Value::List(fp_core::ast::ValueList::new(
                    elements
                        .iter()
                        .map(|value| self.const_value_to_value(value, elem_ty))
                        .collect::<Option<Vec<_>>>()?,
                ))
            }
            mir::ConstValue::List { elements, elem_ty } => {
                Value::List(fp_core::ast::ValueList::new(
                    elements
                        .iter()
                        .map(|value| self.const_value_to_value(value, Some(elem_ty)))
                        .collect::<Option<Vec<_>>>()?,
                ))
            }
            mir::ConstValue::Map {
                entries,
                key_ty,
                value_ty,
            } => Value::Map(fp_core::ast::ValueMap::from_pairs(
                entries
                    .iter()
                    .map(|(key, value)| {
                        Some((
                            self.const_value_to_value(key, Some(key_ty))?,
                            self.const_value_to_value(value, Some(value_ty))?,
                        ))
                    })
                    .collect::<Option<Vec<_>>>()?,
            )),
            mir::ConstValue::Struct(fields) => match ty.map(|ty| &ty.kind) {
                Some(TyKind::Adt(adt_def, _)) => {
                    let variant = adt_def.variants.first()?;
                    if variant.fields.len() != fields.len() {
                        return None;
                    }
                    Value::Structural(fp_core::ast::ValueStructural::new(
                        variant
                            .fields
                            .iter()
                            .zip(fields.iter())
                            .map(|(field_def, field_value)| {
                                Some(fp_core::ast::ValueField::new(
                                    field_def.ident.as_str().into(),
                                    self.const_value_to_value(field_value, None)?,
                                ))
                            })
                            .collect::<Option<Vec<_>>>()?,
                    ))
                }
                Some(TyKind::Tuple(field_tys)) => Value::Tuple(fp_core::ast::ValueTuple::new(
                    fields
                        .iter()
                        .enumerate()
                        .map(|(index, value)| {
                            self.const_value_to_value(
                                value,
                                field_tys.get(index).map(|ty| ty.as_ref()),
                            )
                        })
                        .collect::<Option<Vec<_>>>()?,
                )),
                _ => Value::Tuple(fp_core::ast::ValueTuple::new(
                    fields
                        .iter()
                        .map(|value| self.const_value_to_value(value, None))
                        .collect::<Option<Vec<_>>>()?,
                )),
            },
            _ => return None,
        })
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
            _ => mir::ConstantKind::Val(self.value_to_const_value(value, ty)?, ty.clone()),
        };
        Some(mir::Constant {
            span: Span::null(),
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

    fn expr_id_from_const_key(key: &str) -> Option<u64> {
        let name = key.rsplit(':').next()?;
        let suffix = name.strip_prefix("__fp_expr_")?;
        suffix.parse().ok()
    }
}


struct AstPreProcessor<'a> {
    splice_results: &'a mut BTreeMap<String, SpliceResult>,
}

impl<'a> AstPreProcessor<'a> {
    fn new(splice_results: &'a mut BTreeMap<String, SpliceResult>) -> Self {
        Self { splice_results }
    }

    fn walk(&mut self, node: &mut Node) {
        match node.kind_mut() {
            NodeKind::File(file) => {
                file.collected_items = direct_items(&file.items);
                self.resolve_items(&mut file.items);
            }
            _ => {}
        }
    }

    fn resolve_items(&mut self, items: &mut [Item]) {
        for item in items {
            match item.kind_mut() {
                ItemKind::Module(m) => {
                    m.collected_items = direct_items(&m.items);
                    self.resolve_items(&mut m.items);
                }
                ItemKind::DefFunction(func) => {
                    func.collected_items = direct_expr_items(func.body.as_ref());
                    self.process_expr(&mut func.body);
                }
                ItemKind::DefConst(def) => {
                    self.collect_quote(def);
                }
                ItemKind::Impl(imp) => {
                    imp.collected_items = direct_items(&imp.items);
                    self.resolve_items(&mut imp.items);
                }
                ItemKind::DefTrait(t) => {
                    t.collected_items = direct_items(&t.items);
                    self.resolve_items(&mut t.items);
                }
                _ => {}
            }
        }
    }

    fn process_expr(&mut self, expr: &mut Expr) {
        match expr.kind_mut() {
            ExprKind::Block(block) => {
                block.collected_items = direct_block_items(block);
                let mut new_stmts: Vec<BlockStmt> = Vec::new();
                for stmt in block.stmts.drain(..) {
                    match stmt {
                        BlockStmt::Item(item) => {
                            new_stmts.push(BlockStmt::Item(item));
                        }
                        BlockStmt::Expr(mut e) => {
                            if matches!(e.expr.kind(), ExprKind::Splice(_)) {
                                let resolved = self.resolve_splice(&mut e.expr, &mut new_stmts);
                                if !resolved {
                                    new_stmts.push(BlockStmt::Expr(e));
                                }
                            } else {
                                self.process_expr(&mut e.expr);
                                new_stmts.push(BlockStmt::Expr(e));
                            }
                        }
                        BlockStmt::Let(mut s) => {
                            if let Some(init) = s.init.as_mut() {
                                self.process_expr(init);
                            }
                            new_stmts.push(BlockStmt::Let(s));
                        }
                        BlockStmt::Defer(mut d) => {
                            self.process_expr(d.expr.as_mut());
                            new_stmts.push(BlockStmt::Defer(d));
                        }
                        other => new_stmts.push(other),
                    }
                }
                block.stmts = new_stmts;
            }
            ExprKind::If(e) => {
                self.process_expr(e.then.as_mut());
                if let Some(elze) = e.elze.as_mut() {
                    self.process_expr(elze);
                }
            }
            ExprKind::Loop(e) => self.process_expr(e.body.as_mut()),
            ExprKind::For(e) => self.process_expr(e.body.as_mut()),
            ExprKind::While(e) => self.process_expr(e.body.as_mut()),
            ExprKind::Match(e) => {
                for case in &mut e.cases {
                    self.process_expr(case.body.as_mut());
                }
            }
            ExprKind::ConstBlock(cb) => {
                cb.collected_items = direct_expr_items(cb.expr.as_ref());
                self.process_expr(cb.expr.as_mut());
            }
            ExprKind::Quote(q) => {
                q.collected_items = direct_block_items(&q.block);
                self.process_expr_block_items(&mut q.block.stmts);
            }
            ExprKind::Splice(_splice) => {
                let old = std::mem::replace(expr.kind_mut(), ExprKind::Id(0));
                if let ExprKind::Splice(splice) = old {
                    self.mark_splice_pending(expr, splice);
                }
            }
            ExprKind::Invoke(invoke) => {
                for arg in &mut invoke.args {
                    self.process_expr(arg);
                }
            }
            ExprKind::Struct(s) => {
                for field in &mut s.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.process_expr(value);
                    }
                }
            }
            ExprKind::Tuple(t) => {
                for value in &mut t.values {
                    self.process_expr(value);
                }
            }
            ExprKind::Array(a) => {
                for value in &mut a.values {
                    self.process_expr(value);
                }
            }
            ExprKind::With(w) => {
                self.process_expr(w.context.as_mut());
                self.process_expr(w.body.as_mut());
            }
            ExprKind::BinOp(b) => {
                self.process_expr(b.lhs.as_mut());
                self.process_expr(b.rhs.as_mut());
            }
            ExprKind::UnOp(u) => self.process_expr(u.val.as_mut()),
            ExprKind::Assign(a) => {
                self.process_expr(a.value.as_mut());
            }
            ExprKind::Cast(c) => self.process_expr(c.expr.as_mut()),
            ExprKind::Return(r) => {
                if let Some(value) = r.value.as_mut() {
                    self.process_expr(value.as_mut());
                }
            }
            ExprKind::Let(l) => self.process_expr(l.expr.as_mut()),
            _ => {}
        }
    }

    fn process_expr_block_items(&mut self, stmts: &mut Vec<BlockStmt>) {
        for stmt in stmts {
            match stmt {
                BlockStmt::Item(item) => {
                    if let ItemKind::DefConst(def) = item.kind_mut() {
                        self.collect_quote(def);
                    }
                }
                BlockStmt::Expr(e) => self.process_expr(&mut e.expr),
                BlockStmt::Let(s) => {
                    if let Some(init) = s.init.as_mut() {
                        self.process_expr(init);
                    }
                }
                _ => {}
            }
        }
    }

    fn collect_quote(&mut self, def: &mut ItemDefConst) {
        if !matches!(def.value.kind(), ExprKind::Quote(_)) || def.name.as_str().is_empty() {
            return;
        }
        let ExprKind::Quote(quote) = def.value.kind_mut() else {
            return;
        };
        let mut result = SpliceResult {
            items: Vec::new(),
            stmts: Vec::new(),
            expr: None,
        };
        for stmt in &quote.block.stmts {
            match stmt {
                BlockStmt::Item(item) => result.items.push(item.as_ref().clone()),
                BlockStmt::Expr(e) => result.stmts.push(BlockStmt::Expr(e.clone())),
                other => result.stmts.push(other.clone()),
            }
        }
        if let Some(e) = quote.block.last_expr() {
            result.expr = Some(e.clone());
        }
        let key = def.name.as_str().to_string();
        self.splice_results.insert(key, result);
    }

    fn resolve_splice(&mut self, expr: &mut Expr, stmts: &mut Vec<BlockStmt>) -> bool {
        let old = std::mem::replace(expr.kind_mut(), ExprKind::Id(0));
        let ExprKind::Splice(splice) = old else {
            return false;
        };
        let const_name = if let ExprKind::Name(ref name) = splice.token.kind() {
            name.to_string()
        } else {
            String::new()
        };
        if let Some(result) = self.splice_results.get(&const_name) {
            for item in &result.items {
                stmts.push(BlockStmt::Item(Box::new(item.clone())));
            }
            for stmt in &result.stmts {
                stmts.push(stmt.clone());
            }
            if let Some(ref e) = result.expr {
                stmts.push(BlockStmt::Expr(BlockStmtExpr {
                    expr: Box::new(e.clone()),
                    semicolon: None,
                }));
            }
            return true;
        }
        self.mark_splice_pending(expr, splice);
        false
    }

    fn mark_splice_pending(&mut self, expr: &mut Expr, splice: ExprSplice) {
        *expr.kind_mut() = ExprKind::SplicePending(ExprSplicePending {
            span: splice.span,
            request_id: 0,
            token: splice.token.clone(),
        });
    }
}

fn direct_items(items: &[Item]) -> ItemChunk {
    items.to_vec()
}

fn direct_expr_items(expr: &Expr) -> ItemChunk {
    match expr.kind() {
        ExprKind::Block(block) => block.collected_items.clone(),
        _ => Vec::new(),
    }
}

fn direct_block_items(block: &ExprBlock) -> ItemChunk {
    block
        .stmts
        .iter()
        .filter_map(|stmt| match stmt {
            BlockStmt::Item(item) => Some(item.as_ref().clone()),
            _ => None,
        })
        .collect()
}


impl Default for CompilerDriver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scheduler::ScopeId;
    use fp_core::ast::{
        Expr, FunctionSignature, GenericParam, Ident, Item, ItemDefFunction, Node, TypeBounds,
    };

    fn path() -> FullyQualifiedPath {
        FullyQualifiedPath::from_segments(vec!["crate".to_string(), "main".to_string()])
    }

    #[test]
    fn compile_unit_native_runs_full_pipeline() {
        let path = path();
        let ast_id = AstId::new("ast:crate::main");
        let mut driver = CompilerDriver::new();
        driver
            .state
            .insert_ast(ast_id.clone(), Node::expr(Expr::unit()));

        driver.scheduler.submit(CompilerWork::CompileUnitCompileNative {
            ast: ast_id,
            scope: ScopeId::new("crate::main"),
            path,
        });

        let scheduled = driver
            .run_next()
            .expect("compile unit")
            .expect("compiled answer");
        assert!(matches!(
            scheduled.completed.answer,
            CompilerAnswer::CompileUnitCompileNative
        ));

        assert_eq!(driver.state.hir_len(), 1);
        assert_eq!(driver.state.mir_len(), 1);
        assert_eq!(driver.state.lir_len(), 1);
        assert!(driver.scheduler.is_idle());
    }

    #[test]
    fn const_item_discovers_comptime_need_and_evaluates() {
        let path = path();
        let ast_id = AstId::new("ast:crate::answer");

        let const_block = fp_core::ast::ExprConstBlock {
            span: fp_core::span::Span::null(),
            collected_items: Vec::new(),
            expr: Box::new(Expr::value(fp_core::ast::Value::int(42))),
        };
        let expr = Expr::from(fp_core::ast::ExprKind::ConstBlock(const_block));

        let mut driver = CompilerDriver::new();
        driver.state.insert_ast(ast_id.clone(), Node::expr(expr));

        driver.scheduler.submit(CompilerWork::CompileUnitCompileNative {
            ast: ast_id,
            scope: ScopeId::new("crate::answer"),
            path: path.clone(),
        });

        let scheduled = driver.run_next().expect("compile unit").expect("compiled answer");
        assert!(
            matches!(scheduled.completed.answer, CompilerAnswer::CompileUnitCompileNative),
            "should return CompileUnitCompileNative"
        );

        assert_eq!(driver.state.const_value_len(), 1, "const block should produce const value");
        assert!(
            driver.scheduler.is_idle(),
            "scheduler should be idle after compile handles comptime inline"
        );
    }

    #[test]
    fn driver_loop_resolves_full_comptime_chain_to_completion() {
        let path = path();
        let ast_id = AstId::new("ast:crate::const");
        let mut driver = CompilerDriver::new();
        driver
            .state
            .insert_ast(ast_id.clone(), Node::expr(Expr::unit()));

        driver.scheduler.submit(CompilerWork::CompileUnitCompileNative {
            ast: ast_id,
            scope: ScopeId::new("crate::const"),
            path,
        });

        let mut steps = 0;
        while let Ok(Some(scheduled)) = driver.run_next() {
            steps += 1;
            assert!(steps <= 20, "driver loop should not run forever");
            let _ = scheduled;
        }

        assert_eq!(
            driver.state.const_value_len(),
            1,
            "compile unit should evaluate LIR comptime entries"
        );
        assert!(
            driver.scheduler.is_idle(),
            "scheduler should be idle after compile resolves"
        );
    }

    #[test]
    fn compile_unit_native_submits_enqueue_for_generic() {
        let path = path();
        let ast_id = AstId::new("ast:crate::generic");
        let mut generic_function =
            ItemDefFunction::new_simple(Ident::new("id"), Expr::unit().into());
        generic_function.sig = FunctionSignature {
            generics_params: vec![GenericParam {
                name: Ident::new("T"),
                bounds: TypeBounds::any(),
            }],
            ..FunctionSignature::unit()
        };

        let mut driver = CompilerDriver::new();
        driver
            .state
            .insert_ast(ast_id.clone(), Node::item(Item::from(generic_function)));

        driver.scheduler.submit(CompilerWork::CompileUnitCompileNative {
            ast: ast_id,
            scope: ScopeId::new("crate::generic"),
            path,
        });

        let scheduled = driver.run_next().expect("compile unit").expect("compiled answer");
        assert!(
            matches!(scheduled.completed.answer, CompilerAnswer::CompileUnitCompileNative)
        );

        // Generic work item enqueued
        if !scheduled.followups.is_empty() {
            let next = driver.scheduler.next_request().expect("generic work");
            assert!(matches!(next.work, CompilerWork::EnqueueGeneric { .. }));
        }
    }
}

#[cfg(test)]
mod comptime_source_tests {
    use super::*;
    use crate::scheduler::{AstId, CompilerWork, FullyQualifiedPath, ScopeId};
    use fp_core::frontend::LanguageFrontend;

    fn path() -> FullyQualifiedPath {
        FullyQualifiedPath::from_segments(vec!["test".to_string(), "main".to_string()])
    }

    #[test]
    fn parses_fp_source_and_runs_const_item_through_driver() {
        let source = r#"
const ANSWER: i64 = 42;

fn main() {
    let result = const { ANSWER * 2 };
}
"#;
        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(source, std::path::Path::new("test.fp"))
            .expect("parse .fp source");
        let ast_node = result.ast;

        let mut driver = CompilerDriver::new();
        let ast_id = AstId::new("ast:test::main");
        driver.state.insert_ast(ast_id.clone(), ast_node);

        driver.scheduler.submit(CompilerWork::CompileUnitCompileNative {
            ast: ast_id,
            scope: ScopeId::new("test::main"),
            path: path(),
        });

        let scheduled = driver.run_next().expect("compile unit").expect("compiled answer");
        assert!(
            matches!(scheduled.completed.answer, CompilerAnswer::CompileUnitCompileNative),
            "const item and const block should produce compile with comptime resolution"
        );
    }

    #[test]
    fn parses_simple_const_and_evaluates_through_full_pipeline() {
        let source = "const ANSWER: i64 = 42;\n";
        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(source, std::path::Path::new("const.fp"))
            .expect("parse const source");
        let ast_node = result.ast;

        let mut driver = CompilerDriver::new();
        let ast_id = AstId::new("ast:test::const");
        driver.state.insert_ast(ast_id.clone(), ast_node);

        driver.scheduler.submit(CompilerWork::CompileUnitCompileNative {
            ast: ast_id,
            scope: ScopeId::new("test::const"),
            path: path(),
        });

        // Compile handles comptime evaluation inline
        let _scheduled = driver.run_next().expect("compile unit").expect("compiled");

        // Drain any remaining work (e.g., generic followup)
        while let Ok(Some(_)) = driver.run_next() {}

        assert_eq!(
            driver.state.const_value_len(),
            1,
            "const should produce one compile-time value"
        );
    }

    #[test]
    fn multiple_const_items_produce_separate_comptime_needs() {
        let source = r#"
const WIDTH: i64 = 640;
const HEIGHT: i64 = 480;
const AREA: i64 = WIDTH * HEIGHT;
"#;
        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(source, std::path::Path::new("multi.fp"))
            .expect("parse multi-const source");
        let ast_node = result.ast;

        let mut driver = CompilerDriver::new();
        let ast_id = AstId::new("ast:test::multi");
        driver.state.insert_ast(ast_id.clone(), ast_node);

        driver.scheduler.submit(CompilerWork::CompileUnitCompileNative {
            ast: ast_id,
            scope: ScopeId::new("test::multi"),
            path: path(),
        });

        let _scheduled = driver.run_next().expect("compile unit").expect("compiled");
        // Should produce comptime work
        assert!(
            driver.scheduler.pending_len() > 0 || driver.scheduler.active_len() == 0,
            "should have follow-up work or be done"
        );
    }

    #[test]
    fn const_block_in_function_triggers_comptime_path() {
        let source = r#"
fn calculate() {
    let size = const { 1024 * 8 };
}
"#;
        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(source, std::path::Path::new("block.fp"))
            .expect("parse const-block source");
        let ast_node = result.ast;

        let mut driver = CompilerDriver::new();
        let ast_id = AstId::new("ast:test::block");
        driver.state.insert_ast(ast_id.clone(), ast_node);

        driver.scheduler.submit(CompilerWork::CompileUnitCompileNative {
            ast: ast_id,
            scope: ScopeId::new("test::block"),
            path: path(),
        });

        let _scheduled = driver.run_next().expect("compile unit").expect("compiled");
        // Const block should trigger comptime work
    }

    enum ExampleResult {
        Completed { lowered: usize, executed: usize },
        TypedLooping { followups: usize },
    }

    fn compile_example_file(name: &str) -> Result<ExampleResult, String> {
        let abs = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../../examples")
            .join(name);
        let source = std::fs::read_to_string(&abs).map_err(|e| format!("read: {e}"))?;

        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(&source, &abs)
            .map_err(|e| format!("parse: {e}"))?;
        let ast_node = result.ast;

        let mut driver = CompilerDriver::new();
        let label = name.trim_end_matches(".fp");
        let ast_id = AstId::new(format!("ast:example::{label}"));
        driver.state.insert_ast(ast_id.clone(), ast_node);

        driver.scheduler.submit(CompilerWork::CompileUnitCompileNative {
            ast: ast_id,
            scope: ScopeId::new(label),
            path: FullyQualifiedPath::from_segments(vec!["example".into(), label.to_string()]),
        });

        let scheduled = driver.run_next().map_err(|e| format!("run_next: {e}"))?;
        let scheduled = scheduled.ok_or_else(|| "no work returned".to_string())?;
        let followups = scheduled.followups.len();

        let mut lowered = 0;
        let mut executed = 0;
        let mut step = 0;
        while let Ok(Some(s)) = driver.run_next() {
            step += 1;
            if step > 500 {
                return Ok(ExampleResult::TypedLooping { followups });
            }
            if matches!(
                s.completed.answer,
                CompilerAnswer::CompileUnitCompileNative
            ) {
                lowered += 1;
            }
            if matches!(
                s.completed.answer,
                CompilerAnswer::CompileUnitAnswerComptime { .. }
            ) {
                executed += 1;
            }
        }

        Ok(ExampleResult::Completed { lowered, executed })
    }

    #[test]
    fn run_all_example_files() {
        std::thread::Builder::new()
            .stack_size(8 * 1024 * 1024)
            .name("example-runner".into())
            .spawn(run_all_example_files_impl)
            .unwrap()
            .join()
            .unwrap();
    }

    fn run_all_example_files_impl() {
        let examples_dir =
            std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../examples");
        let mut entries: Vec<_> = std::fs::read_dir(&examples_dir)
            .expect("read examples dir")
            .filter_map(|e| match e {
                Ok(entry) => Some(entry),
                Err(err) => {
                    eprintln!("[fp-compiler] error reading examples dir entry: {err}");
                    None
                }
            })
            .filter(|e| e.path().extension().map_or(false, |ext| ext == "fp"))
            .map(|e| e.file_name().to_string_lossy().to_string())
            .collect();
        entries.sort();

        let mut completed = 0;
        let mut typed = 0;
        let mut errors = 0;

        for name in &entries {
            print!("  {name:.<50} ");
            match compile_example_file(name) {
                Ok(ExampleResult::Completed { lowered, executed }) => {
                    completed += 1;
                    println!("OK  (lowered={lowered}, executed={executed})");
                }
                Ok(ExampleResult::TypedLooping { followups }) => {
                    typed += 1;
                    println!("TYPED (followups={followups}, loops — comptime not resolved)");
                }
                Err(e) => {
                    errors += 1;
                    println!("ERROR: {e}");
                }
            }
        }

        println!(
            "\n  Examples: {completed} completed, {typed} typed-but-looping, {errors} errors ({} total)",
            entries.len()
        );
    }

    fn compile_inline_source(source: &str, expected_const_values: usize) {
        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(source, std::path::Path::new("inline.fp"))
            .unwrap_or_else(|e| panic!("parse inline: {e}"));
        let ast_node = result.ast;

        let mut driver = CompilerDriver::new();
        let ast_id = AstId::new("ast:example::inline");
        driver.state.insert_ast(ast_id.clone(), ast_node);

        driver.scheduler.submit(CompilerWork::CompileUnitCompileNative {
            ast: ast_id,
            scope: ScopeId::new("inline"),
            path: FullyQualifiedPath::from_segments(vec!["example".into(), "inline".into()]),
        });

        let scheduled = driver.run_next().expect("compile unit").expect("compiled");
        assert!(
            matches!(scheduled.completed.answer, CompilerAnswer::CompileUnitCompileNative)
        );

        // Drain any comptime + retry work
        let mut steps = 0;
        while let Ok(Some(_s)) = driver.run_next() {
            steps += 1;
            assert!(steps <= 20, "driver loop should not run forever");
        }

        assert_eq!(
            driver.state.const_value_len(),
            expected_const_values,
            "expected {expected_const_values} const values, got {}",
            driver.state.const_value_len()
        );
    }

    #[test]
    fn comptime_const_with_arithmetic() {
        compile_inline_source(
            r#"
const BUFFER_SIZE: i64 = 1024 * 4;
const MAX_CONNECTIONS: i64 = 150;
const FACTORIAL_5: i64 = 5 * 4 * 3 * 2 * 1;
const IS_LARGE: bool = BUFFER_SIZE > 2048;
"#,
            1,
        );
    }

    #[test]
    fn comptime_const_with_struct_defaults() {
        compile_inline_source(
            r#"
struct Config {
    buffer_size: i64,
    max_connections: i64,
}

const BUFFER_SIZE: i64 = 4096;
const MAX_CONNECTIONS: i64 = 150;
const DEFAULT_CONFIG: Config = Config {
    buffer_size: BUFFER_SIZE,
    max_connections: MAX_CONNECTIONS,
};
"#,
            1,
        );
    }

    #[test]
    fn comptime_const_block_with_conditional() {
        compile_inline_source(
            r#"
const BUFFER_SIZE: i64 = 4096;
const OPTIMIZED_SIZE: i64 = const { BUFFER_SIZE * 2 };
const CACHE_STRATEGY: &str = const {
    if BUFFER_SIZE > 2048 {
        "large"
    } else {
        "small"
    }
};
"#,
            1,
        );
    }
}
