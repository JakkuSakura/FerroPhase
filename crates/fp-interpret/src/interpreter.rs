use super::*;

fn lir_types_compatible(expected: &LirType, actual: &LirType) -> bool {
    match (expected, actual) {
        (
            LirType::Struct {
                fields: expected_fields,
                packed: expected_packed,
                ..
            },
            LirType::Struct {
                fields: actual_fields,
                packed: actual_packed,
                ..
            },
        ) => {
            expected_packed == actual_packed
                && expected_fields.len() == actual_fields.len()
                && expected_fields
                    .iter()
                    .zip(actual_fields)
                    .all(|(expected, actual)| lir_types_compatible(expected, actual))
        }
        (LirType::Ptr(expected), LirType::Ptr(actual)) => lir_types_compatible(expected, actual),
        (LirType::Array(expected, expected_len), LirType::Array(actual, actual_len)) => {
            expected_len == actual_len && lir_types_compatible(expected, actual)
        }
        (LirType::Vector(expected, expected_len), LirType::Vector(actual, actual_len)) => {
            expected_len == actual_len && lir_types_compatible(expected, actual)
        }
        (
            LirType::Function {
                return_type: expected_return,
                param_types: expected_params,
                is_variadic: expected_variadic,
            },
            LirType::Function {
                return_type: actual_return,
                param_types: actual_params,
                is_variadic: actual_variadic,
            },
        ) => {
            expected_variadic == actual_variadic
                && expected_params.len() == actual_params.len()
                && lir_types_compatible(expected_return, actual_return)
                && expected_params
                    .iter()
                    .zip(actual_params)
                    .all(|(expected, actual)| lir_types_compatible(expected, actual))
        }
        _ => expected == actual,
    }
}

#[derive(Clone)]
pub(crate) struct TypedValue {
    pub(super) ty: LirType,
    pub(super) value: Value,
}

pub struct LirInterpreter {
    pub(super) state: ThreadState,
    data_layout: LirDataLayout,
    pub(super) register_values: HashMap<RegisterId, TypedValue>,
    /// Global object handles keyed by name, populated from the LIR
    /// program during run_main / workspace-scoped execution.
    pub(super) global_values: HashMap<String, u64>,
    initialized_globals: std::collections::HashSet<String>,
    /// Optional FFI runtime for calling extern C functions.  Set
    /// before running if the program contains extern declarations.
    ffi: Option<FfiRuntime>,
    /// C signatures of extern functions, keyed by function name.
    /// Populated from LIR functions with `is_declaration = true`.
    extern_sigs: HashMap<String, FfiSignature>,
    /// Tracks the predecessor block ID for correct Phi resolution.
    last_predecessor: Option<BasicBlockId>,
    /// Every package's own LIR, loaded once via `load_program` — function
    /// lookups (`handle_call_named`/`handle_call`/`invoke_function_ref_with_string`)
    /// query this directly on demand (`LirProgram::find_function`/
    /// `find_function_any_package`/`find_function_by_def_id`) instead of a
    /// separately maintained lookup cache.
    program: Option<Rc<fp_core::lir::LirProgram>>,
    host_globals: HostGlobalRegistry,
    host_functions: HostFunctionRegistry,
}

fn json_to_runtime_value(value: serde_json::Value) -> LirResult<Value> {
    match value {
        serde_json::Value::Null => Ok(Value::null()),
        serde_json::Value::Bool(value) => Ok(Value::bool(value)),
        serde_json::Value::Number(value) => value
            .as_i64()
            .map(Value::int)
            .or_else(|| value.as_u64().map(Value::uint))
            .or_else(|| value.as_f64().map(Value::decimal))
            .ok_or_else(|| VmError::Runtime("unsupported JSON number".into())),
        serde_json::Value::String(value) => Ok(Value::string(value)),
        serde_json::Value::Array(values) => Ok(Value::List(ValueList::new(
            values
                .into_iter()
                .map(json_to_runtime_value)
                .collect::<LirResult<Vec<_>>>()?,
        ))),
        serde_json::Value::Object(values) => {
            Ok(Value::Structural(fp_core::ast::ValueStructural::new(
                values
                    .into_iter()
                    .map(|(name, value)| {
                        Ok(fp_core::ast::ValueField::new(
                            fp_core::ast::Ident::new(name),
                            json_to_runtime_value(value)?,
                        ))
                    })
                    .collect::<LirResult<Vec<_>>>()?,
            )))
        }
    }
}

impl LirInterpreter {
    pub fn new() -> Self {
        Self {
            state: ThreadState::new(),
            data_layout: LirDataLayout::new(
                64,
                8,
                vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
            )
            .expect("valid default LIR data layout"),
            register_values: HashMap::new(),
            global_values: HashMap::new(),
            initialized_globals: std::collections::HashSet::new(),
            ffi: FfiRuntime::new().ok(),
            extern_sigs: HashMap::new(),
            last_predecessor: None,
            program: None,
            host_globals: HostGlobalRegistry::new(),
            host_functions: HostFunctionRegistry::new(),
        }
    }

    pub fn set_host_globals(&mut self, registry: HostGlobalRegistry) {
        self.host_globals = registry;
    }

    pub fn host_globals(&self) -> &HostGlobalRegistry {
        &self.host_globals
    }

    pub fn set_host_functions(&mut self, registry: HostFunctionRegistry) {
        self.host_functions = registry;
    }

    pub fn host_functions(&self) -> &HostFunctionRegistry {
        &self.host_functions
    }

    /// Loads every package's own LIR from `program` — the one place
    /// globals get materialized into interpreter memory (`global_values`)
    /// before anything can reference them. Function lookups need no
    /// separate population step: `handle_call`/`handle_call_named`/
    /// `invoke_function_ref_with_string` query `self.program` directly,
    /// on demand, via `LirProgram`'s own lookup APIs.
    pub fn load_program(&mut self, program: Rc<fp_core::lir::LirProgram>) -> LirResult<()> {
        for package in program.packages.values() {
            let blobs: Vec<&LirBlob> = package.blobs.iter().collect();
            self.populate_globals_batch(&blobs)?;
            for blob in &blobs {
                for global in &blob.globals {
                    if !matches!(
                        global.linkage,
                        fp_core::lir::Linkage::External
                            | fp_core::lir::Linkage::AvailableExternally
                    ) {
                        continue;
                    }
                    let host = self.host_globals.get(global.name.as_str()).ok_or_else(|| {
                        VmError::Runtime(format!("unresolved external host global {}", global.name))
                    })?;
                    if !lir_types_compatible(&host.descriptor.ty, &global.ty) {
                        return Err(VmError::TypeMismatch {
                            expected: format!("host global {} has {:?}", global.name, global.ty),
                            found: format!("{:?}", host.descriptor.ty),
                        });
                    }
                    let address =
                        if let Some(address) = self.global_values.get(global.name.as_str()) {
                            *address
                        } else {
                            let address = self.state.mem.heap_alloc(
                                self.data_layout
                                    .size_of(&global.ty)
                                    .map_err(|error| VmError::Runtime(error.to_string()))?,
                                global.alignment.unwrap_or(
                                    self.data_layout
                                        .align_of(&global.ty)
                                        .map_err(|error| VmError::Runtime(error.to_string()))?,
                                ),
                            )?;
                            self.global_values.insert(global.name.to_string(), address);
                            address
                        };
                    let size = self
                        .data_layout
                        .size_of(&global.ty)
                        .map_err(|error| VmError::Runtime(error.to_string()))?;
                    let bytes = unsafe {
                        std::slice::from_raw_parts(host.address() as *const u8, size as usize)
                    };
                    self.state.mem.store_bytes(address, bytes)?;
                }
            }
        }
        self.program = Some(program);
        Ok(())
    }

    /// Convenience: wraps a single flat `LirBlob` as a
    /// one-package program, loads it, and runs its `main` function by
    /// name. Not exposed publicly — a real caller always knows its own
    /// package id and entry `DefId` and should use `run_entrypoint`.
    pub fn run_main(&mut self, program: &LirBlob) -> LirResult<Value> {
        self.run_main_with_package(program, PackageId::new(""))
    }

    fn run_main_with_package(
        &mut self,
        program: &LirBlob,
        package_id: PackageId,
    ) -> LirResult<Value> {
        let lir_program = fp_core::lir::LirProgram::from_single_blob(package_id, program.clone());
        self.load_program(Rc::new(lir_program))?;
        let func = program
            .functions
            .iter()
            .find(|f| f.name.as_str() == "main")
            .ok_or(VmError::Runtime("no entry point".into()))?;
        let func = func.clone();
        self.run_function(&func, &[])
    }

    /// Runs `package_id`'s own `def_id` entry function — `self.program`
    /// (`load_program`) must already hold `package_id`'s own LIR.
    pub fn run_entrypoint(
        &mut self,
        package_id: &PackageId,
        def_id: &fp_core::hir::DefId,
    ) -> LirResult<Value> {
        let function = self
            .program
            .as_ref()
            .and_then(|program| program.find_function_by_def_id(def_id))
            .cloned()
            .ok_or_else(|| VmError::Runtime(format!("entrypoint {def_id} was not emitted")))?;
        let _ = package_id;
        self.run_function(&function, &[])
    }

    /// Read a compile-time result using the result's declared LIR layout.
    ///
    /// The interpreter does not infer semantic values from arbitrary runtime
    /// shapes. A string is reconstructed only from the exact wide-pointer
    /// representation emitted for `&str`.
    pub fn read_typed_const_value(&self, value: Value, ty: &LirType) -> LirResult<Value> {
        let LirType::Struct {
            name: Some(name),
            fields,
            ..
        } = ty
        else {
            return Ok(value);
        };
        if name != "__slice"
            || fields.as_slice() != [LirType::Ptr(Box::new(LirType::I8)), LirType::I64]
        {
            return Ok(value);
        }
        let Value::Tuple(tuple) = value else {
            return Err(VmError::TypeMismatch {
                expected: format!("runtime value for {ty:?}"),
                found: "non-tuple".into(),
            });
        };
        let [Value::Pointer(pointer), length] = tuple.values.as_slice() else {
            return Err(VmError::TypeMismatch {
                expected: format!("pointer and length for {ty:?}"),
                found: format!("{:?}", tuple.values),
            });
        };
        let length = match length {
            Value::Int(length) if length.value >= 0 => length.value as u64,
            Value::UInt(length) => length.value,
            other => {
                return Err(VmError::TypeMismatch {
                    expected: "non-negative slice length".into(),
                    found: format!("{other:?}"),
                });
            }
        };
        let bytes = self.state.mem.load_bytes(pointer.value as u64, length)?;
        let text = String::from_utf8(bytes)
            .map_err(|error| VmError::Runtime(format!("invalid UTF-8 string slice: {error}")))?;
        Ok(Value::string(text))
    }

    pub fn run_function(&mut self, func: &LirFunction, args: &[Value]) -> LirResult<Value> {
        let saved_registers = self.state.regs.gpr.clone();
        let saved_register_values = self.register_values.clone();
        self.state.push_frame(func.name.as_str().to_string());
        self.register_values.clear();
        for local in &func.locals {
            let size = self
                .data_layout
                .size_of(&local.ty)
                .map_err(|error| VmError::Runtime(error.to_string()))?
                .max(8);
            let sp = self.state.regs.sp();
            let addr = self.state.mem.stack_alloc(
                sp,
                size,
                self.data_layout
                    .align_of(&local.ty)
                    .map_err(|error| VmError::Runtime(error.to_string()))?,
            )?;
            self.state.regs.set_sp(addr);
            self.state.set_local_addr(local.id, addr);
        }
        let argument_locals: Vec<&LirLocal> = func
            .locals
            .iter()
            .filter(|local| local.is_argument)
            .collect();
        for (i, arg) in args.iter().enumerate() {
            let reg = i as RegisterId + 1;
            let ty =
                func.signature.params.get(i).ok_or_else(|| {
                    VmError::Runtime(format!("too many arguments for {}", func.name))
                })?;
            let typed = TypedValue {
                ty: ty.clone(),
                value: arg.clone(),
            };
            self.write_typed_register(reg, typed.clone())?;
            if let Some(local) = argument_locals.get(i) {
                self.store_value_at(self.state.local_addr(local.id)?, &typed.ty, &typed.value)?;
            }
        }
        let mut current = func.basic_blocks.first().map(|b| b.id).unwrap_or(0);
        let block_map: HashMap<BasicBlockId, &LirBasicBlock> =
            func.basic_blocks.iter().map(|b| (b.id, b)).collect();
        let result = loop {
            let block = block_map
                .get(&current)
                .ok_or(VmError::Runtime(format!("undefined block {current}")))?;
            for instr in &block.instructions {
                self.exec_instruction(instr).map_err(|error| {
                    VmError::Runtime(format!(
                        "while executing {} block {} instruction {:?}: {error}",
                        func.name, current, instr.kind
                    ))
                })?;
            }
            match &block.terminator {
                LirTerminator::Return(val) => {
                    let v = match val {
                        Some(v) => self.resolve_operand(v)?.value,
                        None => Value::unit(),
                    };
                    break Ok(v);
                }
                LirTerminator::Br(dest) => {
                    self.last_predecessor = Some(current);
                    current = *dest;
                }
                LirTerminator::CondBr {
                    condition,
                    if_true,
                    if_false,
                } => {
                    self.last_predecessor = Some(current);
                    current = if self.resolve_condition(condition)? {
                        *if_true
                    } else {
                        *if_false
                    };
                }
                LirTerminator::Switch {
                    value,
                    default,
                    cases,
                } => {
                    let discriminant = self.resolve_operand(value)?.value;
                    let value = match discriminant {
                        Value::Bool(value) => u64::from(value.value),
                        Value::Int(value) => value.value as u64,
                        Value::UInt(value) => value.value,
                        other => {
                            break Err(VmError::Runtime(format!(
                                "switch discriminant is not an integer: {other:?}"
                            )));
                        }
                    };
                    self.last_predecessor = Some(current);
                    current = cases
                        .iter()
                        .find(|(case, _)| *case == value)
                        .map(|(_, target)| *target)
                        .unwrap_or(*default);
                }
                LirTerminator::Unreachable => break Err(VmError::Runtime("unreachable".into())),
                other => break Err(VmError::Runtime(format!("terminator: {other:?}"))),
            }
        };
        self.state.pop_frame();
        self.state.regs.gpr = saved_registers;
        self.register_values = saved_register_values;
        self.sync_host_globals()?;
        result
    }

    fn sync_host_globals(&mut self) -> LirResult<()> {
        for (name, host) in self.host_globals.iter() {
            if !host.descriptor.mutable {
                continue;
            }
            let Some(address) = self.global_values.get(name).copied() else {
                continue;
            };
            let size = self
                .data_layout
                .size_of(&host.descriptor.ty)
                .map_err(|error| VmError::Runtime(error.to_string()))?;
            let bytes = self.state.mem.load_bytes(address, size)?;
            unsafe {
                std::ptr::copy_nonoverlapping(bytes.as_ptr(), host.address(), bytes.len());
            }
        }
        Ok(())
    }

    fn exec_instruction(&mut self, instr: &LirInstruction) -> LirResult<()> {
        let dst = instr.id;
        let result = match &instr.kind {
            LirInstructionKind::Add(a, b) => self.binop(dst, a, b, |x, y| x.wrapping_add(y)),
            LirInstructionKind::Sub(a, b) => self.binop(dst, a, b, |x, y| x.wrapping_sub(y)),
            LirInstructionKind::Mul(a, b) => self.binop(dst, a, b, |x, y| x.wrapping_mul(y)),
            LirInstructionKind::Div(a, b) => {
                self.binop_div(dst, a, b, instr.result.as_ref().map(|r| &r.ty))
            }
            LirInstructionKind::Rem(a, b) => {
                self.binop_rem(dst, a, b, instr.result.as_ref().map(|r| &r.ty))
            }
            LirInstructionKind::Eq(a, b) => self.compare_eq(dst, a, b, true),
            LirInstructionKind::Ne(a, b) => self.compare_eq(dst, a, b, false),
            LirInstructionKind::Lt(a, b) => self.cmp_signed(dst, a, b, |x, y| x < y),
            LirInstructionKind::Le(a, b) => self.cmp_signed(dst, a, b, |x, y| x <= y),
            LirInstructionKind::Gt(a, b) => self.cmp_signed(dst, a, b, |x, y| x > y),
            LirInstructionKind::Ge(a, b) => self.cmp_signed(dst, a, b, |x, y| x >= y),
            LirInstructionKind::And(a, b) => self.bitop(dst, a, b, |x, y| x & y),
            LirInstructionKind::Or(a, b) => self.bitop(dst, a, b, |x, y| x | y),
            LirInstructionKind::Xor(a, b) => self.bitop(dst, a, b, |x, y| x ^ y),
            LirInstructionKind::Shl(a, b) => self.shift(dst, a, b, |x, s| x.wrapping_shl(s)),
            LirInstructionKind::Shr(a, b) => self.shift(dst, a, b, |x, s| x.wrapping_shr(s)),
            LirInstructionKind::Not(a) => self.bitnot(dst, a),
            LirInstructionKind::Select {
                condition,
                if_true,
                if_false,
            } => {
                let cond = self.resolve_condition(condition)?;
                let chosen = if cond { if_true } else { if_false };
                let value = self.resolve_operand(chosen)?;
                self.write_typed_register(dst, value)
            }
            LirInstructionKind::Phi { incoming } => {
                let predecessor = self
                    .last_predecessor
                    .ok_or_else(|| VmError::Runtime(format!("phi {dst} has no predecessor")))?;
                let (value, _) = incoming
                    .iter()
                    .find(|(_, block)| *block == predecessor)
                    .ok_or_else(|| {
                        VmError::Runtime(format!(
                            "phi {dst} has no incoming value for predecessor {predecessor}"
                        ))
                    })?;
                self.write_typed_register(dst, self.resolve_operand(value)?)
            }
            LirInstructionKind::Alloca { size, alignment } => {
                let raw_size = self.integer_operand(size)?;
                let sp = self.state.regs.sp();
                let addr = self.state.mem.stack_alloc(sp, raw_size, *alignment)?;
                self.state.regs.set_sp(addr);
                let result_ty = self.result_type(instr)?;
                if !matches!(result_ty, LirType::Ptr(_)) {
                    return Err(VmError::TypeMismatch {
                        expected: "alloca pointer result".into(),
                        found: format!("{result_ty:?}"),
                    });
                }
                self.write_typed_result(
                    dst,
                    result_ty,
                    Value::Pointer(fp_core::ast::ValuePointer::managed(addr as i64)),
                )
            }
            LirInstructionKind::Store { value, address, .. } => {
                let ty = value.ty.clone();
                let addr = self.resolve_addr(address)?;
                let runtime_value = self.resolve_operand(value)?.value;
                self.store_value_at(addr, &ty, &runtime_value)
            }
            LirInstructionKind::Load { address, .. } => {
                let addr = self.resolve_addr(address)?;
                let ty = &instr
                    .result
                    .as_ref()
                    .ok_or_else(|| VmError::Runtime("load instruction has no result type".into()))?
                    .ty;
                let value = self.load_value_at(addr, ty)?;
                self.write_typed_result(dst, ty, value)
            }
            LirInstructionKind::GetElementPtr { ptr, indices, .. } => {
                let base = self.pointer_operand(ptr)?;
                let LirType::Ptr(pointee) = ptr.ty.clone() else {
                    return Err(VmError::TypeMismatch {
                        expected: "pointer".into(),
                        found: format!("{:?}", ptr.ty),
                    });
                };
                let elem_size = self
                    .data_layout
                    .size_of(&pointee)
                    .map_err(|error| VmError::Runtime(error.to_string()))?
                    .max(1);
                let mut off = 0u64;
                for (i, idx) in indices.iter().enumerate() {
                    let scale = if i == 0 { elem_size } else { 1 };
                    off = off.wrapping_add(self.integer_operand(idx)?.wrapping_mul(scale));
                }
                self.write_typed_result(
                    dst,
                    instr
                        .result
                        .as_ref()
                        .map(|result| &result.ty)
                        .ok_or_else(|| {
                            VmError::Runtime("gep instruction has no result type".into())
                        })?,
                    Value::Pointer(fp_core::ast::ValuePointer::managed(
                        base.wrapping_add(off) as i64
                    )),
                )
            }
            LirInstructionKind::PtrToInt(v) | LirInstructionKind::IntToPtr(v) => {
                let result_ty = self.result_type(instr)?;
                let value = self.resolve_operand(v)?;
                match &instr.kind {
                    LirInstructionKind::PtrToInt(_) => {
                        let pointer = self.expect_pointer(&value)?;
                        self.write_typed_result(dst, result_ty, Value::int(pointer.value))
                    }
                    LirInstructionKind::IntToPtr(_) => {
                        let integer = self.integer_value(&value)?;
                        self.write_typed_result(
                            dst,
                            result_ty,
                            Value::Pointer(fp_core::ast::ValuePointer::managed(integer as i64)),
                        )
                    }
                    _ => unreachable!(),
                }
            }
            LirInstructionKind::Bitcast(v, dst_ty) => {
                let value = self.resolve_operand(v)?;
                self.write_typed_result(dst, dst_ty, self.bitcast_value(&value, dst_ty)?)
            }
            LirInstructionKind::ZExt(v, dst_ty)
            | LirInstructionKind::Trunc(v, dst_ty)
            | LirInstructionKind::SExt(v, dst_ty)
            | LirInstructionKind::SextOrTrunc(v, dst_ty) => {
                let source = self.resolve_operand(v)?;
                let (source_value, source_bits, _) = self.integer_details(&source)?;
                let (destination_bits, destination_signed) = lir_type_info(dst_ty);
                let result = match &instr.kind {
                    LirInstructionKind::ZExt(..) | LirInstructionKind::Trunc(..) => {
                        mask_integer(source_value, destination_bits)
                    }
                    LirInstructionKind::SExt(..) | LirInstructionKind::SextOrTrunc(..) => {
                        sign_extend_integer(source_value, source_bits, destination_bits)
                    }
                    _ => source_value,
                };
                self.write_typed_result(dst, dst_ty, integer_value(result, destination_signed))
            }
            LirInstructionKind::FPTrunc(v, dst_ty) | LirInstructionKind::FPExt(v, dst_ty) => {
                let value = self.expect_float(&self.resolve_operand(v)?)?;
                self.write_typed_result(dst, dst_ty, Value::decimal(value))
            }
            LirInstructionKind::FPToUI(v, dst_ty) => {
                let val = self.expect_float(&self.resolve_operand(v)?)?;
                #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                {
                    self.write_typed_result(dst, dst_ty, integer_value(val as u64, false))?;
                }
                Ok(())
            }
            LirInstructionKind::FPToSI(v, dst_ty) => {
                let val = self.expect_float(&self.resolve_operand(v)?)?;
                self.write_typed_result(dst, dst_ty, Value::int(val as i64))
            }
            LirInstructionKind::UIToFP(v, dst_ty) => {
                let val = self.integer_value(&self.resolve_operand(v)?)?;
                self.write_typed_result(dst, dst_ty, Value::decimal(val as f64))
            }
            LirInstructionKind::SIToFP(v, dst_ty) => {
                let value = self.resolve_operand(v)?;
                let (_, _, signed) = self.integer_details(&value)?;
                let val = self.integer_value(&value)?;
                let value = if signed {
                    Value::decimal((val as i64) as f64)
                } else {
                    Value::decimal(val as f64)
                };
                self.write_typed_result(dst, dst_ty, value)
            }
            LirInstructionKind::ExtractValue { aggregate, indices } => self.extract_value(
                dst,
                aggregate,
                indices,
                instr.result.as_ref().map(|r| &r.ty),
            ),
            LirInstructionKind::InsertValue {
                aggregate,
                element,
                indices,
            } => self.insert_value(
                dst,
                aggregate,
                element,
                indices,
                instr.result.as_ref().map(|r| &r.ty),
            ),
            LirInstructionKind::Call { function, args, .. } => self.handle_call(
                dst,
                function,
                args,
                instr.result.as_ref().map(|result| &result.ty),
            ),
            LirInstructionKind::IntrinsicCall { kind, format, args } => {
                // Neither carries format-string semantics (their single
                // argument is real payload — text to tokenize, or a
                // `TokenStream` to print — not something to interpolate),
                // so check for them before `render_intrinsic` (which
                // assumes a `{}`-per-argument template and would error
                // confusingly otherwise). Real token-stream support
                // requires an actual tokenizer, which doesn't exist yet —
                // fail loudly here rather than silently return a
                // placeholder unit value for a `TokenStream`/`str` result.
                if matches!(
                    kind,
                    fp_core::lir::LirIntrinsicKind::ProcMacroTokenStreamFromStr
                        | fp_core::lir::LirIntrinsicKind::ProcMacroTokenStreamToString
                ) {
                    return Err(VmError::Runtime(
                        "proc-macro token stream parsing/printing is not yet implemented".into(),
                    ));
                }
                let rendered = self.render_intrinsic(format, args)?;
                match kind {
                    fp_core::lir::LirIntrinsicKind::Print => {
                        print!("{rendered}");
                        return Ok(());
                    }
                    fp_core::lir::LirIntrinsicKind::Println => {
                        println!("{rendered}");
                        return Ok(());
                    }
                    fp_core::lir::LirIntrinsicKind::Format => {
                        // Unlike `Print`/`Println` (side-effecting, no
                        // meaningful return value) `Format` is a real
                        // value-producing expression — e.g. `f"..."` used
                        // as a plain string value, including in an
                        // implicit type-position const block (see
                        // `ast_to_hir`'s `ast::Ty::Expr` arm for
                        // `CallKind::Format`). Write the actual rendered
                        // string instead of discarding it as `unit`.
                        return self.write_typed_result(
                            dst,
                            self.result_type(instr)?,
                            Value::string(rendered),
                        );
                    }
                    fp_core::lir::LirIntrinsicKind::TimeNow => {}
                    fp_core::lir::LirIntrinsicKind::ProcMacroTokenStreamFromStr
                    | fp_core::lir::LirIntrinsicKind::ProcMacroTokenStreamToString => {
                        unreachable!("handled above")
                    }
                }
                self.write_typed_result(dst, self.result_type(instr)?, Value::unit())
            }
            LirInstructionKind::ComptimeOp(op) => match op {
                ComptimeOp::CreateStruct { name } => {
                    let struct_name = self.render_str_argument(name)?;
                    let fields: Vec<fp_core::ast::StructuralField> = vec![];
                    let struct_ty = Ty::Struct(TypeStruct {
                        name: fp_core::ast::Ident::new(struct_name),
                        generics_params: vec![],
                        repr: fp_core::ast::ReprOptions::default(),
                        fields,
                    });
                    let obj = Value::Type(struct_ty);
                    self.write_typed_result(dst, self.result_type(instr)?, obj)
                }
                ComptimeOp::PrimitiveType { name } => {
                    let type_name = self.render_str_argument(name)?;
                    let ty = primitive_type_value_ty(&type_name).ok_or_else(|| {
                        VmError::Runtime(format!(
                            "`{type_name}` is not a supported primitive type value"
                        ))
                    })?;
                    self.write_typed_result(dst, self.result_type(instr)?, Value::Type(ty))
                }
                ComptimeOp::AddField {
                    struct_handle,
                    field_name,
                    field_type,
                } => {
                    let struct_val = self.object_value_operand(struct_handle)?;
                    let field_name_str = self.render_str_argument(field_name)?;
                    let field_ty = match self
                        .resolve_runtime_value(field_type, &LirType::Ptr(Box::new(LirType::I8)))
                    {
                        Ok(Value::Type(ty)) => ty,
                        _ => Ty::Unknown(TypeUnknown),
                    };
                    let mut new_val = struct_val;
                    if let Value::Type(ref mut ty) = new_val {
                        if let Ty::Struct(s) = ty {
                            if !s.fields.iter().any(|f| f.name.as_str() == &field_name_str) {
                                s.fields.push(fp_core::ast::StructuralField::new(
                                    fp_core::ast::Ident::new(field_name_str),
                                    field_ty,
                                ));
                            }
                        }
                    }
                    self.write_typed_result(dst, self.result_type(instr)?, new_val)
                }
                ComptimeOp::IntoType { value } => {
                    let struct_val = self.object_value_operand(value)?;
                    let struct_ty = match struct_val {
                        Value::Type(Ty::Struct(s)) => s.clone(),
                        _ => {
                            return Err(VmError::Runtime(
                                "expected struct type in IntoType".into(),
                            ));
                        }
                    };
                    let wrapped = Value::Type(Ty::Type(TypeType {
                        span: fp_core::span::Span::null(),
                        inner: Some(Box::new(Ty::Struct(struct_ty))),
                    }));
                    self.write_typed_result(dst, self.result_type(instr)?, wrapped)
                }
                ComptimeOp::CompileWarning { message } => {
                    let text = self.render_str_argument(message)?;
                    eprintln!("warning: {text}");
                    self.write_typed_result(dst, self.result_type(instr)?, Value::unit())
                }
                ComptimeOp::CompileError { message } => {
                    let text = self.render_str_argument(message)?;
                    Err(VmError::Runtime(format!("compile_error!: {text}")))
                }
                ComptimeOp::Unionify { function } => {
                    let LirValueKind::Function(LirFunctionRef::Definition(def_id)) = &function.kind
                    else {
                        return Err(VmError::Runtime(
                            "unionify's argument must be a plain function reference".into(),
                        ));
                    };
                    let closure = Value::UnionifyClosure(def_id.clone());
                    self.write_typed_result(dst, self.result_type(instr)?, closure)
                }
            },
            LirInstructionKind::Freeze(value) => {
                let value = self.resolve_operand(value)?;
                self.write_typed_result(dst, self.result_type(instr)?, value.value)
            }
            LirInstructionKind::LandingPad { result_type, .. } => {
                // The interpreter has no unwinding runtime. Match the native
                // backend's landing-pad contract by materializing the zero
                // exception pointer/selector tuple (or the zero value for a
                // backend-specific result type).
                if instr.result.is_none() {
                    return Err(VmError::Runtime(
                        "landing pad instruction has no result type".into(),
                    ));
                }
                self.write_typed_result(dst, result_type, Self::default_value_for_type(result_type))
            }
            LirInstructionKind::InlineAsm { .. } | LirInstructionKind::ExecQuery(_) => {
                Err(VmError::Runtime("unsupported".into()))
            }
            _ => Err(VmError::Runtime("unimplemented".into())),
        };
        result
    }

    fn write_typed_register(&mut self, register: RegisterId, value: TypedValue) -> LirResult<()> {
        let raw = self.encode_value(&value)?;
        self.state.regs.write(Self::register_slot(register), raw);
        self.register_values.insert(register, value);
        Ok(())
    }

    fn encode_value(&mut self, value: &TypedValue) -> LirResult<u64> {
        self.encode_storage_word(value.value.clone(), &value.ty)
    }

    fn decode_storage_value(&self, raw: u64, ty: &LirType) -> LirResult<Value> {
        if Self::is_aggregate_runtime_type(ty) {
            return self
                .state
                .objects
                .get(raw as usize)
                .cloned()
                .ok_or_else(|| VmError::Runtime(format!("dangling aggregate handle {raw}")));
        }
        self.decode_scalar(raw, ty)
    }

    fn register_slot(register: RegisterId) -> RegisterId {
        register.saturating_add(2)
    }

    fn resolve_string_value(&self, val: &LirValue) -> LirResult<String> {
        let value = self.resolve_operand(val)?.value;
        let handle = match value {
            Value::Pointer(pointer) => usize::try_from(pointer.value)
                .map_err(|_| VmError::Runtime("negative string pointer".into()))?,
            other => {
                return Err(VmError::TypeMismatch {
                    expected: "string backing pointer".into(),
                    found: format!("{other:?}"),
                });
            }
        };
        let value =
            self.state.objects.get(handle).ok_or_else(|| {
                VmError::Runtime(format!("string handle {handle} is out of range"))
            })?;
        let bytes = match value {
            Value::String(string) => return Ok(string.value.clone()),
            Value::Bytes(bytes) => bytes.value.as_ref(),
            other => {
                return Err(VmError::Runtime(format!(
                    "expected string backing object, found {other:?}"
                )));
            }
        };
        let bytes = bytes.strip_suffix(&[0]).ok_or_else(|| {
            VmError::Runtime("string backing object is not NUL-terminated".into())
        })?;
        String::from_utf8(bytes.to_vec())
            .map_err(|error| VmError::Runtime(format!("invalid UTF-8 string constant: {error}")))
    }

    /// Renders a `&str`-typed argument to text — unlike `resolve_string_
    /// value` (which only accepts a bare format-string-constant pointer),
    /// this also accepts a real `&str` fat pointer (the `__slice`
    /// `{ptr, len}` pair every ordinary `&str` value/argument uses), the
    /// same shape `render_intrinsic` already unwraps for `println!`-style
    /// arguments — locals first resolve through their stack slot, then the
    /// resulting aggregate is rendered with its declared slice type.
    fn render_str_argument(&self, val: &LirValue) -> LirResult<String> {
        let value = self.resolve_operand(val)?;
        let value = self.read_typed_const_value(value.value, &value.ty)?;
        match value {
            Value::String(value) => Ok(value.value),
            other => Err(VmError::TypeMismatch {
                expected: "string slice".into(),
                found: format!("{other:?}"),
            }),
        }
    }

    fn object_value_operand(&self, val: &LirValue) -> LirResult<Value> {
        let typed = self.resolve_operand(val)?;
        match typed.value {
            Value::Type(value) => Ok(Value::Type(value)),
            Value::Pointer(pointer) => self
                .state
                .objects
                .get(
                    usize::try_from(pointer.value)
                        .map_err(|_| VmError::Runtime("negative managed object pointer".into()))?,
                )
                .cloned()
                .ok_or_else(|| VmError::Runtime("managed object pointer is dangling".into())),
            value => Err(VmError::TypeMismatch {
                expected: "managed object reference".into(),
                found: format!("{value:?}"),
            }),
        }
    }

    fn resolve_constant_address(&self, ty: &LirType, kind: &LirConstantKind) -> LirResult<u64> {
        match kind {
            LirConstantKind::GlobalAddress { global } => self
                .global_values
                .get(global.as_str())
                .copied()
                .ok_or_else(|| VmError::Runtime(format!("missing global {global}"))),
            LirConstantKind::Data(LirConstantData::Integer(integer)) => {
                Ok(integer_constant_value(integer))
            }
            LirConstantKind::Data(LirConstantData::Float(_)) => Err(VmError::Runtime(format!(
                "constant GEP index must be an integer, found {ty:?}"
            ))),
            LirConstantKind::Data(LirConstantData::Bytes(_)) => Err(VmError::Runtime(
                "constant address cannot be formed from byte data".into(),
            )),
            LirConstantKind::Null | LirConstantKind::Undef | LirConstantKind::Poison => Ok(0),
            LirConstantKind::Expr(LirConstantExpr::GetElementPtr { base, indices, .. }) => {
                let base_raw = self.resolve_constant_address(&base.ty, &base.kind)?;
                let LirType::Ptr(pointee) = base.ty.clone() else {
                    return Err(VmError::TypeMismatch {
                        expected: "pointer constant base".into(),
                        found: format!("{:?}", base.ty),
                    });
                };
                let mut current = *pointee;
                let mut offset = 0u64;
                for (index, index_constant) in indices.iter().enumerate() {
                    let index_raw = match &index_constant.kind {
                        LirConstantKind::Data(LirConstantData::Integer(integer)) => {
                            integer_constant_value(integer)
                        }
                        _ => {
                            return Err(VmError::Runtime(format!(
                                "constant GEP index {index} is not an integer"
                            )));
                        }
                    };
                    if index == 0 {
                        let scale = self
                            .data_layout
                            .size_of(&current)
                            .map_err(|error| VmError::Runtime(error.to_string()))?;
                        offset = offset
                            .checked_add(index_raw.checked_mul(scale).ok_or_else(|| {
                                VmError::Runtime("constant GEP offset overflow".into())
                            })?)
                            .ok_or_else(|| {
                                VmError::Runtime("constant GEP offset overflow".into())
                            })?;
                        continue;
                    }
                    match current.clone() {
                        LirType::Array(elem, len) => {
                            if index_raw >= len {
                                return Err(VmError::Runtime(format!(
                                    "constant GEP index {index_raw} out of bounds"
                                )));
                            }
                            let scale = self
                                .data_layout
                                .size_of(&elem)
                                .map_err(|error| VmError::Runtime(error.to_string()))?;
                            offset = offset
                                .checked_add(index_raw.checked_mul(scale).ok_or_else(|| {
                                    VmError::Runtime("constant GEP offset overflow".into())
                                })?)
                                .ok_or_else(|| {
                                    VmError::Runtime("constant GEP offset overflow".into())
                                })?;
                            current = *elem;
                        }
                        LirType::Vector(elem, len) => {
                            if index_raw >= u64::from(len) {
                                return Err(VmError::Runtime(format!(
                                    "constant GEP index {index_raw} out of bounds"
                                )));
                            }
                            let scale = self
                                .data_layout
                                .size_of(&elem)
                                .map_err(|error| VmError::Runtime(error.to_string()))?;
                            offset = offset
                                .checked_add(index_raw.checked_mul(scale).ok_or_else(|| {
                                    VmError::Runtime("constant GEP offset overflow".into())
                                })?)
                                .ok_or_else(|| {
                                    VmError::Runtime("constant GEP offset overflow".into())
                                })?;
                            current = *elem;
                        }
                        LirType::Struct { fields, .. } => {
                            let field = fields.get(index_raw as usize).ok_or_else(|| {
                                VmError::Runtime(format!(
                                    "constant GEP field {index_raw} out of bounds"
                                ))
                            })?;
                            let layout = self
                                .data_layout
                                .struct_layout(&current)
                                .map_err(|error| VmError::Runtime(error.to_string()))?
                                .ok_or_else(|| VmError::Runtime("missing struct layout".into()))?;
                            offset = offset
                                .checked_add(layout.field_offsets[index_raw as usize])
                                .ok_or_else(|| {
                                    VmError::Runtime("constant GEP offset overflow".into())
                                })?;
                            current = field.clone();
                        }
                        other => {
                            return Err(VmError::Runtime(format!(
                                "cannot index constant GEP through {other:?}"
                            )));
                        }
                    }
                }
                base_raw
                    .checked_add(offset)
                    .ok_or_else(|| VmError::Runtime("constant GEP address overflow".into()))
            }
            LirConstantKind::Aggregate(_) | LirConstantKind::FunctionAddress(_) => Err(
                VmError::Runtime("constant address requires a scalar or GEP constant".into()),
            ),
        }
    }

    fn populate_globals_batch(&mut self, programs: &[&LirBlob]) -> LirResult<()> {
        for program in programs {
            self.data_layout = program.data_layout.clone();
            for global in &program.globals {
                if self.global_values.contains_key(global.name.as_str()) {
                    continue;
                }
                let size = self
                    .data_layout
                    .size_of(&global.ty)
                    .map_err(|error| VmError::Runtime(error.to_string()))?;
                let alignment = global.alignment.unwrap_or(
                    self.data_layout
                        .align_of(&global.ty)
                        .map_err(|error| VmError::Runtime(error.to_string()))?,
                );
                let address = self.state.mem.heap_alloc(size, alignment)?;
                self.global_values.insert(global.name.to_string(), address);
            }
        }
        for program in programs {
            for global in &program.globals {
                if self.initialized_globals.contains(global.name.as_str()) {
                    continue;
                }
                let address = *self
                    .global_values
                    .get(global.name.as_str())
                    .ok_or_else(|| VmError::Runtime(format!("missing global {}", global.name)))?;
                let size = self
                    .data_layout
                    .size_of(&global.ty)
                    .map_err(|error| VmError::Runtime(error.to_string()))?;
                if let Some(initializer) = &global.initializer {
                    match &initializer.kind {
                        LirConstantKind::Data(LirConstantData::Bytes(bytes)) => {
                            if bytes.len() as u64 != size {
                                return Err(VmError::Runtime(format!(
                                    "global {} initializer has {} bytes, expected {}",
                                    global.name,
                                    bytes.len(),
                                    size
                                )));
                            }
                            self.state.mem.store_bytes(address, bytes)?;
                        }
                        _ => {
                            let value =
                                self.constant_to_value(&LirValue::constant(initializer.clone()))?;
                            self.store_value_at(address, &global.ty, &value)?;
                        }
                    }
                }
                for relocation in &global.relocations {
                    if relocation.kind != fp_core::lir::LirRelocationKind::Abs64 {
                        return Err(VmError::Runtime(format!(
                            "unsupported global relocation {:?}",
                            relocation.kind
                        )));
                    }
                    let target = match &relocation.target {
                        fp_core::lir::LirRelocationTarget::Global(name) => {
                            *self.global_values.get(name.as_str()).ok_or_else(|| {
                                VmError::Runtime(format!("missing relocation target {name}"))
                            })?
                        }
                        fp_core::lir::LirRelocationTarget::Function(name) => {
                            return Err(VmError::Runtime(format!(
                                "function relocation {} is not a runtime address",
                                name
                            )));
                        }
                    };
                    let value = if relocation.addend >= 0 {
                        target.checked_add(relocation.addend as u64)
                    } else {
                        target.checked_sub(relocation.addend.unsigned_abs())
                    }
                    .ok_or_else(|| VmError::Runtime("global relocation overflow".into()))?;
                    self.state
                        .mem
                        .store_u64(address + relocation.offset, value)?;
                }
                self.initialized_globals.insert(global.name.to_string());
            }
        }
        // Collect C signatures from extern function declarations.
        for program in programs {
            for func in &program.functions {
                if func.is_declaration {
                    let sig = lir_sig_to_ffi(&func.signature);
                    if let Some(host) = self.host_functions.get(func.name.as_str()) {
                        if host.descriptor.signature != func.signature {
                            return Err(VmError::Runtime(format!(
                                "host function '{}' signature does not match its declaration",
                                func.name
                            )));
                        }
                    }
                    self.extern_sigs.insert(func.name.to_string(), sig);
                }
            }
        }
        Ok(())
    }

    /// Inject externally-resolved constant values as globals so that
    /// comptime functions can reference other already-computed consts.
    pub fn inject_globals(&mut self, values: &HashMap<String, Value>) -> LirResult<()> {
        for (name, value) in values {
            let handle = self.state.objects.len() as u64;
            self.state.objects.push(value.clone());
            self.global_values.insert(name.clone(), handle);

            // Executable constants are referenced by their module-qualified
            // symbol in MIR/LIR, while the compile-time store is keyed by
            // source location. Publish the compiler identity alongside the
            // internal evaluation key.
            if let Some(symbol) = Self::qualified_const_symbol(name) {
                if self.global_values.contains_key(&symbol) {
                    return Err(VmError::Runtime(format!(
                        "duplicate injected constant symbol {symbol}"
                    )));
                }
                self.global_values.insert(symbol, handle);
            }
        }
        Ok(())
    }

    fn qualified_const_symbol(key: &str) -> Option<String> {
        let mut parts = key.splitn(4, ':');
        parts.next()?;
        parts.next()?;
        parts.next()?;
        let name = parts.next()?;
        (name.contains("::") && !name.contains(":::")).then(|| name.to_string())
    }

    /// Resolve an address operand — for `LirValue::Local`, returns
    /// the pre-allocated stack address rather than the value at it.
    fn resolve_addr(&self, val: &LirValue) -> LirResult<u64> {
        match &val.kind {
            LirValueKind::Local(id) => Ok(self.state.local_addr(*id)?),
            _ => self.pointer_operand(val),
        }
    }

    fn resolve_operand(&self, val: &LirValue) -> LirResult<TypedValue> {
        match &val.kind {
            LirValueKind::Register(register) => {
                let value = self
                    .register_values
                    .get(register)
                    .cloned()
                    .ok_or(VmError::UndefinedRegister(*register))?;
                if value.ty != val.ty {
                    return Err(VmError::TypeMismatch {
                        expected: format!("{:?}", val.ty),
                        found: format!("{:?}", value.ty),
                    });
                }
                Ok(value)
            }
            LirValueKind::Local(local) | LirValueKind::StackSlot(local) => Ok(TypedValue {
                ty: val.ty.clone(),
                value: self.load_value_at(self.state.local_addr(*local)?, &val.ty)?,
            }),
            LirValueKind::Global(name) => Ok(TypedValue {
                ty: val.ty.clone(),
                value: self.resolve_runtime_value(
                    &LirValue::constant(LirConstant::global_address(val.ty.clone(), name.clone())),
                    &val.ty,
                )?,
            }),
            _ => Ok(TypedValue {
                ty: val.ty.clone(),
                value: self.resolve_runtime_value(val, &val.ty)?,
            }),
        }
    }

    fn resolve_typed_value(&self, val: &LirValue) -> LirResult<TypedValue> {
        self.resolve_operand(val)
    }

    fn resolve_condition(&self, val: &LirValue) -> LirResult<bool> {
        let typed = self.resolve_typed_value(val)?;
        if !matches!(
            typed.ty,
            LirType::I1
                | LirType::I8
                | LirType::I16
                | LirType::I32
                | LirType::I64
                | LirType::I128
                | LirType::Integer(_)
        ) {
            return Err(VmError::TypeMismatch {
                expected: "integer condition".into(),
                found: format!("{:?}", typed.ty),
            });
        }
        match typed.value {
            Value::Bool(value) => Ok(value.value),
            Value::UInt(value) => Ok(value.value != 0),
            Value::Int(value) => Ok(value.value != 0),
            other => Err(VmError::TypeMismatch {
                expected: "boolean condition".into(),
                found: format!("{other:?}"),
            }),
        }
    }

    fn result_type<'a>(&self, instr: &'a LirInstruction) -> LirResult<&'a LirType> {
        instr
            .result
            .as_ref()
            .map(|result| &result.ty)
            .ok_or_else(|| VmError::Runtime("instruction has no result type".into()))
    }

    fn write_typed_result(&mut self, dst: RegisterId, ty: &LirType, value: Value) -> LirResult<()> {
        self.write_typed_register(
            dst,
            TypedValue {
                ty: ty.clone(),
                value,
            },
        )
    }

    fn integer_value(&self, value: &TypedValue) -> LirResult<u64> {
        match &value.value {
            Value::Bool(value) => Ok(u64::from(value.value)),
            Value::Int(value) => Ok(value.value as u64),
            Value::UInt(value) => Ok(value.value),
            ref value => Err(VmError::TypeMismatch {
                expected: "integer value".into(),
                found: format!("{value:?}"),
            }),
        }
    }

    fn integer_details(&self, value: &TypedValue) -> LirResult<(u64, u32, bool)> {
        if !is_integer_type(&value.ty) {
            return Err(VmError::TypeMismatch {
                expected: "integer LIR type".into(),
                found: format!("{:?}", value.ty),
            });
        }
        let (bits, signed) = lir_type_info(&value.ty);
        Ok((self.integer_value(value)?, bits, signed))
    }

    fn integer_operand(&self, value: &LirValue) -> LirResult<u64> {
        self.integer_value(&self.resolve_operand(value)?)
    }

    fn pointer_operand(&self, value: &LirValue) -> LirResult<u64> {
        self.expect_pointer(&self.resolve_operand(value)?)
            .map(|pointer| pointer.value as u64)
    }

    fn expect_pointer(&self, value: &TypedValue) -> LirResult<fp_core::ast::ValuePointer> {
        if !matches!(value.ty, LirType::Ptr(_)) {
            return Err(VmError::TypeMismatch {
                expected: "pointer LIR type".into(),
                found: format!("{:?}", value.ty),
            });
        }
        match &value.value {
            Value::Pointer(pointer) => Ok(*pointer),
            Value::Null(_) => Ok(fp_core::ast::ValuePointer::managed(0)),
            ref value => Err(VmError::TypeMismatch {
                expected: "pointer value".into(),
                found: format!("{value:?}"),
            }),
        }
    }

    fn expect_float(&self, value: &TypedValue) -> LirResult<f64> {
        if !matches!(value.ty, LirType::F32 | LirType::F64) {
            return Err(VmError::TypeMismatch {
                expected: "floating-point LIR type".into(),
                found: format!("{:?}", value.ty),
            });
        }
        match &value.value {
            Value::Decimal(value) => Ok(value.value),
            ref value => Err(VmError::TypeMismatch {
                expected: "floating-point value".into(),
                found: format!("{value:?}"),
            }),
        }
    }

    fn bitcast_value(&self, value: &TypedValue, destination: &LirType) -> LirResult<Value> {
        let (source_bits, _) = lir_type_info(&value.ty);
        let (destination_bits, destination_signed) = lir_type_info(destination);
        if source_bits != destination_bits {
            return Err(VmError::TypeMismatch {
                expected: format!("bitcast-compatible type of {source_bits} bits"),
                found: format!("{destination:?}"),
            });
        }
        match (&value.value, destination) {
            (Value::Decimal(value), LirType::I32) => Ok(integer_value(
                value.value.to_bits() as u64,
                destination_signed,
            )),
            (Value::Decimal(value), LirType::I64) => {
                Ok(integer_value(value.value.to_bits(), destination_signed))
            }
            (Value::Int(value), LirType::F64) => {
                Ok(Value::decimal(f64::from_bits(value.value as u64)))
            }
            (Value::UInt(value), LirType::F64) => Ok(Value::decimal(f64::from_bits(value.value))),
            (Value::Pointer(value), LirType::I64) => {
                Ok(integer_value(value.value as u64, destination_signed))
            }
            (Value::Int(value), LirType::Ptr(_)) => Ok(Value::Pointer(
                fp_core::ast::ValuePointer::managed(value.value as i64),
            )),
            (Value::UInt(value), LirType::Ptr(_)) => Ok(Value::Pointer(
                fp_core::ast::ValuePointer::managed(value.value as i64),
            )),
            (Value::Pointer(value), LirType::Ptr(_)) => Ok(Value::Pointer(*value)),
            (_, _) if value.ty == *destination => Ok(value.value.clone()),
            _ => Err(VmError::Runtime(format!(
                "unsupported bitcast from {:?} to {destination:?}",
                value.ty
            ))),
        }
    }

    fn insert_value(
        &mut self,
        dst: u32,
        aggregate: &LirValue,
        element: &LirValue,
        indices: &[u32],
        aggregate_ty: Option<&LirType>,
    ) -> LirResult<()> {
        let aggregate_ty = aggregate_ty.cloned().ok_or_else(|| {
            VmError::Runtime("insert_value instruction has no result type".into())
        })?;
        let element_ty = self.aggregate_element_type(&aggregate_ty, indices)?;
        // If the aggregate is Undef (initial state from handle_aggregate),
        // resolve it to a default aggregate value based on the type.
        let mut aggregate_value = match &aggregate.kind {
            LirValueKind::Constant(LirConstantKind::Undef) => {
                Self::default_value_for_type(&aggregate_ty)
            }
            _ => self.resolve_aggregate_value(aggregate, &aggregate_ty)?,
        };
        let element_value = self.resolve_runtime_value(element, &element_ty)?;
        Self::aggregate_insert(&mut aggregate_value, &element_ty, indices, element_value)?;
        self.write_typed_result(dst, &aggregate_ty, aggregate_value)
    }

    fn extract_value(
        &mut self,
        dst: u32,
        aggregate: &LirValue,
        indices: &[u32],
        result_ty: Option<&LirType>,
    ) -> LirResult<()> {
        let aggregate_ty = aggregate.ty.clone();
        let aggregate_value = self.resolve_aggregate_value(aggregate, &aggregate_ty)?;
        let result_ty = result_ty.ok_or_else(|| {
            VmError::Runtime("extract_value instruction has no result type".into())
        })?;
        let element_ty = self.aggregate_element_type(&aggregate_ty, indices)?;
        if element_ty != *result_ty {
            return Err(VmError::TypeMismatch {
                expected: format!("{:?}", element_ty),
                found: format!("{:?}", result_ty),
            });
        }
        let value = Self::aggregate_extract(&aggregate_value, indices)?;
        self.store_runtime_value(dst, result_ty, value)
    }

    fn store_runtime_value(&mut self, dst: u32, ty: &LirType, value: Value) -> LirResult<()> {
        self.write_typed_result(dst, ty, value)
    }

    pub(super) fn resolve_runtime_value(&self, val: &LirValue, ty: &LirType) -> LirResult<Value> {
        if let LirValueKind::Register(register) = &val.kind {
            let value = self
                .register_values
                .get(register)
                .ok_or(VmError::UndefinedRegister(*register))?;
            if value.ty != *ty || val.ty != *ty {
                return Err(VmError::TypeMismatch {
                    expected: format!("{ty:?}"),
                    found: format!("register {:?} has {:?}", val.ty, value.ty),
                });
            }
            return Ok(value.value.clone());
        }
        if Self::is_aggregate_runtime_type(ty) {
            return self.resolve_aggregate_value(val, ty);
        }
        if matches!(ty, LirType::Ptr(_)) {
            return match &val.kind {
                LirValueKind::Constant(LirConstantKind::Null) => Ok(Value::null()),
                LirValueKind::Constant(kind) => {
                    Ok(Value::Pointer(fp_core::ast::ValuePointer::managed(
                        self.resolve_constant_address(ty, kind)? as i64,
                    )))
                }
                _ => Ok(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    self.pointer_operand(val)? as i64,
                ))),
            };
        }
        match &val.kind {
            LirValueKind::Constant(_) => self.constant_to_value(val),
            LirValueKind::Local(id) | LirValueKind::StackSlot(id) => {
                self.load_value_at(self.state.local_addr(*id)?, ty)
            }
            LirValueKind::Global(name) => self.constant_to_value(&LirValue::constant(
                LirConstant::global_address(ty.clone(), name.clone()),
            )),
            LirValueKind::Register(register) => self
                .register_values
                .get(register)
                .map(|value| value.value.clone())
                .ok_or(VmError::UndefinedRegister(*register)),
            LirValueKind::Function(_) => {
                Err(VmError::Runtime("function is not a runtime value".into()))
            }
        }
    }

    fn resolve_aggregate_value(&self, val: &LirValue, ty: &LirType) -> LirResult<Value> {
        match &val.kind {
            LirValueKind::Register(id) => {
                if let Some(value) = self.register_values.get(id) {
                    if value.ty != *ty || !Self::is_aggregate_runtime_type(ty) {
                        return Err(VmError::TypeMismatch {
                            expected: format!("aggregate {ty:?}"),
                            found: format!("{:?}", value.ty),
                        });
                    }
                    return Ok(value.value.clone());
                }
                Err(VmError::UndefinedRegister(*id))
            }
            LirValueKind::Constant(LirConstantKind::Undef)
            | LirValueKind::Constant(LirConstantKind::Null) => Ok(Self::default_value_for_type(ty)),
            LirValueKind::Global(name)
            | LirValueKind::Constant(LirConstantKind::GlobalAddress { global: name }) => {
                let address = self
                    .global_values
                    .get(name.as_str())
                    .copied()
                    .ok_or_else(|| VmError::Runtime(format!("missing global {name}")))?;
                self.load_value_at(address, ty)
            }
            LirValueKind::Constant(_) => self.constant_to_value(val),
            _ => Err(VmError::Runtime(format!(
                "expected aggregate value for {ty:?}, found {val:?}"
            ))),
        }
    }

    pub(super) fn constant_to_value(&self, constant: &LirValue) -> LirResult<Value> {
        let value = match &constant.kind {
            LirValueKind::Constant(LirConstantKind::Data(LirConstantData::Integer(integer))) => {
                match integer {
                    LirInteger::I1(value) => Value::bool(*value),
                    LirInteger::I8(value) => {
                        integer_value(u64::from(*value), lir_type_info(&constant.ty).1)
                    }
                    LirInteger::I16(value) => {
                        integer_value(u64::from(*value), lir_type_info(&constant.ty).1)
                    }
                    LirInteger::I32(value) => {
                        integer_value(u64::from(*value), lir_type_info(&constant.ty).1)
                    }
                    LirInteger::I64(value) => integer_value(*value, lir_type_info(&constant.ty).1),
                    LirInteger::I128(_) | LirInteger::Arbitrary(_) => {
                        todo!("interpreter conversion for wide integer constants")
                    }
                }
            }
            LirValueKind::Constant(LirConstantKind::Data(LirConstantData::Float(float))) => {
                match float {
                    LirFloat::F32(value) => Value::decimal(f32::from_bits(*value) as f64),
                    LirFloat::F64(value) => Value::decimal(f64::from_bits(*value)),
                }
            }
            LirValueKind::Constant(LirConstantKind::Data(LirConstantData::Bytes(bytes))) => {
                Value::Bytes(fp_core::ast::ValueBytes::from(bytes.as_slice()))
            }
            LirValueKind::Constant(LirConstantKind::Aggregate(aggregate)) => match aggregate {
                LirConstantAggregate::Array(values) | LirConstantAggregate::Vector(values) => {
                    Value::List(ValueList::new(
                        values
                            .iter()
                            .map(|value| self.constant_to_value(&LirValue::constant(value.clone())))
                            .collect::<LirResult<Vec<_>>>()?,
                    ))
                }
                LirConstantAggregate::Struct(values) => Value::Tuple(ValueTuple::new(
                    values
                        .iter()
                        .map(|value| self.constant_to_value(&LirValue::constant(value.clone())))
                        .collect::<LirResult<Vec<_>>>()?,
                )),
            },
            LirValueKind::Constant(LirConstantKind::Null) => Value::null(),
            LirValueKind::Constant(LirConstantKind::Undef) => {
                Self::default_value_for_type(&constant.ty)
            }
            LirValueKind::Constant(LirConstantKind::GlobalAddress { global }) => {
                let address = self
                    .global_values
                    .get(global.as_str())
                    .copied()
                    .ok_or_else(|| VmError::Runtime(format!("missing global {global}")))?;
                if matches!(constant.ty, LirType::Ptr(_)) {
                    Value::Pointer(fp_core::ast::ValuePointer::managed(address as i64))
                } else {
                    self.load_value_at(address, &constant.ty)?
                }
            }
            LirValueKind::Constant(LirConstantKind::Expr(LirConstantExpr::GetElementPtr {
                ..
            })) => Value::Pointer(fp_core::ast::ValuePointer::managed(
                self.resolve_constant_address(
                    &constant.ty,
                    match &constant.kind {
                        LirValueKind::Constant(kind) => kind,
                        _ => unreachable!(),
                    },
                )? as i64,
            )),
            LirValueKind::Constant(LirConstantKind::FunctionAddress(_)) => Value::uint(0),
            _ => return Err(VmError::Runtime(format!("not a constant: {constant:?}"))),
        };
        Ok(value)
    }

    fn encode_storage_word(&mut self, value: Value, ty: &LirType) -> LirResult<u64> {
        if Self::is_aggregate_runtime_type(ty) {
            let handle = self.state.objects.len() as u64;
            self.state.objects.push(value);
            return Ok(handle);
        }
        if matches!(ty, LirType::Ptr(_)) {
            return match value {
                Value::Pointer(pointer) => Ok(pointer.value as u64),
                Value::Null(_) => Ok(0),
                value @ (Value::Type(_) | Value::String(_) | Value::Bytes(_)) => {
                    let handle = self.state.objects.len() as u64;
                    self.state.objects.push(value);
                    Ok(handle)
                }
                other => Err(VmError::TypeMismatch {
                    expected: "pointer value".into(),
                    found: format!("{other:?}"),
                }),
            };
        }
        match value {
            Value::Int(value) => Ok(value.value as u64),
            Value::UInt(value) => Ok(value.value),
            Value::Bool(value) => Ok(u64::from(value.value)),
            Value::Decimal(value) => Ok(value.value.to_bits()),
            Value::Null(_) | Value::Undefined(_) | Value::Unit(_) => Ok(0),
            other => Err(VmError::TypeMismatch {
                expected: format!("scalar value for {ty:?}"),
                found: format!("{other:?}"),
            }),
        }
    }

    fn default_value_for_type(ty: &LirType) -> Value {
        match ty {
            LirType::Array(elem, len) => Value::List(ValueList::new(
                (0..*len)
                    .map(|_| Self::default_value_for_type(elem))
                    .collect::<Vec<_>>(),
            )),
            LirType::Vector(elem, len) => Value::List(ValueList::new(
                (0..*len)
                    .map(|_| Self::default_value_for_type(elem))
                    .collect::<Vec<_>>(),
            )),
            LirType::Struct { fields, .. } => Value::Tuple(ValueTuple::new(
                fields
                    .iter()
                    .map(Self::default_value_for_type)
                    .collect::<Vec<_>>(),
            )),
            LirType::Ptr(_) => Value::uint(0),
            LirType::Void => Value::unit(),
            _ => {
                let (bits, signed) = lir_type_info(ty);
                decode_integer(0, signed, bits)
            }
        }
    }

    fn aggregate_element_type(&self, ty: &LirType, indices: &[u32]) -> LirResult<LirType> {
        let mut current = ty.clone();
        for index in indices {
            current = match current {
                LirType::Struct { fields, .. } => {
                    fields
                        .get(*index as usize)
                        .cloned()
                        .ok_or(VmError::Runtime(format!(
                            "struct index {index} out of range"
                        )))?
                }
                LirType::Array(elem, len) => {
                    if (*index as u64) >= len {
                        return Err(VmError::Runtime(format!(
                            "array index {index} out of range"
                        )));
                    }
                    *elem
                }
                LirType::Vector(elem, len) => {
                    if *index >= len {
                        return Err(VmError::Runtime(format!(
                            "vector index {index} out of range"
                        )));
                    }
                    *elem
                }
                other => {
                    return Err(VmError::Runtime(format!(
                        "cannot index into non-aggregate type {other:?}"
                    )));
                }
            };
        }
        Ok(current)
    }

    fn aggregate_insert(
        aggregate: &mut Value,
        element_ty: &LirType,
        indices: &[u32],
        element: Value,
    ) -> LirResult<()> {
        let Some((first, rest)) = indices.split_first() else {
            *aggregate = element;
            return Ok(());
        };
        match aggregate {
            Value::List(list) => {
                let idx = *first as usize;
                let slot = list.values.get_mut(idx).ok_or_else(|| {
                    VmError::Runtime(format!("aggregate index {} out of range", first))
                })?;
                if rest.is_empty() {
                    *slot = element;
                    return Ok(());
                }
                if matches!(slot, Value::Null(_) | Value::Undefined(_)) {
                    *slot = Self::default_value_for_type(element_ty);
                }
                Self::aggregate_insert(slot, element_ty, rest, element)
            }
            Value::Tuple(tuple) => {
                let idx = *first as usize;
                let slot = tuple.values.get_mut(idx).ok_or_else(|| {
                    VmError::Runtime(format!("aggregate index {} out of range", first))
                })?;
                if rest.is_empty() {
                    *slot = element;
                    return Ok(());
                }
                if matches!(slot, Value::Null(_) | Value::Undefined(_)) {
                    *slot = Self::default_value_for_type(element_ty);
                }
                Self::aggregate_insert(slot, element_ty, rest, element)
            }
            Value::Struct(vs) => {
                let idx = *first as usize;
                if idx >= vs.structural.fields.len() {
                    return Err(VmError::Runtime(format!(
                        "InsertValue index {} out of bounds for struct {} ({} fields)",
                        first,
                        vs.ty.name,
                        vs.structural.fields.len()
                    )));
                }
                let slot = &mut vs.structural.fields[idx].value;
                if rest.is_empty() {
                    *slot = element;
                    return Ok(());
                }
                Self::aggregate_insert(slot, element_ty, rest, element)
            }
            Value::Structural(vs) => {
                let idx = *first as usize;
                if idx >= vs.fields.len() {
                    return Err(VmError::Runtime(format!(
                        "InsertValue index {} out of bounds for structural ({} fields)",
                        first,
                        vs.fields.len()
                    )));
                }
                let slot = &mut vs.fields[idx].value;
                if rest.is_empty() {
                    *slot = element;
                    return Ok(());
                }
                Self::aggregate_insert(slot, element_ty, rest, element)
            }
            other => Err(VmError::Runtime(format!(
                "InsertValue expects aggregate (List, Tuple, Struct, Structural), found {other:?}"
            ))),
        }
    }

    fn aggregate_extract(aggregate: &Value, indices: &[u32]) -> LirResult<Value> {
        let mut current = aggregate;
        for index in indices {
            current = match current {
                Value::List(list) => list.values.get(*index as usize).ok_or_else(|| {
                    VmError::Runtime(format!("aggregate index {} out of range", index))
                })?,
                Value::Tuple(tuple) => tuple.values.get(*index as usize).ok_or_else(|| {
                    VmError::Runtime(format!("aggregate index {} out of range", index))
                })?,
                Value::Struct(vs) => {
                    let idx = *index as usize;
                    vs.structural
                        .fields
                        .get(idx)
                        .map(|f| &f.value)
                        .ok_or_else(|| {
                            VmError::Runtime(format!(
                                "ExtractValue index {} out of bounds for struct {} ({} fields)",
                                index,
                                vs.ty.name,
                                vs.structural.fields.len()
                            ))
                        })?
                }
                Value::Structural(vs) => {
                    let idx = *index as usize;
                    vs.fields.get(idx).map(|f| &f.value).ok_or_else(|| {
                        VmError::Runtime(format!(
                            "ExtractValue index {} out of bounds for structural ({} fields)",
                            index,
                            vs.fields.len()
                        ))
                    })?
                }
                other => {
                    return Err(VmError::Runtime(format!(
                        "ExtractValue expects aggregate (List, Tuple, Struct, Structural), found {other:?}"
                    )));
                }
            };
        }
        Ok(current.clone())
    }

    fn is_aggregate_runtime_type(ty: &LirType) -> bool {
        matches!(
            ty,
            LirType::Struct { .. } | LirType::Array(..) | LirType::Vector(..)
        )
    }

    fn binop(
        &mut self,
        dst: u32,
        a: &LirValue,
        b: &LirValue,
        op: fn(i64, i64) -> i64,
    ) -> LirResult<()> {
        let lhs = self.resolve_operand(a)?;
        let rhs = self.resolve_operand(b)?;
        self.require_same_type(&lhs, &rhs)?;
        let (_, _, signed) = self.integer_details(&lhs)?;
        let result = if signed {
            integer_value(
                op(
                    self.integer_value(&lhs)? as i64,
                    self.integer_value(&rhs)? as i64,
                ) as u64,
                signed,
            )
        } else {
            integer_value(
                op(
                    self.integer_value(&lhs)? as i64,
                    self.integer_value(&rhs)? as i64,
                ) as u64,
                signed,
            )
        };
        self.write_typed_result(dst, &lhs.ty, result)
    }

    fn compare_eq(&mut self, dst: u32, a: &LirValue, b: &LirValue, equal: bool) -> LirResult<()> {
        let lhs = self.resolve_operand(a)?;
        let rhs = self.resolve_operand(b)?;
        self.require_same_type(&lhs, &rhs)?;
        let result = self.integer_value(&lhs)? == self.integer_value(&rhs)?;
        if !is_integer_type(&lhs.ty) {
            return Err(VmError::TypeMismatch {
                expected: "integer comparison operand".into(),
                found: format!("{:?}", lhs.ty),
            });
        }
        self.write_typed_result(
            dst,
            &LirType::I1,
            Value::bool(if equal { result } else { !result }),
        )
    }

    fn bitop(
        &mut self,
        dst: u32,
        a: &LirValue,
        b: &LirValue,
        op: fn(u64, u64) -> u64,
    ) -> LirResult<()> {
        let lhs = self.resolve_operand(a)?;
        let rhs = self.resolve_operand(b)?;
        self.require_same_type(&lhs, &rhs)?;
        let (value, _, signed) = self.integer_details(&lhs)?;
        let result = op(value, self.integer_value(&rhs)?);
        self.write_typed_result(dst, &lhs.ty, integer_value(result, signed))
    }

    fn bitnot(&mut self, dst: u32, a: &LirValue) -> LirResult<()> {
        let typed = self.resolve_operand(a)?;
        let (value, _, signed) = self.integer_details(&typed)?;
        self.write_typed_result(dst, &typed.ty, integer_value(!value, signed))
    }

    fn cmp_signed(
        &mut self,
        dst: u32,
        a: &LirValue,
        b: &LirValue,
        op: fn(i64, i64) -> bool,
    ) -> LirResult<()> {
        let lhs = self.resolve_operand(a)?;
        let rhs = self.resolve_operand(b)?;
        self.require_same_type(&lhs, &rhs)?;
        let (_, _, signed) = self.integer_details(&lhs)?;
        let result = if signed {
            op(
                self.integer_value(&lhs)? as i64,
                self.integer_value(&rhs)? as i64,
            )
        } else {
            op(
                self.integer_value(&lhs)? as i64,
                self.integer_value(&rhs)? as i64,
            )
        };
        self.write_typed_result(dst, &LirType::I1, Value::bool(result))
    }

    fn shift(
        &mut self,
        dst: u32,
        a: &LirValue,
        b: &LirValue,
        op: fn(u64, u32) -> u64,
    ) -> LirResult<()> {
        let lhs = self.resolve_operand(a)?;
        let rhs = self.resolve_operand(b)?;
        self.require_same_type(&lhs, &rhs)?;
        let (_, _, signed) = self.integer_details(&lhs)?;
        let result = op(self.integer_value(&lhs)?, self.integer_value(&rhs)? as u32);
        self.write_typed_result(dst, &lhs.ty, integer_value(result, signed))
    }

    fn require_same_type(&self, lhs: &TypedValue, rhs: &TypedValue) -> LirResult<()> {
        if lhs.ty == rhs.ty {
            return Ok(());
        }
        Err(VmError::TypeMismatch {
            expected: format!("{:?}", lhs.ty),
            found: format!("{:?}", rhs.ty),
        })
    }

    fn binop_div(
        &mut self,
        dst: u32,
        a: &LirValue,
        b: &LirValue,
        ty: Option<&LirType>,
    ) -> LirResult<()> {
        let lhs = self.resolve_operand(a)?;
        let rhs = self.resolve_operand(b)?;
        self.require_same_type(&lhs, &rhs)?;
        let rhs_value = self.integer_value(&rhs)?;
        if rhs_value == 0 {
            return Err(VmError::DivisionByZero);
        }
        let (_, signed) = lir_type_info(
            ty.ok_or_else(|| VmError::Runtime("division has no result type".into()))?,
        );
        let result = if signed {
            (self.integer_value(&lhs)? as i64).wrapping_div(rhs_value as i64) as u64
        } else {
            self.integer_value(&lhs)?.wrapping_div(rhs_value)
        };
        self.write_typed_result(dst, ty.unwrap(), integer_value(result, signed))
    }

    fn binop_rem(
        &mut self,
        dst: u32,
        a: &LirValue,
        b: &LirValue,
        ty: Option<&LirType>,
    ) -> LirResult<()> {
        let lhs = self.resolve_operand(a)?;
        let rhs = self.resolve_operand(b)?;
        self.require_same_type(&lhs, &rhs)?;
        let rhs_value = self.integer_value(&rhs)?;
        if rhs_value == 0 {
            return Err(VmError::DivisionByZero);
        }
        let ty = ty.ok_or_else(|| VmError::Runtime("remainder has no result type".into()))?;
        let (_, signed) = lir_type_info(ty);
        let result = if signed {
            (self.integer_value(&lhs)? as i64).wrapping_rem(rhs_value as i64) as u64
        } else {
            self.integer_value(&lhs)?.wrapping_rem(rhs_value)
        };
        self.write_typed_result(dst, ty, integer_value(result, signed))
    }

    /// Handles calling the special `unionify(f)` closure returned by the
    /// reflection intrinsics. Generic named function values are resolved
    /// earlier by `handle_call` from their managed string handle.
    pub(super) fn handle_unionify_closure_call(
        &mut self,
        dst: u32,
        function: &LirValue,
        args: &[LirValue],
        result_ty: Option<&LirType>,
    ) -> LirResult<()> {
        let callee = self.resolve_operand(function)?;
        let Value::UnionifyClosure(def_id) = callee.value else {
            return Err(VmError::Runtime("indirect call".into()));
        };
        let [type_arg] = args else {
            return Err(VmError::Runtime(
                "unionify's closure takes exactly one argument".into(),
            ));
        };
        let type_val = self.object_value_operand(type_arg)?;
        let Value::Type(reflected_ty) = type_val else {
            return Err(VmError::TypeMismatch {
                expected: "type value".into(),
                found: format!("{type_val:?}"),
            });
        };
        let members = collect_literal_union_members(&reflected_ty).ok_or_else(|| {
            VmError::Runtime(
                "unionify's closure argument must be a string literal type or a union of \
                 string literal types"
                    .into(),
            )
        })?;
        let function_ref = LirFunctionRef::Definition(def_id);
        let mut transformed = Vec::with_capacity(members.len());
        for member in members {
            transformed.push(self.invoke_function_ref_with_string(&function_ref, &member)?);
        }
        let result = Value::Type(build_literal_union(transformed));
        let Some(ty) = result_ty else {
            return Ok(());
        };
        self.write_typed_result(dst, ty, result)
    }

    /// Invokes a plain (non-indirect) function reference with a single
    /// `&str` argument and returns its `String` result — used by
    /// `handle_unionify_closure_call` to apply a function to each member of
    /// a reflected union type. Shares `handle_call`'s `Definition` dispatch
    /// logic, but works directly on `Value`s (no destination register /
    /// `LirType` signature check) since the caller already knows the
    /// expected shape.
    fn invoke_function_ref_with_string(
        &mut self,
        function_ref: &LirFunctionRef,
        arg: &str,
    ) -> LirResult<String> {
        let LirFunctionRef::Definition(def_id) = function_ref else {
            return Err(VmError::Runtime(
                "unionify only supports functions resolved to a definition".into(),
            ));
        };
        let function = self
            .program
            .as_ref()
            .and_then(|program| program.find_function_by_def_id(def_id))
            .cloned()
            .ok_or(VmError::Runtime(format!(
                "missing function definition {def_id}"
            )))?;
        // `&str` (`Ptr<I8>`) parameters are managed-object pointers, not
        // bare `Value::String`s (see `render_typed_value`'s `Ptr<I8>` arm) —
        // push the argument onto the object heap and pass a pointer to it,
        // matching how every other `&str`-typed value flows through this
        // interpreter (e.g. the `"format"`/`"str_alloc"` builtins).
        let handle = self.state.objects.len() as u64;
        self.state.objects.push(Value::string(arg.to_string()));
        let arg_value = Value::Pointer(fp_core::ast::ValuePointer::managed(handle as i64));
        let result = self.run_function(&function, &[arg_value])?;
        let result_value = match result {
            Value::Pointer(pointer) => {
                let handle = usize::try_from(pointer.value)
                    .map_err(|_| VmError::Runtime("negative string pointer".into()))?;
                self.state.objects.get(handle).cloned().ok_or_else(|| {
                    VmError::Runtime(format!("string handle {handle} is out of range"))
                })?
            }
            other => other,
        };
        match result_value {
            Value::String(s) => Ok(s.value),
            other => Err(VmError::TypeMismatch {
                expected: "string".into(),
                found: format!("{other:?}"),
            }),
        }
    }

    fn handle_call(
        &mut self,
        dst: u32,
        function: &LirValue,
        args: &[LirValue],
        result_ty: Option<&LirType>,
    ) -> LirResult<()> {
        let LirValueKind::Function(function_ref) = &function.kind else {
            let callee = self.resolve_operand(function)?;
            if let Value::Pointer(pointer) = callee.value {
                let handle = usize::try_from(pointer.value)
                    .map_err(|_| VmError::Runtime("negative function handle".into()))?;
                if let Some(Value::String(name)) = self.state.objects.get(handle).cloned() {
                    return self.handle_call_named(dst, &name.value, args, None, None, result_ty);
                }
            }
            return self.handle_unionify_closure_call(dst, function, args, result_ty);
        };
        match function_ref {
            LirFunctionRef::Name(name) => {
                self.handle_call_named(dst, name.as_str(), args, None, None, result_ty)
            }
            LirFunctionRef::Package { package_id, name } => self.handle_call_named(
                dst,
                name.as_str(),
                args,
                Some(package_id.clone()),
                None,
                result_ty,
            ),
            LirFunctionRef::Definition(def_id) => {
                let function = self
                    .program
                    .as_ref()
                    .and_then(|program| program.find_function_by_def_id(def_id))
                    .cloned()
                    .ok_or(VmError::Runtime(format!(
                        "missing function definition {def_id}"
                    )))?;
                let resolved_args: Vec<Value> = args
                    .iter()
                    .enumerate()
                    .map(|(index, arg)| {
                        let ty = function.signature.params.get(index).ok_or_else(|| {
                            VmError::Runtime(format!("too many arguments for {}", function.name))
                        })?;
                        let value = self.resolve_operand(arg)?;
                        if value.ty != *ty {
                            return Err(VmError::TypeMismatch {
                                expected: format!("{ty:?}"),
                                found: format!("{:?}", value.ty),
                            });
                        }
                        Ok(value.value)
                    })
                    .collect::<LirResult<Vec<_>>>()?;
                let value = self.run_function(&function, &resolved_args)?;
                let Some(ty) = result_ty else {
                    return Ok(());
                };
                if *ty != function.signature.return_type {
                    return Err(VmError::TypeMismatch {
                        expected: format!("{:?}", function.signature.return_type),
                        found: format!("{ty:?}"),
                    });
                }
                self.write_typed_result(dst, ty, value)
            }
        }
    }

    fn handle_call_named(
        &mut self,
        dst: u32,
        name: &str,
        args: &[LirValue],
        package_id: Option<PackageId>,
        definition: Option<LirFunction>,
        result_ty: Option<&LirType>,
    ) -> LirResult<()> {
        let typed_args: Vec<TypedValue> = args
            .iter()
            .map(|arg| self.resolve_operand(arg))
            .collect::<LirResult<Vec<_>>>()?;
        if let Some(host) = self.host_functions.get(name) {
            let sig = lir_sig_to_ffi(&host.descriptor.signature);
            let address = host.address();
            let (raws, _cstrings) = self.prepare_ffi_args(&typed_args, &sig)?;
            let ffi = self
                .ffi
                .as_mut()
                .ok_or_else(|| VmError::Runtime("FFI runtime is unavailable".into()))?;
            let raw_result = ffi
                .call_address(address, name, &sig, &raws)
                .map_err(|error| VmError::Runtime(format!("ffi call '{name}' failed: {error}")))?
                .ok_or_else(|| VmError::Runtime(format!("ffi call '{name}' returned no value")))?;
            let ty = result_ty
                .ok_or_else(|| VmError::Runtime(format!("call '{name}' has no result type")))?;
            return self.write_typed_result(dst, ty, self.decode_storage_value(raw_result, ty)?);
        }
        if let Some(sig) = self.extern_sigs.get(name).cloned() {
            let (raws, _cstrings) = self.prepare_ffi_args(&typed_args, &sig)?;
            let ffi = self
                .ffi
                .as_mut()
                .ok_or_else(|| VmError::Runtime("FFI runtime is unavailable".into()))?;
            let raw_result = ffi
                .call(name, &sig, &raws)
                .map_err(|error| VmError::Runtime(format!("ffi call '{name}' failed: {error}")))?
                .ok_or_else(|| VmError::Runtime(format!("ffi call '{name}' returned no value")))?;
            let ty = result_ty
                .ok_or_else(|| VmError::Runtime(format!("call '{name}' has no result type")))?;
            return self.write_typed_result(dst, ty, self.decode_storage_value(raw_result, ty)?);
        }

        if name.starts_with("__bc_") || Self::is_known_intrinsic(name) {
            let result = self.call_intrinsic(name, &typed_args, result_ty)?;
            return self.write_typed_register(dst, result);
        }

        let function = if definition.is_some() {
            definition
        } else {
            let name = Name::new(name);
            let direct = self
                .program
                .as_ref()
                .and_then(|program| match &package_id {
                    Some(package_id) => program.find_function(package_id, &name),
                    None => program.find_function_any_package(&name),
                })
                .cloned();
            direct
        }
        .ok_or_else(|| VmError::Runtime(format!("missing function {name}")))?;
        let resolved_args: Vec<Value> = args
            .iter()
            .enumerate()
            .map(|(index, arg)| {
                let ty = function.signature.params.get(index).ok_or_else(|| {
                    VmError::Runtime(format!("too many arguments for {}", function.name))
                })?;
                let value = self.resolve_operand(arg)?;
                if value.ty != *ty {
                    return Err(VmError::TypeMismatch {
                        expected: format!("{ty:?}"),
                        found: format!("{:?}", value.ty),
                    });
                }
                Ok(value.value)
            })
            .collect::<LirResult<Vec<_>>>()?;
        if args.len() != function.signature.params.len() {
            return Err(VmError::Runtime(format!(
                "function {} expects {} arguments, got {}",
                function.name,
                function.signature.params.len(),
                args.len()
            )));
        }
        let value = self
            .run_function(&function, &resolved_args)
            .map_err(|error| {
                VmError::Runtime(format!(
                    "while executing function {}: {error}",
                    function.name
                ))
            })?;
        let Some(ty) = result_ty else {
            return Ok(());
        };
        if *ty != function.signature.return_type {
            return Err(VmError::TypeMismatch {
                expected: format!("{:?}", function.signature.return_type),
                found: format!("{ty:?}"),
            });
        }
        self.write_typed_result(dst, ty, value)
    }

    fn call_intrinsic(
        &mut self,
        name: &str,
        args: &[TypedValue],
        result_ty: Option<&LirType>,
    ) -> LirResult<TypedValue> {
        if let Some(rest) = name.strip_prefix("__bc_") {
            return self.call_bc_intrinsic(rest, args, result_ty);
        }
        let result_ty = match result_ty {
            Some(ty) => ty.clone(),
            None if matches!(name, "println" | "print" | "printf" | "eprintln" | "eprint") => {
                LirType::Void
            }
            None => {
                return Err(VmError::Runtime(format!(
                    "intrinsic '{name}' has no result type"
                )));
            }
        };
        let unit = || TypedValue {
            ty: result_ty.clone(),
            value: Value::unit(),
        };
        match name {
            "println" => {
                for arg in args {
                    print!("{}", self.render_value(&arg.value));
                }
                println!();
                Ok(unit())
            }
            "print" | "printf" => {
                for arg in args {
                    print!("{}", self.render_value(&arg.value));
                }
                Ok(unit())
            }
            "eprintln" => {
                for arg in args {
                    eprint!("{}", self.render_value(&arg.value));
                }
                eprintln!();
                Ok(unit())
            }
            "eprint" => {
                for arg in args {
                    eprint!("{}", self.render_value(&arg.value));
                }
                Ok(unit())
            }
            "sizeof" | "strlen" => Ok(TypedValue {
                ty: result_ty.clone(),
                value: integer_value(0, lir_type_info(&result_ty).1),
            }),
            "malloc" => {
                let size = args
                    .first()
                    .ok_or_else(|| VmError::Runtime("malloc expects a size".into()))?;
                let size = self.integer_value(size)? as usize;
                let obj = Value::Bytes(fp_core::ast::ValueBytes::zeroed(size));
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(obj);
                Ok(TypedValue {
                    ty: result_ty.clone(),
                    value: Value::Pointer(fp_core::ast::ValuePointer::managed(handle as i64)),
                })
            }
            "free" => Ok(unit()),
            "realloc" => {
                let _ptr = args
                    .first()
                    .ok_or_else(|| VmError::Runtime("realloc expects a pointer".into()))?;
                let _new_size = args
                    .get(1)
                    .ok_or_else(|| VmError::Runtime("realloc expects a size".into()))?;
                let obj = Value::Unit(Default::default());
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(obj);
                Ok(TypedValue {
                    ty: result_ty.clone(),
                    value: Value::Pointer(fp_core::ast::ValuePointer::managed(handle as i64)),
                })
            }
            "sin" | "cos" | "tan" | "sqrt" | "pow" => Ok(TypedValue {
                ty: result_ty.clone(),
                value: Value::decimal(0.0),
            }),
            "strcmp" => {
                if args.len() != 2 {
                    return Err(VmError::Runtime("strcmp expects two arguments".into()));
                }
                let lhs = self.expect_pointer(&args[0])?;
                let rhs = self.expect_pointer(&args[1])?;
                Ok(TypedValue {
                    ty: result_ty.clone(),
                    value: integer_value(u64::from(lhs.value == rhs.value), false),
                })
            }
            _ => Err(VmError::Runtime(format!("unknown intrinsic: {name}"))),
        }
    }

    fn is_known_intrinsic(name: &str) -> bool {
        if name.starts_with("__bc_") {
            return true;
        }
        matches!(
            name,
            "println"
                | "print"
                | "eprintln"
                | "eprint"
                | "printf"
                | "sizeof"
                | "strlen"
                | "malloc"
                | "free"
                | "realloc"
                | "sin"
                | "cos"
                | "tan"
                | "sqrt"
                | "pow"
                | "strcmp"
        )
    }

    fn call_bc_intrinsic(
        &mut self,
        name: &str,
        args: &[TypedValue],
        result_ty: Option<&LirType>,
    ) -> LirResult<TypedValue> {
        let result_ty = match result_ty {
            Some(ty) => ty,
            None if matches!(name, "println" | "print" | "eprintln" | "eprint" | "printf") => {
                &LirType::Void
            }
            None => {
                return Err(VmError::Runtime(format!(
                    "bytecode intrinsic '{name}' has no result type"
                )));
            }
        };
        let result = |value: Value| TypedValue {
            ty: result_ty.clone(),
            value,
        };
        match name {
            "make_tuple" | "make_array" | "make_list" => {
                let count = self.bc_integer_arg(name, args, 0)? as usize;
                Self::require_bc_arity(name, args, count + 1)?;
                let elements: Vec<Value> = args[1..].iter().map(|arg| arg.value.clone()).collect();
                let obj = match name {
                    "make_tuple" => Value::Tuple(ValueTuple::new(elements)),
                    _ => Value::List(ValueList::new(elements)),
                };
                let handle = self.state.objects.len() as u64;
                let value = obj.clone();
                self.state.objects.push(obj);
                Ok(result(if Self::is_aggregate_runtime_type(result_ty) {
                    value
                } else {
                    Value::uint(handle)
                }))
            }
            "make_map" => {
                let count = self.bc_integer_arg(name, args, 0)? as usize;
                Self::require_bc_arity(name, args, count * 2 + 1)?;
                let mut entries = Vec::with_capacity(count);
                let mut i = 1;
                for _ in 0..count {
                    let key = args
                        .get(i)
                        .ok_or_else(|| VmError::Runtime(format!("missing key {i}")))?;
                    let value = args
                        .get(i + 1)
                        .ok_or_else(|| VmError::Runtime(format!("missing value {}", i + 1)))?;
                    entries.push(ValueMapEntry::new(key.value.clone(), value.value.clone()));
                    i += 2;
                }
                let obj = Value::Map(fp_core::ast::ValueMap { entries });
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(obj.clone());
                Ok(result(if Self::is_aggregate_runtime_type(result_ty) {
                    obj
                } else {
                    Value::uint(handle)
                }))
            }
            "tuple_get" | "array_get" => {
                Self::require_bc_arity(name, args, 2)?;
                let handle = self.container_handle(name, &args[0])?;
                let obj = self
                    .state
                    .objects
                    .get(handle)
                    .cloned()
                    .ok_or(VmError::Runtime(format!("dangling handle {handle}")))?;
                let element = match &obj {
                    Value::Tuple(t) => Some(self.indexed_value(&args[1], &t.values, name)?),
                    Value::List(l) => Some(self.indexed_value(&args[1], &l.values, name)?),
                    Value::Map(map) => map
                        .entries
                        .iter()
                        .find(|entry| entry.key == args[1].value)
                        .map(|entry| entry.value.clone()),
                    Value::Struct(value) => {
                        let field =
                            self.struct_field_by_name(&value.structural.fields, &args[1], name)?;
                        Some(field.value.clone())
                    }
                    Value::Structural(value) => {
                        let field = self.struct_field_by_name(&value.fields, &args[1], name)?;
                        Some(field.value.clone())
                    }
                    _ => return Err(VmError::Runtime("get on non-container".into())),
                }
                .ok_or_else(|| VmError::Runtime("container key not found".into()))?;
                Ok(result(element))
            }
            "tuple_set" => {
                Self::require_bc_arity(name, args, 3)?;
                let handle = self.container_handle(name, &args[0])?;
                let index = self.bc_integer_arg(name, args, 1)? as usize;
                let obj = self
                    .state
                    .objects
                    .get(handle)
                    .cloned()
                    .ok_or(VmError::Runtime(format!("dangling handle {handle}")))?;
                if matches!(obj, Value::Struct(_) | Value::Structural(_)) {
                    let mut value = obj;
                    let field_index = self.bc_integer_arg(name, args, 1)? as usize;
                    let replacement = args[2].value.clone();
                    match &mut value {
                        Value::Struct(structure) => {
                            structure
                                .structural
                                .fields
                                .get_mut(field_index)
                                .ok_or_else(|| {
                                    VmError::Runtime(format!(
                                        "field index {field_index} out of bounds"
                                    ))
                                })?
                                .value = replacement
                        }
                        Value::Structural(structure) => {
                            structure
                                .fields
                                .get_mut(field_index)
                                .ok_or_else(|| {
                                    VmError::Runtime(format!(
                                        "field index {field_index} out of bounds"
                                    ))
                                })?
                                .value = replacement
                        }
                        _ => unreachable!(),
                    }
                    let new_handle = self.state.objects.len() as u64;
                    self.state.objects.push(value.clone());
                    return Ok(result(if Self::is_aggregate_runtime_type(result_ty) {
                        value
                    } else {
                        Value::uint(new_handle)
                    }));
                }
                let mut values = match &obj {
                    Value::Tuple(t) => t.values.clone(),
                    _ => return Err(VmError::Runtime("set on non-tuple".into())),
                };
                if index >= values.len() {
                    return Err(VmError::Runtime(format!("index {index} out of bounds")));
                }
                values[index] = args[2].value.clone();
                let new_handle = self.state.objects.len() as u64;
                let value = Value::Tuple(ValueTuple::new(values));
                self.state.objects.push(value.clone());
                Ok(result(if Self::is_aggregate_runtime_type(result_ty) {
                    value
                } else {
                    Value::uint(new_handle)
                }))
            }
            "array_set" => {
                Self::require_bc_arity(name, args, 3)?;
                let handle = self.container_handle(name, &args[0])?;
                let index = self.bc_integer_arg(name, args, 1)? as usize;
                let obj = self
                    .state
                    .objects
                    .get(handle)
                    .cloned()
                    .ok_or(VmError::Runtime(format!("dangling handle {handle}")))?;
                let mut values = match &obj {
                    Value::Tuple(t) => t.values.clone(),
                    Value::List(l) => l.values.clone(),
                    _ => return Err(VmError::Runtime("set on non-array container".into())),
                };
                if index >= values.len() {
                    return Err(VmError::Runtime(format!("index {index} out of bounds")));
                }
                values[index] = args[2].value.clone();
                let value = match obj {
                    Value::Tuple(_) => Value::Tuple(ValueTuple::new(values)),
                    Value::List(_) => Value::List(ValueList::new(values)),
                    _ => unreachable!(),
                };
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(value.clone());
                Ok(result(if Self::is_aggregate_runtime_type(result_ty) {
                    value
                } else {
                    Value::uint(handle)
                }))
            }
            "container_len" => {
                Self::require_bc_arity(name, args, 1)?;
                let handle = self.container_handle(name, &args[0])?;
                let obj = self
                    .state
                    .objects
                    .get(handle)
                    .cloned()
                    .ok_or(VmError::Runtime(format!("dangling handle {handle}")))?;
                let len = match &obj {
                    Value::Tuple(t) => t.values.len() as u64,
                    Value::List(l) => l.values.len() as u64,
                    Value::Map(m) => m.entries.len() as u64,
                    Value::String(s) => s.value.len() as u64,
                    _ => return Err(VmError::Runtime("len on non-container".into())),
                };
                Ok(result(integer_value(len, lir_type_info(result_ty).1)))
            }
            "slice" => {
                Self::require_bc_arity(name, args, 3)?;
                let handle = self.container_handle(name, &args[0])?;
                let start = usize::try_from(self.bc_integer_arg(name, args, 1)?)
                    .map_err(|_| VmError::Runtime("negative slice start".into()))?;
                let end = usize::try_from(self.bc_integer_arg(name, args, 2)?)
                    .map_err(|_| VmError::Runtime("negative slice end".into()))?;
                if start > end {
                    return Err(VmError::Runtime("slice start exceeds end".into()));
                }
                let object = self
                    .state
                    .objects
                    .get(handle)
                    .cloned()
                    .ok_or(VmError::Runtime(format!("dangling handle {handle}")))?;
                let sliced = match object {
                    Value::String(string) => {
                        let chars: Vec<char> = string.value.chars().collect();
                        let part = chars
                            .get(start..end)
                            .ok_or(VmError::Runtime("string slice out of bounds".into()))?
                            .iter()
                            .collect::<String>();
                        Value::string(part)
                    }
                    Value::List(list) => Value::List(ValueList::new(
                        list.values
                            .get(start..end)
                            .ok_or(VmError::Runtime("list slice out of bounds".into()))?
                            .to_vec(),
                    )),
                    Value::Tuple(tuple) => Value::Tuple(ValueTuple::new(
                        tuple
                            .values
                            .get(start..end)
                            .ok_or(VmError::Runtime("tuple slice out of bounds".into()))?
                            .to_vec(),
                    )),
                    _ => return Err(VmError::Runtime("slice on non-sequence".into())),
                };
                let new_handle = self.state.objects.len() as u64;
                self.state.objects.push(sliced);
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    new_handle as i64,
                ))))
            }
            "proc_macro_token_stream_from_str" => {
                Self::require_bc_arity(name, args, 1)?;
                let handle = self.container_handle(name, &args[0])?;
                let source = match self.state.objects.get(handle) {
                    Some(Value::String(value)) => value.value.clone(),
                    Some(_) => {
                        return Err(VmError::Runtime(
                            "token stream source must be a string".into(),
                        ));
                    }
                    None => return Err(VmError::Runtime(format!("dangling handle {handle}"))),
                };
                let tokens = source
                    .split_whitespace()
                    .map(|text| {
                        fp_core::ast::MacroTokenTree::Token(fp_core::ast::MacroToken {
                            text: text.to_owned(),
                            span: fp_core::span::Span::null(),
                        })
                    })
                    .collect();
                let stream = Value::TokenStream(fp_core::ast::ValueTokenStream { tokens });
                let stream_handle = self.state.objects.len() as u64;
                self.state.objects.push(stream);
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    stream_handle as i64,
                ))))
            }
            "str_alloc" => {
                Self::require_bc_arity(name, args, 1)?;
                let len = self.bc_integer_arg(name, args, 0)? as usize;
                let s = " ".repeat(len);
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(Value::string(s));
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    handle as i64,
                ))))
            }
            "str_const" => {
                let mut bytes = Vec::with_capacity(args.len());
                for index in 0..args.len() {
                    let byte = self.bc_integer_arg(name, args, index)?;
                    let byte = u8::try_from(byte).map_err(|_| {
                        VmError::Runtime(format!("string byte {byte} is out of range"))
                    })?;
                    bytes.push(byte);
                }
                let text = String::from_utf8(bytes)
                    .map_err(|error| VmError::Runtime(format!("invalid UTF-8 string: {error}")))?;
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(Value::string(text));
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    handle as i64,
                ))))
            }
            "yaml_to_json" => {
                Self::require_bc_arity(name, args, 1)?;
                let handle = self.container_handle(name, &args[0])?;
                let source = match self.state.objects.get(handle) {
                    Some(Value::String(string)) => string.value.clone(),
                    Some(value) => {
                        return Err(VmError::Runtime(format!(
                            "{name} expects a string, got {value:?}"
                        )));
                    }
                    None => {
                        return Err(VmError::Runtime(format!("dangling string handle {handle}")));
                    }
                };
                let yaml = serde_yaml::from_str::<serde_yaml::Value>(&source)
                    .map_err(|error| VmError::Runtime(format!("failed to parse YAML: {error}")))?;
                let json = serde_json::to_string(&yaml).map_err(|error| {
                    VmError::Runtime(format!("failed to serialize YAML as JSON: {error}"))
                })?;
                let handle = self.state.objects.len() as i64;
                self.state.objects.push(Value::string(json));
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    handle,
                ))))
            }
            "json_parse" => {
                Self::require_bc_arity(name, args, 1)?;
                let source = {
                    let handle = self.container_handle(name, &args[0])?;
                    let Value::String(value) = self
                        .state
                        .objects
                        .get(handle)
                        .cloned()
                        .ok_or(VmError::Runtime(format!("dangling string handle {handle}")))?
                    else {
                        return Err(VmError::Runtime("json_parse expects a string".into()));
                    };
                    value.value
                };
                let parsed = serde_json::from_str::<serde_json::Value>(&source)
                    .map_err(|error| VmError::Runtime(format!("failed to parse JSON: {error}")))?;
                let value = json_to_runtime_value(parsed)?;
                let handle = self.state.objects.len() as i64;
                self.state.objects.push(value);
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    handle,
                ))))
            }
            "println" => {
                for arg in args {
                    print!("{}", self.render_value(&arg.value));
                }
                println!();
                Ok(result(Value::unit()))
            }
            "print" => {
                for arg in args {
                    print!("{}", self.render_value(&arg.value));
                }
                Ok(result(Value::unit()))
            }
            "format" => {
                let mut result = String::new();
                for arg in args {
                    result.push_str(&self.render_value(&arg.value));
                }
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(Value::string(result));
                Ok(TypedValue {
                    ty: result_ty.clone(),
                    value: Value::Pointer(fp_core::ast::ValuePointer::managed(handle as i64)),
                })
            }
            "time_now" => {
                use std::time::{SystemTime, UNIX_EPOCH};
                let dur = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap_or_default();
                Ok(result(Value::decimal(dur.as_secs_f64())))
            }
            "panic" => {
                let message = args
                    .first()
                    .map(|arg| self.render_value(&arg.value))
                    .unwrap_or_else(|| "panic".into());
                Err(VmError::Runtime(format!("panic: {message}")))
            }
            "catch_unwind" => {
                Self::require_bc_arity(name, args, 1)?;
                let function_handle = self.container_handle(name, &args[0])?;
                let function_name = match self.state.objects.get(function_handle).cloned() {
                    Some(Value::String(value)) => value.value,
                    Some(value) => {
                        return Err(VmError::Runtime(format!(
                            "catch_unwind expects a function reference, got {value:?}"
                        )));
                    }
                    None => {
                        return Err(VmError::Runtime(format!(
                            "dangling function handle {function_handle}"
                        )));
                    }
                };
                let function = self
                    .program
                    .as_ref()
                    .and_then(|program| {
                        program.find_function_any_package(&fp_core::lir::Name::new(
                            function_name.clone(),
                        ))
                    })
                    .cloned()
                    .ok_or_else(|| VmError::Runtime(format!("missing function {function_name}")))?;
                let fields = match self.run_function(&function, &[]) {
                    Ok(value) => vec![
                        fp_core::ast::ValueField::new(
                            fp_core::ast::Ident::new("ok"),
                            Value::bool(true),
                        ),
                        fp_core::ast::ValueField::new(fp_core::ast::Ident::new("value"), value),
                        fp_core::ast::ValueField::new(
                            fp_core::ast::Ident::new("error"),
                            Value::None(Default::default()),
                        ),
                    ],
                    Err(error) => vec![
                        fp_core::ast::ValueField::new(
                            fp_core::ast::Ident::new("ok"),
                            Value::bool(false),
                        ),
                        fp_core::ast::ValueField::new(
                            fp_core::ast::Ident::new("value"),
                            Value::None(Default::default()),
                        ),
                        fp_core::ast::ValueField::new(
                            fp_core::ast::Ident::new("error"),
                            Value::string(error.to_string()),
                        ),
                    ],
                };
                let handle = self.state.objects.len() as i64;
                self.state
                    .objects
                    .push(Value::Structural(fp_core::ast::ValueStructural::new(
                        fields,
                    )));
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    handle,
                ))))
            }
            "shell_exec" => {
                Self::require_bc_arity(name, args, 1)?;
                let handle = self.container_handle(name, &args[0])?;
                let command = match self.state.objects.get(handle) {
                    Some(Value::String(value)) => value.value.clone(),
                    Some(value) => {
                        return Err(VmError::Runtime(format!(
                            "{name} expects a string, got {value:?}"
                        )));
                    }
                    None => {
                        return Err(VmError::Runtime(format!("dangling string handle {handle}")));
                    }
                };
                let output = std::process::Command::new("sh")
                    .arg("-c")
                    .arg(&command)
                    .output()
                    .map_err(|error| {
                        VmError::Runtime(format!("failed to execute shell command: {error}"))
                    })?;
                if !output.status.success() {
                    return Err(VmError::Runtime(format!(
                        "shell command exited with status {}: {}",
                        output.status,
                        String::from_utf8_lossy(&output.stderr).trim()
                    )));
                }
                let text = String::from_utf8(output.stdout).map_err(|error| {
                    VmError::Runtime(format!("shell command returned invalid UTF-8: {error}"))
                })?;
                let result_handle = self.state.objects.len() as i64;
                self.state.objects.push(Value::string(text));
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    result_handle,
                ))))
            }
            "fs_exists" | "fs_is_file" | "fs_is_dir" => {
                Self::require_bc_arity(name, args, 1)?;
                let handle = self.container_handle(name, &args[0])?;
                let Value::String(path) = self
                    .state
                    .objects
                    .get(handle)
                    .cloned()
                    .ok_or(VmError::Runtime(format!("dangling string handle {handle}")))?
                else {
                    return Err(VmError::Runtime(format!("{name} expects a string")));
                };
                let path = std::path::Path::new(&path.value);
                let exists = match name {
                    "fs_exists" => path.exists(),
                    "fs_is_file" => path.is_file(),
                    "fs_is_dir" => path.is_dir(),
                    _ => unreachable!(),
                };
                Ok(result(Value::bool(exists)))
            }
            "fs_read_to_string" => {
                Self::require_bc_arity(name, args, 1)?;
                let handle = self.container_handle(name, &args[0])?;
                let Value::String(path) = self
                    .state
                    .objects
                    .get(handle)
                    .cloned()
                    .ok_or(VmError::Runtime(format!("dangling string handle {handle}")))?
                else {
                    return Err(VmError::Runtime(format!("{name} expects a string path")));
                };
                let contents = std::fs::read_to_string(&path.value).map_err(|error| {
                    VmError::Runtime(format!("failed to read '{}': {error}", path.value))
                })?;
                let new_handle = self.state.objects.len() as i64;
                self.state.objects.push(Value::string(contents));
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    new_handle,
                ))))
            }
            "fs_write_string" | "fs_append_string" => {
                Self::require_bc_arity(name, args, 2)?;
                let read_string = |arg: &TypedValue| -> LirResult<String> {
                    let handle = self.container_handle(name, arg)?;
                    let Some(Value::String(value)) = self.state.objects.get(handle) else {
                        return Err(VmError::Runtime(format!("{name} expects string arguments")));
                    };
                    Ok(value.value.clone())
                };
                let path = read_string(&args[0])?;
                let contents = read_string(&args[1])?;
                let io_result = if name == "fs_write_string" {
                    std::fs::write(&path, contents)
                } else {
                    use std::io::Write;
                    let mut file = std::fs::OpenOptions::new()
                        .create(true)
                        .append(true)
                        .open(&path);
                    match file.as_mut() {
                        Ok(file) => file.write_all(contents.as_bytes()),
                        Err(error) => Err(std::io::Error::new(error.kind(), error.to_string())),
                    }
                };
                io_result.map_err(|error| {
                    VmError::Runtime(format!("failed to write '{}': {error}", path))
                })?;
                Ok(result(Value::unit()))
            }
            "fs_create_dir_all" | "fs_remove_file" | "fs_remove_dir_all" => {
                Self::require_bc_arity(name, args, 1)?;
                let handle = self.container_handle(name, &args[0])?;
                let Value::String(path) = self
                    .state
                    .objects
                    .get(handle)
                    .cloned()
                    .ok_or(VmError::Runtime(format!("dangling string handle {handle}")))?
                else {
                    return Err(VmError::Runtime(format!("{name} expects a string path")));
                };
                let io_result = match name {
                    "fs_create_dir_all" => std::fs::create_dir_all(&path.value),
                    "fs_remove_file" => std::fs::remove_file(&path.value),
                    "fs_remove_dir_all" => std::fs::remove_dir_all(&path.value),
                    _ => unreachable!(),
                };
                io_result.map_err(|error| {
                    VmError::Runtime(format!("{name} '{}': {error}", path.value))
                })?;
                Ok(result(Value::unit()))
            }
            "fs_read_dir" => {
                Self::require_bc_arity(name, args, 1)?;
                let handle = self.container_handle(name, &args[0])?;
                let Value::String(path) = self
                    .state
                    .objects
                    .get(handle)
                    .cloned()
                    .ok_or(VmError::Runtime(format!("dangling string handle {handle}")))?
                else {
                    return Err(VmError::Runtime(format!("{name} expects a string path")));
                };
                let entries = std::fs::read_dir(&path.value).map_err(|error| {
                    VmError::Runtime(format!(
                        "failed to read directory '{}': {error}",
                        path.value
                    ))
                })?;
                let mut paths = Vec::new();
                for entry in entries {
                    let entry = entry.map_err(|error| {
                        VmError::Runtime(format!("failed to read directory entry: {error}"))
                    })?;
                    paths.push(entry.path().to_string_lossy().into_owned());
                }
                paths.sort();
                let mut values = Vec::with_capacity(paths.len());
                for path in paths {
                    let entry_handle = self.state.objects.len() as i64;
                    self.state.objects.push(Value::string(path));
                    values.push(Value::Pointer(fp_core::ast::ValuePointer::managed(
                        entry_handle,
                    )));
                }
                let list_handle = self.state.objects.len() as i64;
                self.state.objects.push(Value::List(ValueList::new(values)));
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    list_handle,
                ))))
            }
            "fs_walk_dir" => {
                Self::require_bc_arity(name, args, 1)?;
                let handle = self.container_handle(name, &args[0])?;
                let Value::String(path) = self
                    .state
                    .objects
                    .get(handle)
                    .cloned()
                    .ok_or(VmError::Runtime(format!("dangling string handle {handle}")))?
                else {
                    return Err(VmError::Runtime(format!("{name} expects a string path")));
                };
                let mut paths = Vec::new();
                let mut pending = vec![std::path::PathBuf::from(&path.value)];
                while let Some(directory) = pending.pop() {
                    let entries = std::fs::read_dir(&directory).map_err(|error| {
                        VmError::Runtime(format!(
                            "failed to walk '{}': {error}",
                            directory.display()
                        ))
                    })?;
                    for entry in entries {
                        let entry = entry.map_err(|error| {
                            VmError::Runtime(format!("failed to read directory entry: {error}"))
                        })?;
                        let entry_path = entry.path();
                        paths.push(entry_path.to_string_lossy().into_owned());
                        if entry
                            .file_type()
                            .map_err(|error| {
                                VmError::Runtime(format!(
                                    "failed to inspect '{}': {error}",
                                    entry_path.display()
                                ))
                            })?
                            .is_dir()
                        {
                            pending.push(entry_path);
                        }
                    }
                }
                paths.sort();
                let values = paths
                    .into_iter()
                    .map(|path| {
                        let entry_handle = self.state.objects.len() as i64;
                        self.state.objects.push(Value::string(path));
                        Value::Pointer(fp_core::ast::ValuePointer::managed(entry_handle))
                    })
                    .collect();
                let list_handle = self.state.objects.len() as i64;
                self.state.objects.push(Value::List(ValueList::new(values)));
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    list_handle,
                ))))
            }
            "fs_glob" => {
                Self::require_bc_arity(name, args, 1)?;
                let handle = self.container_handle(name, &args[0])?;
                let Value::String(pattern) = self
                    .state
                    .objects
                    .get(handle)
                    .cloned()
                    .ok_or(VmError::Runtime(format!("dangling string handle {handle}")))?
                else {
                    return Err(VmError::Runtime(format!("{name} expects a string pattern")));
                };
                let matcher = globset::Glob::new(&pattern.value)
                    .map_err(|error| VmError::Runtime(format!("invalid glob pattern: {error}")))?
                    .compile_matcher();
                let mut matches = Vec::new();
                let mut pending = vec![std::path::PathBuf::from(".")];
                while let Some(directory) = pending.pop() {
                    let entries = std::fs::read_dir(&directory).map_err(|error| {
                        VmError::Runtime(format!(
                            "failed to read '{}': {error}",
                            directory.display()
                        ))
                    })?;
                    for entry in entries {
                        let entry = entry.map_err(|error| {
                            VmError::Runtime(format!("failed to read directory entry: {error}"))
                        })?;
                        let entry_path = entry.path();
                        let relative_path = entry_path.strip_prefix(".").unwrap_or(&entry_path);
                        if matcher.is_match(&entry_path) || matcher.is_match(relative_path) {
                            matches.push(entry_path.to_string_lossy().into_owned());
                        }
                        if entry
                            .file_type()
                            .map_err(|error| {
                                VmError::Runtime(format!(
                                    "failed to inspect '{}': {error}",
                                    entry_path.display()
                                ))
                            })?
                            .is_dir()
                        {
                            pending.push(entry_path);
                        }
                    }
                }
                matches.sort();
                let values = matches
                    .into_iter()
                    .map(|path| {
                        let entry_handle = self.state.objects.len() as i64;
                        self.state.objects.push(Value::string(path));
                        Value::Pointer(fp_core::ast::ValuePointer::managed(entry_handle))
                    })
                    .collect();
                let list_handle = self.state.objects.len() as i64;
                self.state.objects.push(Value::List(ValueList::new(values)));
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    list_handle,
                ))))
            }
            "path_join" | "path_parent" | "path_file_name" | "path_extension" | "path_stem"
            | "path_normalize" | "path_is_absolute" => {
                if name == "path_join" {
                    if args.len() < 2 {
                        return Err(VmError::Runtime(
                            "path_join expects at least two path components".into(),
                        ));
                    }
                } else {
                    Self::require_bc_arity(name, args, 1)?;
                }
                let path_value = |arg: &TypedValue| -> LirResult<String> {
                    let handle = self.container_handle(name, arg)?;
                    let Some(Value::String(value)) = self.state.objects.get(handle) else {
                        return Err(VmError::Runtime(format!("{name} expects a string")));
                    };
                    Ok(value.value.clone())
                };
                let first = path_value(&args[0])?;
                if name == "path_is_absolute" {
                    return Ok(result(Value::bool(
                        std::path::Path::new(&first).is_absolute(),
                    )));
                }
                let text = match name {
                    "path_join" => {
                        let mut joined = std::path::PathBuf::from(first);
                        for arg in &args[1..] {
                            joined.push(path_value(arg)?);
                        }
                        joined.to_string_lossy().into_owned()
                    }
                    "path_parent" => std::path::Path::new(&first)
                        .parent()
                        .map(|path| path.to_string_lossy().into_owned())
                        .unwrap_or_default(),
                    "path_file_name" => std::path::Path::new(&first)
                        .file_name()
                        .map(|value| value.to_string_lossy().into_owned())
                        .unwrap_or_default(),
                    "path_extension" => std::path::Path::new(&first)
                        .extension()
                        .map(|value| value.to_string_lossy().into_owned())
                        .unwrap_or_default(),
                    "path_stem" => std::path::Path::new(&first)
                        .file_stem()
                        .map(|value| value.to_string_lossy().into_owned())
                        .unwrap_or_default(),
                    "path_normalize" => {
                        let mut normalized = std::path::PathBuf::new();
                        for component in std::path::Path::new(&first).components() {
                            match component {
                                std::path::Component::CurDir => {}
                                std::path::Component::ParentDir => {
                                    normalized.pop();
                                }
                                component => normalized.push(component.as_os_str()),
                            }
                        }
                        normalized.to_string_lossy().into_owned()
                    }
                    _ => unreachable!(),
                };
                let handle = self.state.objects.len() as i64;
                self.state.objects.push(Value::string(text));
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    handle,
                ))))
            }
            "env_current_dir" | "env_temp_dir" | "env_home_dir" | "env_var" | "env_var_exists" => {
                let takes_name = matches!(name, "env_var" | "env_var_exists");
                Self::require_bc_arity(name, args, usize::from(takes_name))?;
                let string_arg = |arg: &TypedValue| -> LirResult<String> {
                    let handle = self.container_handle(name, arg)?;
                    let Some(Value::String(value)) = self.state.objects.get(handle) else {
                        return Err(VmError::Runtime(format!("{name} expects a string")));
                    };
                    Ok(value.value.clone())
                };
                if name == "env_var_exists" {
                    return Ok(result(Value::bool(
                        std::env::var_os(string_arg(&args[0])?).is_some(),
                    )));
                }
                let value = match name {
                    "env_current_dir" => std::env::current_dir()
                        .map_err(|error| VmError::Runtime(format!("current directory: {error}")))?
                        .to_string_lossy()
                        .into_owned(),
                    "env_temp_dir" => std::env::temp_dir().to_string_lossy().into_owned(),
                    "env_home_dir" => std::env::var("HOME").map_err(|error| {
                        VmError::Runtime(format!("HOME is unavailable: {error}"))
                    })?,
                    "env_var" => std::env::var(string_arg(&args[0])?).map_err(|error| {
                        VmError::Runtime(format!("environment variable is unavailable: {error}"))
                    })?,
                    _ => unreachable!(),
                };
                let handle = self.state.objects.len() as i64;
                self.state.objects.push(Value::string(value));
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    handle,
                ))))
            }
            "io_read_stdin_to_string" => {
                Self::require_bc_arity(name, args, 0)?;
                use std::io::Read;
                let mut contents = String::new();
                std::io::stdin()
                    .read_to_string(&mut contents)
                    .map_err(|error| VmError::Runtime(format!("failed to read stdin: {error}")))?;
                let handle = self.state.objects.len() as i64;
                self.state.objects.push(Value::string(contents));
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    handle,
                ))))
            }
            "io_write_stdout" | "io_write_stderr" => {
                Self::require_bc_arity(name, args, 1)?;
                let handle = self.container_handle(name, &args[0])?;
                let Value::String(contents) = self
                    .state
                    .objects
                    .get(handle)
                    .cloned()
                    .ok_or(VmError::Runtime(format!("dangling string handle {handle}")))?
                else {
                    return Err(VmError::Runtime(format!("{name} expects a string")));
                };
                use std::io::Write;
                let write_result = if name == "io_write_stdout" {
                    std::io::stdout().write_all(contents.value.as_bytes())
                } else {
                    std::io::stderr().write_all(contents.value.as_bytes())
                };
                write_result.map_err(|error| {
                    VmError::Runtime(format!("failed to write {name}: {error}"))
                })?;
                Ok(result(Value::unit()))
            }
            "debug_assertions" => {
                if args.is_empty() {
                    return Err(VmError::Runtime(
                        "debug_assertions expects a condition".into(),
                    ));
                }
                let condition = match &args[0].value {
                    Value::Bool(value) => value.value,
                    Value::UInt(value) => value.value != 0,
                    ref value => {
                        return Err(VmError::Runtime(format!(
                            "debug_assertions expects bool, got {value:?}"
                        )));
                    }
                };
                if !condition {
                    let message = args
                        .get(1)
                        .map(|value| self.render_value(&value.value))
                        .unwrap_or_else(|| "debug assertion failed".into());
                    return Err(VmError::Runtime(message));
                }
                Ok(result(Value::unit()))
            }
            "input" => {
                if args.len() > 1 {
                    return Err(VmError::Runtime("input accepts at most one prompt".into()));
                }
                if let Some(prompt) = args.first() {
                    let handle = self.container_handle(name, prompt)?;
                    let Value::String(prompt) = self
                        .state
                        .objects
                        .get(handle)
                        .cloned()
                        .ok_or(VmError::Runtime("dangling input prompt handle".into()))?
                    else {
                        return Err(VmError::Runtime("input prompt must be a string".into()));
                    };
                    print!("{}", prompt.value);
                }
                use std::io::BufRead;
                let mut line = String::new();
                std::io::stdin()
                    .lock()
                    .read_line(&mut line)
                    .map_err(|error| VmError::Runtime(format!("failed to read input: {error}")))?;
                let handle = self.state.objects.len() as i64;
                self.state.objects.push(Value::string(
                    line.strip_suffix('\n')
                        .and_then(|line| line.strip_suffix('\r'))
                        .unwrap_or(&line)
                        .to_string(),
                ));
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    handle,
                ))))
            }
            "type_name" => {
                Self::require_bc_arity(name, args, 1)?;
                let value = &args[0].value;
                let type_name = match value {
                    Value::Unit(_) => "unit",
                    Value::Bool(_) => "bool",
                    Value::Int(_) => "int",
                    Value::UInt(_) => "uint",
                    Value::BigInt(_) => "bigint",
                    Value::Decimal(_) => "decimal",
                    Value::BigDecimal(_) => "bigdecimal",
                    Value::Char(_) => "char",
                    Value::String(_) => "string",
                    Value::Tuple(_) => "tuple",
                    Value::List(_) => "list",
                    Value::Map(_) => "map",
                    Value::Bytes(_) => "bytes",
                    Value::Pointer(_) => "pointer",
                    Value::Offset(_) => "offset",
                    Value::Function(_) => "function",
                    Value::Null(_) => "null",
                    Value::None(_) => "none",
                    Value::Some(_) | Value::Option(_) => "option",
                    Value::Type(_) => "type",
                    Value::Struct(_) | Value::Structural(_) => "struct",
                    Value::UnionifyClosure(_) => "function",
                    _ => "value",
                };
                let handle = self.state.objects.len() as i64;
                self.state
                    .objects
                    .push(Value::string(type_name.to_string()));
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    handle,
                ))))
            }
            "type_of" => {
                Self::require_bc_arity(name, args, 1)?;
                let ty = match &args[0].value {
                    Value::Unit(_) => Ty::unit(),
                    Value::Bool(_) => Ty::Primitive(TypePrimitive::Bool),
                    Value::Int(_) => Ty::Primitive(TypePrimitive::i64()),
                    Value::UInt(_) => Ty::Primitive(TypePrimitive::Int(fp_core::ast::TypeInt::U64)),
                    Value::Decimal(_) | Value::BigDecimal(_) => Ty::Primitive(TypePrimitive::f64()),
                    Value::Char(_) => Ty::Primitive(TypePrimitive::Char),
                    Value::String(_) => Ty::Primitive(TypePrimitive::String),
                    Value::List(_) => Ty::Primitive(TypePrimitive::List),
                    Value::Tuple(tuple) => Ty::Tuple(fp_core::ast::TypeTuple {
                        types: vec![Ty::unknown(); tuple.values.len()],
                    }),
                    Value::Type(ty) => ty.clone(),
                    Value::Struct(value) => Ty::Struct(value.ty.clone()),
                    Value::Structural(value) => Ty::Structural(fp_core::ast::TypeStructural {
                        fields: value
                            .fields
                            .iter()
                            .map(|field| {
                                fp_core::ast::StructuralField::new(
                                    field.name.clone(),
                                    Ty::unknown(),
                                )
                            })
                            .collect(),
                    }),
                    Value::Function(_) | Value::UnionifyClosure(_) => Ty::unknown(),
                    _ => Ty::unknown(),
                };
                Ok(result(Value::Type(ty)))
            }
            "sleep" => {
                Self::require_bc_arity(name, args, 1)?;
                let seconds = self.expect_float(&args[0])?;
                if seconds.is_sign_negative() {
                    return Err(VmError::Runtime("sleep duration cannot be negative".into()));
                }
                std::thread::sleep(std::time::Duration::from_secs_f64(seconds));
                Ok(result(Value::unit()))
            }
            "spawn" | "select" => {
                Self::require_bc_arity(name, args, 1)?;
                Ok(result(args[0].value.clone()))
            }
            "join" => {
                Self::require_bc_arity(name, args, 1)?;
                if args.len() == 1 {
                    return Ok(result(args[0].value.clone()));
                }
                let value = Value::Tuple(ValueTuple::new(
                    args.iter().map(|arg| arg.value.clone()).collect(),
                ));
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(value.clone());
                Ok(result(if Self::is_aggregate_runtime_type(result_ty) {
                    value
                } else {
                    Value::uint(handle)
                }))
            }
            "yield" => {
                Self::require_bc_arity(name, args, 0)?;
                Ok(result(Value::unit()))
            }
            "size_of" => {
                Self::require_bc_arity(name, args, 1)?;
                let (bits, _) = lir_type_info(&args[0].ty);
                Ok(result(integer_value(
                    u64::from(bits.div_ceil(8)),
                    lir_type_info(result_ty).1,
                )))
            }
            "field_count" => {
                Self::require_bc_arity(name, args, 1)?;
                let object = match &args[0].value {
                    Value::Pointer(pointer) if pointer.value >= 0 => self
                        .state
                        .objects
                        .get(pointer.value as usize)
                        .ok_or(VmError::Runtime("dangling struct handle".into()))?,
                    value => value,
                };
                let count = match object {
                    Value::Struct(value) => value.structural.fields.len(),
                    Value::Structural(value) => value.fields.len(),
                    value => {
                        return Err(VmError::Runtime(format!(
                            "field_count expects a struct, got {value:?}"
                        )));
                    }
                };
                Ok(result(integer_value(
                    count as u64,
                    lir_type_info(result_ty).1,
                )))
            }
            "field_name_at" => {
                Self::require_bc_arity(name, args, 2)?;
                let index = usize::try_from(self.bc_integer_arg(name, args, 1)?)
                    .map_err(|_| VmError::Runtime("field index is out of range".into()))?;
                let object = match &args[0].value {
                    Value::Pointer(pointer) if pointer.value >= 0 => self
                        .state
                        .objects
                        .get(pointer.value as usize)
                        .ok_or(VmError::Runtime("dangling struct handle".into()))?,
                    value => value,
                };
                let field_name = match object {
                    Value::Struct(value) => value.structural.fields.get(index),
                    Value::Structural(value) => value.fields.get(index),
                    value => {
                        return Err(VmError::Runtime(format!(
                            "field_name_at expects a struct, got {value:?}"
                        )));
                    }
                }
                .ok_or_else(|| VmError::Runtime(format!("field index {index} out of bounds")))?
                .name
                .name
                .clone();
                let handle = self.state.objects.len() as i64;
                self.state.objects.push(Value::string(field_name));
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    handle,
                ))))
            }
            "has_field" => {
                Self::require_bc_arity(name, args, 2)?;
                let field_handle = self.container_handle(name, &args[1])?;
                let Value::String(field_name) = self
                    .state
                    .objects
                    .get(field_handle)
                    .cloned()
                    .ok_or(VmError::Runtime(format!(
                        "dangling field name handle {field_handle}"
                    )))?
                else {
                    return Err(VmError::Runtime(
                        "has_field expects a string field name".into(),
                    ));
                };
                let object = match &args[0].value {
                    Value::Pointer(pointer) if pointer.value >= 0 => self
                        .state
                        .objects
                        .get(pointer.value as usize)
                        .ok_or(VmError::Runtime("dangling struct handle".into()))?,
                    value => value,
                };
                let exists = match object {
                    Value::Struct(value) => value
                        .structural
                        .fields
                        .iter()
                        .any(|field| field.name.name == field_name.value),
                    Value::Structural(value) => value
                        .fields
                        .iter()
                        .any(|field| field.name.name == field_name.value),
                    value => {
                        return Err(VmError::Runtime(format!(
                            "has_field expects a struct, got {value:?}"
                        )));
                    }
                };
                Ok(result(Value::bool(exists)))
            }
            "has_method" => {
                Self::require_bc_arity(name, args, 2)?;
                let _method_handle = self.container_handle(name, &args[1])?;
                // Method tables are compile-time metadata; calls that reach the
                // interpreter are validly folded only when that metadata exists.
                Ok(result(Value::bool(false)))
            }
            "method_count" => {
                Self::require_bc_arity(name, args, 1)?;
                Ok(result(integer_value(0, lir_type_info(result_ty).1)))
            }
            "field_type" => {
                Self::require_bc_arity(name, args, 2)?;
                let _field_handle = self.container_handle(name, &args[1])?;
                Ok(result(Value::Type(Ty::unknown())))
            }
            "struct_size" => {
                Self::require_bc_arity(name, args, 1)?;
                let (bits, _) = lir_type_info(&args[0].ty);
                Ok(result(integer_value(
                    u64::from(bits.div_ceil(8)),
                    lir_type_info(result_ty).1,
                )))
            }
            "reflect_fields" => {
                Self::require_bc_arity(name, args, 1)?;
                let object = match &args[0].value {
                    Value::Pointer(pointer) if pointer.value >= 0 => self
                        .state
                        .objects
                        .get(pointer.value as usize)
                        .ok_or(VmError::Runtime("dangling struct handle".into()))?,
                    value => value,
                };
                let names = match object {
                    Value::Struct(value) => value
                        .structural
                        .fields
                        .iter()
                        .map(|field| Value::string(field.name.name.clone()))
                        .collect(),
                    Value::Structural(value) => value
                        .fields
                        .iter()
                        .map(|field| Value::string(field.name.name.clone()))
                        .collect(),
                    value => {
                        return Err(VmError::Runtime(format!(
                            "reflect_fields expects a struct, got {value:?}"
                        )));
                    }
                };
                let list = Value::List(ValueList::new(names));
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(list.clone());
                Ok(result(if Self::is_aggregate_runtime_type(result_ty) {
                    list
                } else {
                    Value::uint(handle)
                }))
            }
            "create_struct" => {
                if args.is_empty() || args.len() % 2 != 0 {
                    return Err(VmError::Runtime(
                        "create_struct expects field-name/value pairs".into(),
                    ));
                }
                let mut fields = Vec::with_capacity(args.len() / 2);
                for pair in args.chunks_exact(2) {
                    let handle = self.container_handle(name, &pair[0])?;
                    let Value::String(field_name) =
                        self.state
                            .objects
                            .get(handle)
                            .cloned()
                            .ok_or(VmError::Runtime(format!(
                                "dangling field name handle {handle}"
                            )))?
                    else {
                        return Err(VmError::Runtime(
                            "create_struct field names must be strings".into(),
                        ));
                    };
                    fields.push(fp_core::ast::ValueField::new(
                        fp_core::ast::Ident::new(field_name.value),
                        pair[1].value.clone(),
                    ));
                }
                let handle = self.state.objects.len() as i64;
                self.state
                    .objects
                    .push(Value::Structural(fp_core::ast::ValueStructural::new(
                        fields,
                    )));
                Ok(result(Value::Pointer(fp_core::ast::ValuePointer::managed(
                    handle,
                ))))
            }
            _ => Err(VmError::Runtime(format!("unknown bc intrinsic: {name}"))),
        }
    }

    fn bc_integer_arg(&self, name: &str, args: &[TypedValue], index: usize) -> LirResult<u64> {
        let arg = args.get(index).ok_or_else(|| {
            VmError::Runtime(format!(
                "bytecode intrinsic '{name}' is missing argument {index}"
            ))
        })?;
        self.integer_value(arg)
    }

    fn container_handle(&self, name: &str, value: &TypedValue) -> LirResult<usize> {
        let handle = match &value.value {
            Value::Pointer(pointer) if pointer.value >= 0 => pointer.value as u64,
            Value::Pointer(_) => {
                return Err(VmError::Runtime(format!(
                    "invalid container handle for {name}"
                )));
            }
            _ => self.integer_value(value)?,
        };
        usize::try_from(handle)
            .map_err(|_| VmError::Runtime(format!("invalid container handle for {name}")))
    }

    fn require_bc_arity(name: &str, args: &[TypedValue], expected: usize) -> LirResult<()> {
        if args.len() == expected {
            return Ok(());
        }
        Err(VmError::Runtime(format!(
            "bytecode intrinsic `{name}` expects {expected} arguments, got {}",
            args.len()
        )))
    }

    fn indexed_value(&self, key: &TypedValue, values: &[Value], name: &str) -> LirResult<Value> {
        let index = self.bc_integer_arg(name, std::slice::from_ref(key), 0)? as usize;
        values
            .get(index)
            .cloned()
            .ok_or_else(|| VmError::Runtime(format!("index {index} out of bounds")))
    }

    fn struct_field_by_name<'a>(
        &self,
        fields: &'a [fp_core::ast::ValueField],
        key: &TypedValue,
        name: &str,
    ) -> LirResult<&'a fp_core::ast::ValueField> {
        let handle = self.container_handle(name, key)?;
        let Value::String(field_name) = self
            .state
            .objects
            .get(handle)
            .cloned()
            .ok_or(VmError::Runtime("dangling field name handle".into()))?
        else {
            return Err(VmError::Runtime("struct field key must be a string".into()));
        };
        fields
            .iter()
            .find(|field| field.name.name == field_name.value)
            .ok_or_else(|| VmError::Runtime(format!("unknown struct field '{}'", field_name.value)))
    }

    fn store_value_at(&mut self, addr: u64, ty: &LirType, value: &Value) -> LirResult<()> {
        match ty {
            LirType::Struct { fields, .. } => {
                let layout = self
                    .data_layout
                    .struct_layout(ty)
                    .map_err(|error| VmError::Runtime(error.to_string()))?
                    .ok_or_else(|| VmError::Runtime(format!("missing layout for {ty:?}")))?;
                for (index, field_ty) in fields.iter().enumerate() {
                    let field = Self::aggregate_field(value, index)?;
                    self.store_value_at(addr + layout.field_offsets[index], field_ty, field)?;
                }
                Ok(())
            }
            LirType::Array(elem_ty, count) => {
                let stride = self
                    .data_layout
                    .size_of(elem_ty)
                    .map_err(|error| VmError::Runtime(error.to_string()))?;
                for index in 0..*count as usize {
                    let element = Self::aggregate_field(value, index)?;
                    self.store_value_at(addr + stride * index as u64, elem_ty, element)?;
                }
                Ok(())
            }
            LirType::Vector(elem_ty, count) => {
                let stride = self
                    .data_layout
                    .size_of(elem_ty)
                    .map_err(|error| VmError::Runtime(error.to_string()))?;
                for index in 0..*count as usize {
                    let element = Self::aggregate_field(value, index)?;
                    self.store_value_at(addr + stride * index as u64, elem_ty, element)?;
                }
                Ok(())
            }
            _ => {
                let raw = self.encode_storage_word(value.clone(), ty)?;
                mem_store(&mut self.state.mem, addr, raw, ty)
            }
        }
    }

    fn load_value_at(&self, addr: u64, ty: &LirType) -> LirResult<Value> {
        match ty {
            LirType::Struct { fields, .. } => {
                let layout = self
                    .data_layout
                    .struct_layout(ty)
                    .map_err(|error| VmError::Runtime(error.to_string()))?
                    .ok_or_else(|| VmError::Runtime(format!("missing layout for {ty:?}")))?;
                let mut values = Vec::with_capacity(fields.len());
                for (index, field_ty) in fields.iter().enumerate() {
                    values.push(self.load_value_at(addr + layout.field_offsets[index], field_ty)?);
                }
                Ok(Value::Tuple(ValueTuple::new(values)))
            }
            LirType::Array(elem_ty, count) => {
                let stride = self
                    .data_layout
                    .size_of(elem_ty)
                    .map_err(|error| VmError::Runtime(error.to_string()))?;
                let mut values = Vec::with_capacity(*count as usize);
                for index in 0..*count as usize {
                    values.push(self.load_value_at(addr + stride * index as u64, elem_ty)?);
                }
                Ok(Value::List(ValueList::new(values)))
            }
            LirType::Vector(elem_ty, count) => {
                let stride = self
                    .data_layout
                    .size_of(elem_ty)
                    .map_err(|error| VmError::Runtime(error.to_string()))?;
                let mut values = Vec::with_capacity(*count as usize);
                for index in 0..*count as usize {
                    values.push(self.load_value_at(addr + stride * index as u64, elem_ty)?);
                }
                Ok(Value::List(ValueList::new(values)))
            }
            _ => {
                let raw = mem_load(&self.state.mem, addr, ty)?;
                self.decode_scalar(raw, ty)
            }
        }
    }

    fn aggregate_field(value: &Value, index: usize) -> LirResult<&Value> {
        match value {
            Value::Tuple(tuple) => tuple
                .values
                .get(index)
                .ok_or_else(|| VmError::Runtime(format!("aggregate field {index} out of bounds"))),
            Value::List(list) => list
                .values
                .get(index)
                .ok_or_else(|| VmError::Runtime(format!("aggregate field {index} out of bounds"))),
            Value::Struct(structure) => structure
                .structural
                .fields
                .get(index)
                .map(|field| &field.value)
                .ok_or_else(|| VmError::Runtime(format!("aggregate field {index} out of bounds"))),
            Value::Structural(structure) => structure
                .fields
                .get(index)
                .map(|field| &field.value)
                .ok_or_else(|| VmError::Runtime(format!("aggregate field {index} out of bounds"))),
            _ => Err(VmError::Runtime(format!(
                "expected aggregate, found {value:?}"
            ))),
        }
    }

    fn decode_scalar(&self, raw: u64, ty: &LirType) -> LirResult<Value> {
        let (bits, signed) = lir_type_info(ty);
        match ty {
            LirType::F32 => Ok(Value::decimal(f32::from_bits(raw as u32) as f64)),
            LirType::F64 => Ok(Value::decimal(f64::from_bits(raw))),
            // `type` values use the pointer-sized `Ptr(Void)` ABI. Their
            // payloads live in the interpreter object pool, so recover the
            // typed value on a load instead of exposing its pool handle as
            // an ordinary pointer.
            LirType::Ptr(pointee) if matches!(pointee.as_ref(), LirType::Void) => {
                match self.state.objects.get(raw as usize) {
                    Some(Value::Type(value)) => Ok(Value::Type(value.clone())),
                    _ => Ok(Value::Pointer(fp_core::ast::ValuePointer::managed(raw as i64))),
                }
            }
            LirType::Ptr(_) => Ok(Value::Pointer(fp_core::ast::ValuePointer::managed(
                raw as i64,
            ))),
            LirType::Void => Ok(Value::unit()),
            _ => Ok(decode_integer(raw, signed, bits)),
        }
    }
}
