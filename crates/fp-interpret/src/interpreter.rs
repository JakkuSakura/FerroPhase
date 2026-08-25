use super::*;

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
        }
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
        }
        self.program = Some(program);
        Ok(())
    }

    /// Test-only convenience: wraps a single flat `LirBlob` as a
    /// one-package program, loads it, and runs its `main` function by
    /// name. Not exposed publicly — a real caller always knows its own
    /// package id and entry `DefId` and should use `run_entrypoint`.
    #[cfg(test)]
    pub(super) fn run_main(&mut self, program: &LirBlob) -> LirResult<Value> {
        self.run_main_with_package(program, PackageId::new(""))
    }

    #[cfg(test)]
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
            let raw = self.encode_value(&typed)?;
            self.write_typed_register(reg, typed)?;
            if let Some(local) = argument_locals.get(i) {
                self.state
                    .mem
                    .store_u64(self.state.local_addr(local.id)?, raw)?;
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
                self.exec_instruction(instr)?;
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
                LirTerminator::Unreachable => break Err(VmError::Runtime("unreachable".into())),
                other => break Err(VmError::Runtime(format!("terminator: {other:?}"))),
            }
        };
        self.state.pop_frame();
        self.state.regs.gpr = saved_registers;
        self.register_values = saved_register_values;
        result
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
                    let struct_name = self.resolve_string_value(name)?;
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
                    let type_name = self.resolve_string_value(name)?;
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
                    let field_name_str = self.resolve_string_value(field_name)?;
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
            LirInstructionKind::InlineAsm { .. }
            | LirInstructionKind::LandingPad { .. }
            | LirInstructionKind::Freeze(_)
            | LirInstructionKind::ExecQuery(_) => Err(VmError::Runtime("unsupported".into())),
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
    /// arguments — reused here via the same `resolve_runtime_value` +
    /// `render_typed_value` pair, keyed by the argument's own `LirValue::ty`.
    fn render_str_argument(&self, val: &LirValue) -> LirResult<String> {
        let value = self.resolve_runtime_value(val, &val.ty)?;
        self.render_typed_value(&value, &val.ty)
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
                if func.is_declaration && func.calling_convention == CallingConvention::C {
                    let sig = lir_sig_to_ffi(&func.signature);
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

    /// Handles calling the closure `unionify(f)` returns — the only
    /// indirect-call shape this interpreter supports (see `handle_call`'s
    /// doc comment). Any other non-statically-known callee still errors
    /// with "indirect call"; this is deliberately not general first-class
    /// function/currying support.
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
            // Not a statically-known function reference — the only
            // supported indirect-call shape today is calling the closure
            // `unionify(f)` returns (`Value::UnionifyClosure`), never a
            // general first-class/curried function value.
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
            self.program
                .as_ref()
                .and_then(|program| match &package_id {
                    Some(package_id) => program.find_function(package_id, &name),
                    None => program.find_function_any_package(&name),
                })
                .cloned()
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

    fn call_intrinsic(
        &mut self,
        name: &str,
        args: &[TypedValue],
        result_ty: Option<&LirType>,
    ) -> LirResult<TypedValue> {
        if let Some(rest) = name.strip_prefix("__bc_") {
            return self.call_bc_intrinsic(rest, args, result_ty);
        }
        let result_ty = result_ty
            .ok_or_else(|| VmError::Runtime(format!("intrinsic '{name}' has no result type")))?;
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
                value: integer_value(0, lir_type_info(result_ty).1),
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
        let result_ty = result_ty.ok_or_else(|| {
            VmError::Runtime(format!("bytecode intrinsic '{name}' has no result type"))
        })?;
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
                let index = self.bc_integer_arg(name, args, 1)? as usize;
                let obj = self
                    .state
                    .objects
                    .get(handle)
                    .cloned()
                    .ok_or(VmError::Runtime(format!("dangling handle {handle}")))?;
                let element = match &obj {
                    Value::Tuple(t) => t.values.get(index).cloned(),
                    Value::List(l) => l.values.get(index).cloned(),
                    _ => return Err(VmError::Runtime("get on non-container".into())),
                }
                .ok_or(VmError::Runtime(format!("index {index} out of bounds")))?;
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
        let handle = self.integer_value(value)?;
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
            LirType::Ptr(_) => Ok(Value::Pointer(fp_core::ast::ValuePointer::managed(
                raw as i64,
            ))),
            LirType::Void => Ok(Value::unit()),
            _ => Ok(decode_integer(raw, signed, bits)),
        }
    }
}
