use super::*;

pub(super) fn normalize_program_for_target(program: &mut AsmProgram) {
    intern_string_constants(program);
    normalize_syscall_conventions_for_target(program);
    match program.target.architecture {
        AsmArchitecture::X86_64 => normalize_program_for_x86_64(program),
        AsmArchitecture::Aarch64 => normalize_program_for_aarch64(program),
        _ => normalize_program_generic(program),
    }
}

pub(super) fn intern_string_constants(program: &mut AsmProgram) {
    #[derive(Default)]
    struct InternContext {
        seen: HashMap<String, Name>,
        globals: Vec<AsmGlobal>,
        next_id: u32,
    }

    impl InternContext {
        fn intern_cstring(&mut self, text: &str) -> Name {
            if let Some(name) = self.seen.get(text) {
                return name.clone();
            }

            self.next_id += 1;
            let name = Name::new(format!("fp_str_{}", self.next_id));

            let mut bytes = Vec::with_capacity(text.len() + 1);
            bytes.extend_from_slice(text.as_bytes());
            bytes.push(0);
            let ty = AsmType::Array(Box::new(AsmType::I8), bytes.len() as u64);
            self.globals.push(AsmGlobal {
                name: name.clone(),
                ty,
                initializer: Some(AsmConstant::Bytes(bytes)),
                relocations: Vec::new(),
                section: Some(".rodata".to_string()),
                linkage: Linkage::Private,
                visibility: Visibility::Default,
                alignment: Some(1),
                is_constant: true,
            });
            self.seen.insert(text.to_string(), name.clone());
            name
        }
    }

    fn rewrite_value(value: &mut AsmValue, ctx: &mut InternContext) {
        match value {
            AsmValue::Constant(constant) => rewrite_constant(constant, ctx),
            AsmValue::Address(address) => {
                if let Some(base) = address.base.as_deref_mut() {
                    rewrite_value(base, ctx);
                }
                if let Some(index) = address.index.as_deref_mut() {
                    rewrite_value(index, ctx);
                }
                if let Some(segment) = address.segment.as_deref_mut() {
                    rewrite_value(segment, ctx);
                }
            }
            AsmValue::Comparison(comparison) => {
                rewrite_value(&mut comparison.lhs, ctx);
                rewrite_value(&mut comparison.rhs, ctx);
            }
            _ => {}
        }
    }

    fn rewrite_constant(constant: &mut AsmConstant, ctx: &mut InternContext) {
        match constant {
            AsmConstant::String(text) => {
                let symbol = ctx.intern_cstring(text);
                *constant =
                    AsmConstant::GlobalRef(symbol, AsmType::Ptr(Box::new(AsmType::I8)), Vec::new());
            }
            AsmConstant::Array(values, _) | AsmConstant::Struct(values, _) => {
                for value in values {
                    rewrite_constant(value, ctx);
                }
            }
            _ => {}
        }
    }

    fn rewrite_instruction(kind: &mut AsmInstructionKind, ctx: &mut InternContext) {
        match kind {
            AsmInstructionKind::Nop => {}
            AsmInstructionKind::Add(lhs, rhs)
            | AsmInstructionKind::Sub(lhs, rhs)
            | AsmInstructionKind::Mul(lhs, rhs)
            | AsmInstructionKind::Div(lhs, rhs)
            | AsmInstructionKind::Rem(lhs, rhs)
            | AsmInstructionKind::And(lhs, rhs)
            | AsmInstructionKind::Or(lhs, rhs)
            | AsmInstructionKind::Xor(lhs, rhs)
            | AsmInstructionKind::Shl(lhs, rhs)
            | AsmInstructionKind::Shr(lhs, rhs)
            | AsmInstructionKind::Eq(lhs, rhs)
            | AsmInstructionKind::Ne(lhs, rhs)
            | AsmInstructionKind::Lt(lhs, rhs)
            | AsmInstructionKind::Le(lhs, rhs)
            | AsmInstructionKind::Gt(lhs, rhs)
            | AsmInstructionKind::Ge(lhs, rhs)
            | AsmInstructionKind::Ult(lhs, rhs)
            | AsmInstructionKind::Ule(lhs, rhs)
            | AsmInstructionKind::Ugt(lhs, rhs)
            | AsmInstructionKind::Uge(lhs, rhs)
            | AsmInstructionKind::ZipLow { lhs, rhs, .. } => {
                rewrite_value(lhs, ctx);
                rewrite_value(rhs, ctx);
            }
            AsmInstructionKind::Not(value)
            | AsmInstructionKind::PtrToInt(value)
            | AsmInstructionKind::IntToPtr(value)
            | AsmInstructionKind::Freeze(value)
            | AsmInstructionKind::ExtractLane { vector: value, .. } => {
                rewrite_value(value, ctx);
            }
            AsmInstructionKind::Load { address, .. } => {
                rewrite_value(address, ctx);
            }
            AsmInstructionKind::Store { value, address, .. } => {
                rewrite_value(value, ctx);
                rewrite_value(address, ctx);
            }
            AsmInstructionKind::Alloca { size, .. } => {
                rewrite_value(size, ctx);
            }
            AsmInstructionKind::SymbolAddress { .. } => {}
            AsmInstructionKind::GetElementPtr { ptr, indices, .. } => {
                rewrite_value(ptr, ctx);
                for index in indices {
                    rewrite_value(index, ctx);
                }
            }
            AsmInstructionKind::Bitcast(value, _)
            | AsmInstructionKind::Trunc(value, _)
            | AsmInstructionKind::ZExt(value, _)
            | AsmInstructionKind::SExt(value, _)
            | AsmInstructionKind::FPExt(value, _)
            | AsmInstructionKind::FPTrunc(value, _)
            | AsmInstructionKind::FPToUI(value, _)
            | AsmInstructionKind::FPToSI(value, _)
            | AsmInstructionKind::UIToFP(value, _)
            | AsmInstructionKind::SIToFP(value, _) => {
                rewrite_value(value, ctx);
            }
            AsmInstructionKind::ExtractValue { aggregate, .. } => {
                rewrite_value(aggregate, ctx);
            }
            AsmInstructionKind::InsertValue {
                aggregate, element, ..
            } => {
                rewrite_value(aggregate, ctx);
                rewrite_value(element, ctx);
            }
            AsmInstructionKind::Call { function, args, .. } => {
                rewrite_value(function, ctx);
                let preserve_strings = matches!(
                    function,
                    AsmValue::Function(name)
                        if matches!(
                            name.as_str(),
                            "printf" | "fprintf" | "sprintf" | "snprintf" | "dprintf" | "vprintf"
                                | "vfprintf" | "vsprintf" | "vsnprintf" | "vdprintf"
                        )
                );
                for arg in args {
                    if preserve_strings {
                        if matches!(arg, AsmValue::Constant(AsmConstant::String(_))) {
                            continue;
                        }
                    }
                    rewrite_value(arg, ctx);
                }
            }
            AsmInstructionKind::IntrinsicCall { args, .. } => {
                for arg in args {
                    rewrite_value(arg, ctx);
                }
            }
            AsmInstructionKind::SextOrTrunc(value, _) => {
                rewrite_value(value, ctx);
            }
            AsmInstructionKind::Phi { incoming } => {
                for (value, _) in incoming {
                    rewrite_value(value, ctx);
                }
            }
            AsmInstructionKind::Select {
                condition,
                if_true,
                if_false,
            } => {
                rewrite_value(condition, ctx);
                rewrite_value(if_true, ctx);
                rewrite_value(if_false, ctx);
            }
            AsmInstructionKind::InlineAsm { inputs, .. } => {
                for input in inputs {
                    rewrite_value(input, ctx);
                }
            }
            AsmInstructionKind::LandingPad {
                personality,
                clauses,
                ..
            } => {
                if let Some(personality) = personality {
                    rewrite_value(personality, ctx);
                }
                for clause in clauses {
                    match clause {
                        AsmLandingPadClause::Catch(value) => rewrite_value(value, ctx),
                        AsmLandingPadClause::Filter(values) => {
                            for value in values {
                                rewrite_value(value, ctx);
                            }
                        }
                    }
                }
            }
            AsmInstructionKind::Syscall { number, args, .. } => {
                rewrite_value(number, ctx);
                for arg in args {
                    rewrite_value(arg, ctx);
                }
            }
            AsmInstructionKind::SysOp(op) => match op {
                fp_core::asmir::AsmSysOp::Exit { code } => rewrite_value(code, ctx),
                fp_core::asmir::AsmSysOp::GetPid | fp_core::asmir::AsmSysOp::GetTid => {}
                fp_core::asmir::AsmSysOp::Dlopen { path, flags } => {
                    rewrite_value(path, ctx);
                    rewrite_value(flags, ctx);
                }
                fp_core::asmir::AsmSysOp::Dlsym { handle, symbol } => {
                    rewrite_value(handle, ctx);
                    rewrite_value(symbol, ctx);
                }
                fp_core::asmir::AsmSysOp::Dlclose { handle } => rewrite_value(handle, ctx),
                fp_core::asmir::AsmSysOp::Unlink { path }
                | fp_core::asmir::AsmSysOp::Rmdir { path } => rewrite_value(path, ctx),
                fp_core::asmir::AsmSysOp::Mkdir { path, mode } => {
                    rewrite_value(path, ctx);
                    rewrite_value(mode, ctx);
                }
                fp_core::asmir::AsmSysOp::Rename { from, to } => {
                    rewrite_value(from, ctx);
                    rewrite_value(to, ctx);
                }
                fp_core::asmir::AsmSysOp::Access { path, mode } => {
                    rewrite_value(path, ctx);
                    rewrite_value(mode, ctx);
                }
                fp_core::asmir::AsmSysOp::Write { fd, buffer, len }
                | fp_core::asmir::AsmSysOp::Read { fd, buffer, len } => {
                    rewrite_value(fd, ctx);
                    rewrite_value(buffer, ctx);
                    rewrite_value(len, ctx);
                }
                fp_core::asmir::AsmSysOp::Close { fd } => rewrite_value(fd, ctx),
                fp_core::asmir::AsmSysOp::Open {
                    path, flags, mode, ..
                } => {
                    rewrite_value(path, ctx);
                    rewrite_value(flags, ctx);
                    rewrite_value(mode, ctx);
                }
                fp_core::asmir::AsmSysOp::Seek { fd, offset, whence } => {
                    rewrite_value(fd, ctx);
                    rewrite_value(offset, ctx);
                    rewrite_value(whence, ctx);
                }
                fp_core::asmir::AsmSysOp::Mmap {
                    addr,
                    len,
                    prot,
                    flags,
                    fd,
                    offset,
                } => {
                    rewrite_value(addr, ctx);
                    rewrite_value(len, ctx);
                    rewrite_value(prot, ctx);
                    rewrite_value(flags, ctx);
                    rewrite_value(fd, ctx);
                    rewrite_value(offset, ctx);
                }
                fp_core::asmir::AsmSysOp::Munmap { addr, len } => {
                    rewrite_value(addr, ctx);
                    rewrite_value(len, ctx);
                }
                fp_core::asmir::AsmSysOp::Opendir { path } => rewrite_value(path, ctx),
                fp_core::asmir::AsmSysOp::Readdir { dir, .. }
                | fp_core::asmir::AsmSysOp::Closedir { dir } => rewrite_value(dir, ctx),
            },
            AsmInstructionKind::Splat { value, .. } => rewrite_value(value, ctx),
            AsmInstructionKind::BuildVector { elements } => {
                for element in elements {
                    rewrite_value(element, ctx);
                }
            }
            AsmInstructionKind::InsertLane { vector, value, .. } => {
                rewrite_value(vector, ctx);
                rewrite_value(value, ctx);
            }
            AsmInstructionKind::Unreachable => {}
        }
    }

    fn rewrite_terminator(terminator: &mut AsmTerminator, ctx: &mut InternContext) {
        match terminator {
            AsmTerminator::Return(value) => {
                if let Some(value) = value {
                    rewrite_value(value, ctx);
                }
            }
            AsmTerminator::CondBr { condition, .. } => rewrite_value(condition, ctx),
            AsmTerminator::Switch { value, .. } => rewrite_value(value, ctx),
            AsmTerminator::IndirectBr { address, .. } => rewrite_value(address, ctx),
            AsmTerminator::Invoke { function, args, .. } => {
                rewrite_value(function, ctx);
                for arg in args {
                    rewrite_value(arg, ctx);
                }
            }
            AsmTerminator::Resume(value)
            | AsmTerminator::CleanupRet {
                cleanup_pad: value, ..
            }
            | AsmTerminator::CatchRet {
                catch_pad: value, ..
            } => rewrite_value(value, ctx),
            AsmTerminator::CatchSwitch { parent_pad, .. } => {
                if let Some(value) = parent_pad {
                    rewrite_value(value, ctx);
                }
            }
            AsmTerminator::Br(..) | AsmTerminator::Unreachable => {}
        }
    }

    let mut ctx = InternContext::default();

    for global in &mut program.globals {
        if let Some(initializer) = &mut global.initializer {
            rewrite_constant(initializer, &mut ctx);
        }
    }

    for function in &mut program.functions {
        for block in &mut function.basic_blocks {
            for instruction in &mut block.instructions {
                rewrite_instruction(&mut instruction.kind, &mut ctx);
            }
            rewrite_terminator(&mut block.terminator, &mut ctx);
        }
    }

    program.globals.extend(ctx.globals);
}

pub(super) fn normalize_syscall_conventions_for_target(program: &mut AsmProgram) {
    let Some(convention) = syscall_convention_for_target(&program.target) else {
        return;
    };

    for function in &mut program.functions {
        for block in &mut function.basic_blocks {
            let mut last_constants: HashMap<u32, AsmConstant> = HashMap::new();
            for instruction in &mut block.instructions {
                if let AsmInstructionKind::Freeze(AsmValue::Constant(constant)) = &instruction.kind
                {
                    last_constants.insert(instruction.id, constant.clone());
                }

                if let AsmInstructionKind::Syscall {
                    convention: c,
                    number,
                    ..
                } = &mut instruction.kind
                {
                    let old_convention = *c;
                    *c = convention;

                    if matches!(
                        (old_convention, convention),
                        (
                            AsmSyscallConvention::DarwinX86_64,
                            AsmSyscallConvention::DarwinAarch64
                        ) | (
                            AsmSyscallConvention::DarwinAarch64,
                            AsmSyscallConvention::DarwinX86_64
                        )
                    ) {
                        let constant_number = match number {
                            AsmValue::Constant(AsmConstant::UInt(value, ty)) => {
                                Some((*value as i64, ty.clone()))
                            }
                            AsmValue::Constant(AsmConstant::Int(value, ty)) => {
                                Some((*value, ty.clone()))
                            }
                            AsmValue::Register(id) => {
                                last_constants.get(id).and_then(|constant| match constant {
                                    AsmConstant::UInt(value, ty) => {
                                        Some((*value as i64, ty.clone()))
                                    }
                                    AsmConstant::Int(value, ty) => Some((*value, ty.clone())),
                                    _ => None,
                                })
                            }
                            _ => None,
                        };

                        if let Some((value, ty)) = constant_number {
                            let translated = match (old_convention, convention) {
                                (
                                    AsmSyscallConvention::DarwinX86_64,
                                    AsmSyscallConvention::DarwinAarch64,
                                ) => value.saturating_sub(0x0200_0000),
                                (
                                    AsmSyscallConvention::DarwinAarch64,
                                    AsmSyscallConvention::DarwinX86_64,
                                ) => value.saturating_add(0x0200_0000),
                                _ => value,
                            };

                            if translated != value {
                                *number = AsmValue::Constant(AsmConstant::Int(translated, ty));
                            }
                        }
                    }
                }
            }
        }
    }
}

pub(super) fn syscall_convention_for_target(target: &AsmTarget) -> Option<AsmSyscallConvention> {
    use fp_core::asmir::AsmObjectFormat;

    match (&target.architecture, &target.object_format) {
        (AsmArchitecture::X86_64, AsmObjectFormat::Elf) => Some(AsmSyscallConvention::LinuxX86_64),
        (AsmArchitecture::X86_64, AsmObjectFormat::MachO) => {
            Some(AsmSyscallConvention::DarwinX86_64)
        }
        (AsmArchitecture::Aarch64, AsmObjectFormat::Elf) => {
            Some(AsmSyscallConvention::LinuxAarch64)
        }
        (AsmArchitecture::Aarch64, AsmObjectFormat::MachO) => {
            Some(AsmSyscallConvention::DarwinAarch64)
        }
        _ => None,
    }
}

pub fn normalize_for_target(program: &mut AsmProgram) {
    normalize_program_for_target(program);
}

pub(super) fn normalize_program_generic(program: &mut AsmProgram) {
    // Cloned up front: `program.functions` is borrowed mutably below, so it
    // can't also be reached through `&program` at the same time to merge
    // this in per-function via `merged_register_types`.
    let physical_register_types = program.physical_register_types.clone();
    for function in &mut program.functions {
        // Reconstructed types first so a real instruction-result type wins
        // on any collision, matching `merged_register_types`.
        let mut register_types = physical_register_types.clone();
        register_types.extend(build_operand_type_map(function));
        for block in &mut function.basic_blocks {
            for instruction in &mut block.instructions {
                instruction.opcode = AsmOpcode::Generic(generic_opcode(&instruction.kind));
                instruction.operands = generic_operands(
                    instruction.id,
                    &instruction.kind,
                    Some(&instruction.ty),
                    &register_types,
                );
            }
        }
    }
}
