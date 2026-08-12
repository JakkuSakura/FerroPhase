use fp_core::asmir::{
    AsmArchitecture, AsmAttr, AsmConstant, AsmGenericOpcode, AsmGlobal, AsmOpcode, AsmOperand,
    AsmProgram, AsmRegister, AsmSyscallConvention, AsmTarget, AsmTerminator, AsmType,
    AsmVirtualRegId, Linkage, Name, OperandAccess, Visibility,
};
use std::collections::HashMap;

pub fn normalize_for_target(program: &mut AsmProgram) {
    intern_string_constants(program);
    normalize_syscall_conventions_for_target(program);
}

fn intern_string_constants(program: &mut AsmProgram) {
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

    fn rewrite_operand(operand: &mut AsmOperand, ctx: &mut InternContext) {
        match operand {
            AsmOperand::Constant(constant) => rewrite_constant(constant, ctx),
            AsmOperand::SysOp(op) => rewrite_sysop(op, ctx),
            _ => {}
        }
    }

    fn rewrite_sysop(op: &mut fp_core::asmir::AsmSysOp, ctx: &mut InternContext) {
        use fp_core::asmir::AsmSysOp;
        match op {
            AsmSysOp::Exit { code } => rewrite_operand(code, ctx),
            AsmSysOp::GetPid | AsmSysOp::GetTid => {}
            AsmSysOp::Dlopen { path, flags } => {
                rewrite_operand(path, ctx);
                rewrite_operand(flags, ctx);
            }
            AsmSysOp::Dlsym { handle, symbol } => {
                rewrite_operand(handle, ctx);
                rewrite_operand(symbol, ctx);
            }
            AsmSysOp::Dlclose { handle } => rewrite_operand(handle, ctx),
            AsmSysOp::Unlink { path }
            | AsmSysOp::Rmdir { path }
            | AsmSysOp::Opendir { path } => rewrite_operand(path, ctx),
            AsmSysOp::Mkdir { path, mode } => {
                rewrite_operand(path, ctx);
                rewrite_operand(mode, ctx);
            }
            AsmSysOp::Rename { from, to } => {
                rewrite_operand(from, ctx);
                rewrite_operand(to, ctx);
            }
            AsmSysOp::Access { path, mode } => {
                rewrite_operand(path, ctx);
                rewrite_operand(mode, ctx);
            }
            AsmSysOp::Write { fd, buffer, len } | AsmSysOp::Read { fd, buffer, len } => {
                rewrite_operand(fd, ctx);
                rewrite_operand(buffer, ctx);
                rewrite_operand(len, ctx);
            }
            AsmSysOp::Close { fd } => rewrite_operand(fd, ctx),
            AsmSysOp::Open {
                path, flags, mode, ..
            } => {
                rewrite_operand(path, ctx);
                rewrite_operand(flags, ctx);
                rewrite_operand(mode, ctx);
            }
            AsmSysOp::Seek { fd, offset, whence } => {
                rewrite_operand(fd, ctx);
                rewrite_operand(offset, ctx);
                rewrite_operand(whence, ctx);
            }
            AsmSysOp::Mmap {
                addr,
                len,
                prot,
                flags,
                fd,
                offset,
            } => {
                rewrite_operand(addr, ctx);
                rewrite_operand(len, ctx);
                rewrite_operand(prot, ctx);
                rewrite_operand(flags, ctx);
                rewrite_operand(fd, ctx);
                rewrite_operand(offset, ctx);
            }
            AsmSysOp::Munmap { addr, len } => {
                rewrite_operand(addr, ctx);
                rewrite_operand(len, ctx);
            }
            AsmSysOp::Readdir { dir, .. } | AsmSysOp::Closedir { dir } => {
                rewrite_operand(dir, ctx)
            }
        }
    }

    fn rewrite_terminator_operands(terminator: &mut AsmTerminator, ctx: &mut InternContext) {
        match terminator {
            AsmTerminator::Return(value) => {
                if let Some(value) = value {
                    rewrite_operand(value, ctx);
                }
            }
            AsmTerminator::CondBr { condition, .. } => rewrite_operand(condition, ctx),
            AsmTerminator::Switch { value, .. } => rewrite_operand(value, ctx),
            AsmTerminator::IndirectBr { address, .. } => rewrite_operand(address, ctx),
            AsmTerminator::Invoke { function, args, .. } => {
                rewrite_operand(function, ctx);
                for arg in args {
                    rewrite_operand(arg, ctx);
                }
            }
            AsmTerminator::Resume(value)
            | AsmTerminator::CleanupRet {
                cleanup_pad: value, ..
            }
            | AsmTerminator::CatchRet {
                catch_pad: value, ..
            } => rewrite_operand(value, ctx),
            AsmTerminator::CatchSwitch { parent_pad, .. } => {
                if let Some(value) = parent_pad {
                    rewrite_operand(value, ctx);
                }
            }
            AsmTerminator::Br(..) | AsmTerminator::Unreachable => {}
        }
    }

    const VARIADIC_STRING_PRESERVING_CALLS: &[&str] = &[
        "printf", "fprintf", "sprintf", "snprintf", "dprintf", "vprintf", "vfprintf", "vsprintf",
        "vsnprintf", "vdprintf",
    ];

    let mut ctx = InternContext::default();

    for global in &mut program.globals {
        if let Some(initializer) = &mut global.initializer {
            rewrite_constant(initializer, &mut ctx);
        }
    }

    for function in &mut program.functions {
        for block in &mut function.basic_blocks {
            for instruction in &mut block.instructions {
                let preserve_strings = instruction
                    .call_target_and_args()
                    .map(|(target, _)| {
                        matches!(target, AsmOperand::Symbol(name)
                            if VARIADIC_STRING_PRESERVING_CALLS.contains(&name.as_str()))
                    })
                    .unwrap_or(false);
                let arg_start = instruction
                    .call_target_and_args()
                    .map(|(_, args)| instruction.operands.len() - args.len())
                    .unwrap_or(usize::MAX);

                for (idx, operand) in instruction.operands.iter_mut().enumerate() {
                    if preserve_strings
                        && idx >= arg_start
                        && matches!(operand, AsmOperand::Constant(AsmConstant::String(_)))
                    {
                        continue;
                    }
                    rewrite_operand(operand, &mut ctx);
                }
            }
            rewrite_terminator_operands(&mut block.terminator, &mut ctx);
        }
    }

    program.globals.extend(ctx.globals);
}

fn normalize_syscall_conventions_for_target(program: &mut AsmProgram) {
    let Some(convention) = syscall_convention_for_target(&program.target) else {
        return;
    };

    for function in &mut program.functions {
        for block in &mut function.basic_blocks {
            // `Syscall`'s own vreg id isn't visible here as a HashMap key
            // convention anymore, so track "last constant frozen into each
            // virtual register" directly off the register table id.
            let mut last_constants: HashMap<AsmVirtualRegId, AsmConstant> = HashMap::new();
            for instruction in &mut block.instructions {
                if matches!(instruction.opcode, AsmOpcode::Generic(AsmGenericOpcode::Freeze)) {
                    let result_id = instruction.operands.iter().find_map(|op| match op {
                        AsmOperand::Register {
                            reg: AsmRegister::Virtual(id),
                            access,
                        } if *access != OperandAccess::Read => Some(*id),
                        _ => None,
                    });
                    let constant = instruction.operands.iter().find_map(|op| match op {
                        AsmOperand::Constant(constant) => Some(constant.clone()),
                        _ => None,
                    });
                    if let (Some(id), Some(constant)) = (result_id, constant) {
                        last_constants.insert(id, constant);
                    }
                }

                if !matches!(instruction.opcode, AsmOpcode::Generic(AsmGenericOpcode::Syscall)) {
                    continue;
                }
                let Some(attr_idx) = instruction.operands.iter().position(|op| {
                    matches!(op, AsmOperand::Attr(AsmAttr::SyscallConvention(_)))
                }) else {
                    continue;
                };
                let old_convention = match &instruction.operands[attr_idx] {
                    AsmOperand::Attr(AsmAttr::SyscallConvention(cc)) => *cc,
                    _ => unreachable!(),
                };
                instruction.operands[attr_idx] =
                    AsmOperand::Attr(AsmAttr::SyscallConvention(convention));

                if !matches!(
                    (old_convention, convention),
                    (
                        AsmSyscallConvention::DarwinX86_64,
                        AsmSyscallConvention::DarwinAarch64
                    ) | (
                        AsmSyscallConvention::DarwinAarch64,
                        AsmSyscallConvention::DarwinX86_64
                    )
                ) {
                    continue;
                }

                // `number` is the operand immediately following the
                // SyscallConvention attr (see the Syscall operand schema in
                // `select_instruction`).
                let Some(number_operand) = instruction.operands.get_mut(attr_idx + 1) else {
                    continue;
                };
                let constant_number = match number_operand {
                    AsmOperand::Constant(AsmConstant::UInt(value, ty)) => {
                        Some((*value as i64, ty.clone()))
                    }
                    AsmOperand::Constant(AsmConstant::Int(value, ty)) => Some((*value, ty.clone())),
                    AsmOperand::Register {
                        reg: AsmRegister::Virtual(id),
                        ..
                    } => last_constants.get(id).and_then(|constant| match constant {
                        AsmConstant::UInt(value, ty) => Some((*value as i64, ty.clone())),
                        AsmConstant::Int(value, ty) => Some((*value, ty.clone())),
                        _ => None,
                    }),
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
                        *number_operand = AsmOperand::Constant(AsmConstant::Int(translated, ty));
                    }
                }
            }
        }
    }
}

fn syscall_convention_for_target(target: &AsmTarget) -> Option<AsmSyscallConvention> {
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
