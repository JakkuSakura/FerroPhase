use super::*;

pub fn lower_sys_ops_for_target(program: &mut AsmProgram) -> Result<()> {
    if program.target.object_format == AsmObjectFormat::Coff
        || program.target.object_format == AsmObjectFormat::Pe
    {
        lower_sys_ops_to_windows_imports(program)
    } else {
        lower_sys_ops_to_unix_syscalls(program)
    }
}

pub(super) fn lower_sys_ops_to_unix_syscalls(program: &mut AsmProgram) -> Result<()> {
    let Some(target_convention) = target_syscall_convention(program) else {
        return Ok(());
    };

    let default_cc = program
        .target
        .default_calling_convention
        .clone()
        .unwrap_or(CallingConvention::C);
    let target_dirent_style = match program.target.object_format {
        AsmObjectFormat::MachO => PosixDirentStyle::Darwin,
        _ => PosixDirentStyle::Linux,
    };

    if target_dirent_style == PosixDirentStyle::Darwin
        && program
            .functions
            .iter()
            .filter(|f| !f.is_declaration)
            .flat_map(|f| f.basic_blocks.iter())
            .flat_map(|b| b.instructions.iter())
            .any(|inst| {
                matches!(
                    &inst.kind,
                    AsmInstructionKind::SysOp(AsmSysOp::Readdir {
                        dirent_style: PosixDirentStyle::Linux,
                        ..
                    })
                )
            })
    {
        inject_linux_readdir_shim(program, default_cc.clone())?;
    }

    for function in &mut program.functions {
        if function.is_declaration {
            continue;
        }

        for block in &mut function.basic_blocks {
            for inst in &mut block.instructions {
                let AsmInstructionKind::SysOp(op) = &inst.kind else {
                    continue;
                };

                match op {
                    AsmSysOp::Opendir { path } => {
                        inst.kind = AsmInstructionKind::Call {
                            function: AsmValue::Function("opendir".to_string()),
                            args: vec![path.clone()],
                            calling_convention: default_cc.clone(),
                            tail_call: false,
                        };
                        inst.opcode = AsmOpcode::Generic(AsmGenericOpcode::Call);
                        inst.ty = AsmType::Ptr(Box::new(AsmType::I8));
                    }
                    AsmSysOp::Readdir { dir, dirent_style } => {
                        let name = if *dirent_style != target_dirent_style {
                            "fp_linux_readdir"
                        } else {
                            "readdir"
                        };
                        inst.kind = AsmInstructionKind::Call {
                            function: AsmValue::Function(name.to_string()),
                            args: vec![dir.clone()],
                            calling_convention: default_cc.clone(),
                            tail_call: false,
                        };
                        inst.opcode = AsmOpcode::Generic(AsmGenericOpcode::Call);
                        inst.ty = AsmType::Ptr(Box::new(AsmType::I8));
                    }
                    AsmSysOp::Closedir { dir } => {
                        inst.kind = AsmInstructionKind::Call {
                            function: AsmValue::Function("closedir".to_string()),
                            args: vec![dir.clone()],
                            calling_convention: default_cc.clone(),
                            tail_call: false,
                        };
                        inst.opcode = AsmOpcode::Generic(AsmGenericOpcode::Call);
                        inst.ty = AsmType::I64;
                    }
                    _ => {
                        inst.kind = lower_system_api_to_syscall(op.clone(), target_convention);
                        inst.opcode = AsmOpcode::Generic(AsmGenericOpcode::Syscall);
                        inst.ty = AsmType::I64;
                    }
                }
            }
        }
    }
    Ok(())
}

pub(super) fn inject_linux_readdir_shim(
    program: &mut AsmProgram,
    cc: CallingConvention,
) -> Result<()> {
    if program
        .functions
        .iter()
        .any(|f| f.name.as_str() == "fp_linux_readdir")
    {
        return Ok(());
    }

    #[cfg(not(unix))]
    {
        let _ = (program, cc);
        return Err(Error::from("fp_linux_readdir shim requires a unix host"));
    }

    #[cfg(unix)]
    {
        use fp_core::asmir::{
            AsmBlock, AsmFunction, AsmFunctionSignature, AsmLocal, AsmTerminator,
        };
        use fp_core::lir::{Linkage, Name, Visibility};

        let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
        let null_ptr = AsmValue::Null(ptr_i8.clone());

        const LINUX_DIRENT_SIZE: u64 = 280;
        const LINUX_D_NAME_OFFSET: u64 = 19;
        const LINUX_D_INO_OFFSET: u64 = 0;
        const LINUX_D_RECLEN_OFFSET: u64 = 16;
        const LINUX_D_TYPE_OFFSET: u64 = 18;
        const LINUX_D_NAME_MAX: u64 = 255;

        let host_d_name_offset: u64 = core::mem::offset_of!(libc::dirent, d_name) as u64;
        let host_d_ino_offset: u64 = core::mem::offset_of!(libc::dirent, d_ino) as u64;
        let host_d_type_offset: u64 = core::mem::offset_of!(libc::dirent, d_type) as u64;

        let mut next_id: u32 = program
            .functions
            .iter()
            .flat_map(|f| f.basic_blocks.iter())
            .flat_map(|b| b.instructions.iter().map(|i| i.id))
            .max()
            .unwrap_or(0)
            .saturating_add(1);

        let call = |id: u32, name: &str, args: Vec<AsmValue>, ret: AsmType| AsmInstruction {
            id,
            opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
            kind: AsmInstructionKind::Call {
                function: AsmValue::Function(name.to_string()),
                args,
                calling_convention: cc.clone(),
                tail_call: false,
            },
            ty: ret,
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        };

        let add = |id: u32, lhs: AsmValue, rhs: AsmValue, ty: AsmType| AsmInstruction {
            id,
            opcode: AsmOpcode::Generic(AsmGenericOpcode::Add),
            kind: AsmInstructionKind::Add(lhs, rhs),
            ty: ty,
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        };

        let load = |id: u32, address: AsmValue, ty: AsmType| AsmInstruction {
            id,
            opcode: AsmOpcode::Generic(AsmGenericOpcode::Load),
            kind: AsmInstructionKind::Load {
                address,
                alignment: None,
                volatile: false,
            },
            ty: ty,
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        };

        let store = |id: u32, value: AsmValue, address: AsmValue| AsmInstruction {
            id,
            opcode: AsmOpcode::Generic(AsmGenericOpcode::Store),
            kind: AsmInstructionKind::Store {
                value,
                address,
                alignment: None,
                volatile: false,
            },
            ty: AsmType::Void,
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        };

        let eq = |id: u32, lhs: AsmValue, rhs: AsmValue| AsmInstruction {
            id,
            opcode: AsmOpcode::Generic(AsmGenericOpcode::Eq),
            kind: AsmInstructionKind::Eq(lhs, rhs),
            ty: AsmType::I1,
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        };

        let dir_local = AsmLocal {
            id: 0,
            ty: ptr_i8.clone(),
            name: Some("dir".to_string()),
            is_argument: true,
        };

        // entry:
        //   entry = readdir(dir)
        //   if entry == null { return null }
        //   out = malloc(LINUX_DIRENT_SIZE)
        //   memset(out, 0, LINUX_DIRENT_SIZE)
        //   out->d_ino = entry->d_ino
        //   out->d_reclen = LINUX_DIRENT_SIZE
        //   out->d_type = entry->d_type
        //   strncpy(out->d_name, entry->d_name, LINUX_D_NAME_MAX)
        //   return out

        let call_readdir_id = next_id;
        next_id += 1;
        let entry_ptr = AsmValue::Register(call_readdir_id);

        let is_null_id = next_id;
        next_id += 1;

        let alloc_id = next_id;
        next_id += 1;
        let out_ptr = AsmValue::Register(alloc_id);

        let entry_ino_addr_id = next_id;
        next_id += 1;
        let entry_ino_id = next_id;
        next_id += 1;

        let entry_type_addr_id = next_id;
        next_id += 1;
        let entry_type_id = next_id;
        next_id += 1;

        let out_ino_addr_id = next_id;
        next_id += 1;
        let out_reclen_addr_id = next_id;
        next_id += 1;
        let out_type_addr_id = next_id;
        next_id += 1;

        let out_name_ptr_id = next_id;
        next_id += 1;
        let entry_name_ptr_id = next_id;
        next_id += 1;

        let mut entry_insts = Vec::new();
        entry_insts.push(call(
            call_readdir_id,
            "readdir",
            vec![AsmValue::Local(dir_local.id)],
            ptr_i8.clone(),
        ));
        entry_insts.push(eq(is_null_id, entry_ptr.clone(), null_ptr.clone()));

        let entry_block = AsmBlock {
            id: 0,
            label: Some(Name::new("entry")),
            instructions: entry_insts,
            terminator: AsmTerminator::CondBr {
                condition: AsmValue::Register(is_null_id),
                if_true: 1,
                if_false: 2,
            },
            terminator_encoding: None,
            predecessors: Vec::new(),
            successors: vec![1, 2],
        };

        let null_block = AsmBlock {
            id: 1,
            label: Some(Name::new("return_null")),
            instructions: Vec::new(),
            terminator: AsmTerminator::Return(Some(null_ptr.clone())),
            terminator_encoding: None,
            predecessors: vec![0],
            successors: Vec::new(),
        };

        let mut alloc_insts = Vec::new();
        alloc_insts.push(call(
            alloc_id,
            "malloc",
            vec![AsmValue::Constant(AsmConstant::UInt(
                LINUX_DIRENT_SIZE,
                AsmType::I64,
            ))],
            ptr_i8.clone(),
        ));
        alloc_insts.push(call(
            next_id,
            "memset",
            vec![
                out_ptr.clone(),
                AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)),
                AsmValue::Constant(AsmConstant::UInt(LINUX_DIRENT_SIZE, AsmType::I64)),
            ],
            ptr_i8.clone(),
        ));
        next_id += 1;

        alloc_insts.push(add(
            entry_ino_addr_id,
            entry_ptr.clone(),
            AsmValue::Constant(AsmConstant::UInt(host_d_ino_offset, AsmType::I64)),
            ptr_i8.clone(),
        ));
        alloc_insts.push(load(
            entry_ino_id,
            AsmValue::Register(entry_ino_addr_id),
            AsmType::I64,
        ));

        alloc_insts.push(add(
            out_ino_addr_id,
            out_ptr.clone(),
            AsmValue::Constant(AsmConstant::UInt(LINUX_D_INO_OFFSET, AsmType::I64)),
            ptr_i8.clone(),
        ));
        alloc_insts.push(store(
            next_id,
            AsmValue::Register(entry_ino_id),
            AsmValue::Register(out_ino_addr_id),
        ));
        next_id += 1;

        alloc_insts.push(add(
            out_reclen_addr_id,
            out_ptr.clone(),
            AsmValue::Constant(AsmConstant::UInt(LINUX_D_RECLEN_OFFSET, AsmType::I64)),
            ptr_i8.clone(),
        ));
        alloc_insts.push(store(
            next_id,
            AsmValue::Constant(AsmConstant::UInt(LINUX_DIRENT_SIZE, AsmType::I16)),
            AsmValue::Register(out_reclen_addr_id),
        ));
        next_id += 1;

        alloc_insts.push(add(
            entry_type_addr_id,
            entry_ptr.clone(),
            AsmValue::Constant(AsmConstant::UInt(host_d_type_offset, AsmType::I64)),
            ptr_i8.clone(),
        ));
        alloc_insts.push(load(
            entry_type_id,
            AsmValue::Register(entry_type_addr_id),
            AsmType::I8,
        ));
        alloc_insts.push(add(
            out_type_addr_id,
            out_ptr.clone(),
            AsmValue::Constant(AsmConstant::UInt(LINUX_D_TYPE_OFFSET, AsmType::I64)),
            ptr_i8.clone(),
        ));
        alloc_insts.push(store(
            next_id,
            AsmValue::Register(entry_type_id),
            AsmValue::Register(out_type_addr_id),
        ));
        next_id += 1;

        alloc_insts.push(add(
            out_name_ptr_id,
            out_ptr.clone(),
            AsmValue::Constant(AsmConstant::UInt(LINUX_D_NAME_OFFSET, AsmType::I64)),
            ptr_i8.clone(),
        ));
        alloc_insts.push(add(
            entry_name_ptr_id,
            entry_ptr.clone(),
            AsmValue::Constant(AsmConstant::UInt(host_d_name_offset, AsmType::I64)),
            ptr_i8.clone(),
        ));

        alloc_insts.push(call(
            next_id,
            "strncpy",
            vec![
                AsmValue::Register(out_name_ptr_id),
                AsmValue::Register(entry_name_ptr_id),
                AsmValue::Constant(AsmConstant::UInt(LINUX_D_NAME_MAX, AsmType::I64)),
            ],
            ptr_i8.clone(),
        ));

        let alloc_block = AsmBlock {
            id: 2,
            label: Some(Name::new("alloc")),
            instructions: alloc_insts,
            terminator: AsmTerminator::Return(Some(out_ptr.clone())),
            terminator_encoding: None,
            predecessors: vec![0],
            successors: Vec::new(),
        };

        program.functions.push(AsmFunction {
            name: Name::new("fp_linux_readdir"),
            signature: AsmFunctionSignature {
                params: vec![ptr_i8.clone()],
                return_type: ptr_i8,
                is_variadic: false,
            },
            basic_blocks: vec![entry_block, null_block, alloc_block],
            locals: vec![dir_local],
            stack_slots: Vec::new(),
            frame: None,
            linkage: Linkage::External,
            visibility: Visibility::Default,
            calling_convention: Some(cc),
            section: Some(".text".to_string()),
            is_declaration: false,
        });
        Ok(())
    }
}

pub(super) fn lower_sys_ops_to_windows_imports(program: &mut AsmProgram) -> Result<()> {
    for function in &mut program.functions {
        if function.is_declaration {
            continue;
        }

        let mut next_id = function
            .basic_blocks
            .iter()
            .flat_map(|block| block.instructions.iter().map(|inst| inst.id))
            .max()
            .unwrap_or(0)
            .saturating_add(1);

        for block in &mut function.basic_blocks {
            let snapshot = block.instructions.clone();
            let mut out = Vec::with_capacity(block.instructions.len());

            for inst in &block.instructions {
                let AsmInstructionKind::SysOp(op) = &inst.kind else {
                    out.push(inst.clone());
                    continue;
                };

                match lower_system_api_to_windows_import(
                    op.clone(),
                    inst.id,
                    &snapshot,
                    &mut next_id,
                )? {
                    LoweredWindows::Unchanged => out.push(inst.clone()),
                    LoweredWindows::Single(lowered) => out.push(lowered),
                    LoweredWindows::Sequence(mut seq) => out.append(&mut seq),
                }
            }

            block.instructions = out;
        }
    }
    Ok(())
}

pub(super) fn target_syscall_convention(program: &AsmProgram) -> Option<AsmSyscallConvention> {
    match program.target.object_format {
        AsmObjectFormat::Elf => match program.target.architecture {
            fp_core::asmir::AsmArchitecture::X86_64 => Some(AsmSyscallConvention::LinuxX86_64),
            fp_core::asmir::AsmArchitecture::Aarch64 => Some(AsmSyscallConvention::LinuxAarch64),
            _ => None,
        },
        AsmObjectFormat::MachO => match program.target.architecture {
            fp_core::asmir::AsmArchitecture::X86_64 => Some(AsmSyscallConvention::DarwinX86_64),
            fp_core::asmir::AsmArchitecture::Aarch64 => Some(AsmSyscallConvention::DarwinAarch64),
            _ => None,
        },
        _ => None,
    }
}

pub(super) fn rewrite_syscalls_to_target_unix_convention(program: &mut AsmProgram) -> Result<()> {
    let Some(target_convention) = target_syscall_convention(program) else {
        return Ok(());
    };

    for function in &mut program.functions {
        if function.is_declaration {
            continue;
        }

        for block in &mut function.basic_blocks {
            let snapshot = block.instructions.clone();
            for inst in &mut block.instructions {
                let AsmInstructionKind::Syscall {
                    convention,
                    number,
                    args,
                } = &inst.kind
                else {
                    continue;
                };
                if *convention == target_convention {
                    continue;
                }

                let Some(op) = detect_system_api_from_syscall(convention, number, args, &snapshot)
                else {
                    continue;
                };
                inst.kind = lower_system_api_to_syscall(op, target_convention);
                inst.opcode = AsmOpcode::Generic(AsmGenericOpcode::Syscall);
                inst.ty = AsmType::I64;
            }
        }
    }
    Ok(())
}

pub(super) fn rewrite_posix_calls_to_windows_imports(program: &mut AsmProgram) -> Result<()> {
    for function in &mut program.functions {
        if function.is_declaration {
            continue;
        }

        let mut next_id = function
            .basic_blocks
            .iter()
            .flat_map(|block| block.instructions.iter().map(|inst| inst.id))
            .max()
            .unwrap_or(0)
            .saturating_add(1);

        for block in &mut function.basic_blocks {
            let snapshot = block.instructions.clone();
            let mut out = Vec::with_capacity(block.instructions.len());

            for inst in &block.instructions {
                let Some(op) =
                    detect_system_api_from_posix_call(&inst.kind, PosixDirentStyle::Linux)
                else {
                    out.push(inst.clone());
                    continue;
                };

                match lower_system_api_to_windows_import(op, inst.id, &snapshot, &mut next_id)? {
                    LoweredWindows::Unchanged => out.push(inst.clone()),
                    LoweredWindows::Single(lowered) => out.push(lowered),
                    LoweredWindows::Sequence(mut seq) => out.append(&mut seq),
                }
            }

            block.instructions = out;
        }
    }
    Ok(())
}

pub(super) fn rewrite_syscalls_to_windows_imports(program: &mut AsmProgram) -> Result<()> {
    for function in &mut program.functions {
        if function.is_declaration {
            continue;
        }

        let mut next_id = function
            .basic_blocks
            .iter()
            .flat_map(|block| block.instructions.iter().map(|inst| inst.id))
            .max()
            .unwrap_or(0)
            .saturating_add(1);

        for block in &mut function.basic_blocks {
            let snapshot = block.instructions.clone();
            let mut out = Vec::with_capacity(block.instructions.len());

            for inst in &block.instructions {
                let AsmInstructionKind::Syscall {
                    convention,
                    number,
                    args,
                } = &inst.kind
                else {
                    out.push(inst.clone());
                    continue;
                };

                let Some(op) = detect_system_api_from_syscall(convention, number, args, &snapshot)
                else {
                    out.push(inst.clone());
                    continue;
                };

                match lower_system_api_to_windows_import(op, inst.id, &snapshot, &mut next_id)? {
                    LoweredWindows::Unchanged => out.push(inst.clone()),
                    LoweredWindows::Single(lowered) => {
                        if let fp_core::asmir::AsmTerminator::Return(Some(value)) =
                            &block.terminator
                        {
                            if value == &AsmValue::Register(inst.id) {
                                block.terminator = fp_core::asmir::AsmTerminator::Return(None);
                            }
                        }
                        out.push(lowered);
                    }
                    LoweredWindows::Sequence(mut seq) => out.append(&mut seq),
                }
            }

            block.instructions = out;
        }
    }
    Ok(())
}

pub(super) fn rewrite_windows_imports_to_syscalls(program: &mut AsmProgram) -> Result<()> {
    let convention = match program.target.object_format {
        AsmObjectFormat::Elf => match program.target.architecture {
            fp_core::asmir::AsmArchitecture::X86_64 => Some(AsmSyscallConvention::LinuxX86_64),
            fp_core::asmir::AsmArchitecture::Aarch64 => Some(AsmSyscallConvention::LinuxAarch64),
            _ => None,
        },
        AsmObjectFormat::MachO => match program.target.architecture {
            fp_core::asmir::AsmArchitecture::X86_64 => Some(AsmSyscallConvention::DarwinX86_64),
            fp_core::asmir::AsmArchitecture::Aarch64 => Some(AsmSyscallConvention::DarwinAarch64),
            _ => None,
        },
        _ => None,
    };

    let Some(convention) = convention else {
        return Ok(());
    };

    for function in &mut program.functions {
        if function.is_declaration {
            continue;
        }

        for block in &mut function.basic_blocks {
            let mut out = Vec::with_capacity(block.instructions.len());
            let mut i = 0usize;
            while i < block.instructions.len() {
                if let Some((rewritten, consumed)) =
                    match_writefile_sequence_to_syscall(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) =
                    match_readfile_sequence_to_syscall(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) =
                    match_closehandle_sequence_to_syscall(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) = match_setfilepointerex_sequence_to_syscall(
                    &block.instructions[i..],
                    convention,
                )? {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) =
                    match_virtualalloc_sequence_to_syscall(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) =
                    match_virtualfree_sequence_to_syscall(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) =
                    match_deletefile_sequence_to_syscall(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) =
                    match_createdirectory_sequence_to_syscall(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) =
                    match_removedirectory_sequence_to_syscall(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) =
                    match_movefileex_sequence_to_syscall(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) = match_getfileattributes_sequence_to_syscall(
                    &block.instructions[i..],
                    convention,
                )? {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) =
                    match_freelibrary_sequence_to_unix_call(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                let mut inst = block.instructions[i].clone();
                if let Some(op) = detect_system_api_from_windows_import(&inst.kind, convention) {
                    let (opcode, kind, ty) = lower_system_api_to_unix(op, convention);
                    inst.kind = kind;
                    inst.opcode = opcode;
                    inst.ty = ty;
                }
                out.push(inst);
                i += 1;
            }
            block.instructions = out;
        }
    }
    Ok(())
}
