use super::*;

pub(super) fn match_writefile_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    // Pattern A (stdio):
    //   GetStdHandle; Alloca; WriteFile; Load; [Eq; Select]
    // Pattern B (direct handle):
    //   Alloca; WriteFile; Load; [Eq; Select]
    if instructions.len() < 3 {
        return Ok(None);
    }

    let mut base = 0usize;
    let mut fd_value: Option<AsmValue> = None;
    let handle_value: AsmValue;

    if is_call_named(&instructions[0], "kernel32.dll", "GetStdHandle") {
        if instructions.len() < 4 {
            return Ok(None);
        }
        let getstd = &instructions[0];
        let AsmInstructionKind::Call { args, .. } = &getstd.kind else {
            return Ok(None);
        };
        let Some(handle_code) = args.first().and_then(|value| {
            resolve_i64(value, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
        }) else {
            return Ok(None);
        };
        let fd = match handle_code {
            Some(-11) => 1u64,
            Some(-12) => 2u64,
            _ => return Ok(None),
        };
        fd_value = Some(AsmValue::Constant(AsmConstant::UInt(fd, AsmType::I64)));
        handle_value = AsmValue::Register(getstd.id);
        base = 1;
    } else {
        // Handle comes directly from the WriteFile call's first arg.
        handle_value = AsmValue::Undef(AsmType::I64);
    }

    let alloca = &instructions[base];
    let writefile = instructions
        .get(base + 1)
        .ok_or_else(|| fp_core::error::Error::from("missing WriteFile instruction in sequence"))?;
    let load = instructions
        .get(base + 2)
        .ok_or_else(|| fp_core::error::Error::from("missing Load instruction in sequence"))?;

    if !matches!(alloca.kind, AsmInstructionKind::Alloca { .. }) {
        return Ok(None);
    }
    if !is_call_named(writefile, "kernel32.dll", "WriteFile") {
        return Ok(None);
    }
    let AsmInstructionKind::Load { address, .. } = &load.kind else {
        return Ok(None);
    };
    if address != &AsmValue::Register(alloca.id) {
        return Ok(None);
    }

    let AsmInstructionKind::Call { args, .. } = &writefile.kind else {
        return Ok(None);
    };
    if args.len() < 5 {
        return Ok(None);
    }
    if args[3] != AsmValue::Register(alloca.id) {
        return Ok(None);
    }
    let handle_arg = if base == 1 {
        if args[0] != handle_value {
            return Ok(None);
        }
        handle_value
    } else {
        args[0].clone()
    };

    let fd = fd_value.unwrap_or(handle_arg);
    let op = SystemApiOp::Write {
        fd,
        buffer: args[1].clone(),
        len: args[2].clone(),
    };

    let load_index = base + 2;
    let (dest_id, consumed_tail) = match_result_chain_at(instructions, load_index, load.id);
    let kind = lower_system_api_to_syscall(op, convention);

    Ok(Some((
        AsmInstruction {
            id: dest_id,
            opcode: AsmOpcode::Generic(AsmGenericOpcode::Syscall),
            kind,
            ty: AsmType::I64,
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        },
        consumed_tail,
    )))
}

pub(super) fn match_readfile_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    // Pattern A (stdio):
    //   GetStdHandle; Alloca; ReadFile; Load; [Eq; Select]
    // Pattern B (direct handle):
    //   Alloca; ReadFile; Load; [Eq; Select]
    if instructions.len() < 3 {
        return Ok(None);
    }

    let mut base = 0usize;
    let mut fd_value: Option<AsmValue> = None;
    let handle_value: AsmValue;

    if is_call_named(&instructions[0], "kernel32.dll", "GetStdHandle") {
        if instructions.len() < 4 {
            return Ok(None);
        }
        let getstd = &instructions[0];
        let AsmInstructionKind::Call { args, .. } = &getstd.kind else {
            return Ok(None);
        };
        let Some(handle_code) = args.first().and_then(|value| {
            resolve_i64(value, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
        }) else {
            return Ok(None);
        };
        match handle_code {
            Some(-10) => {}
            _ => return Ok(None),
        }
        fd_value = Some(AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)));
        handle_value = AsmValue::Register(getstd.id);
        base = 1;
    } else {
        handle_value = AsmValue::Undef(AsmType::I64);
    }

    let alloca = &instructions[base];
    let readfile = instructions
        .get(base + 1)
        .ok_or_else(|| fp_core::error::Error::from("missing ReadFile instruction in sequence"))?;
    let load = instructions
        .get(base + 2)
        .ok_or_else(|| fp_core::error::Error::from("missing Load instruction in sequence"))?;

    if !matches!(alloca.kind, AsmInstructionKind::Alloca { .. }) {
        return Ok(None);
    }
    if !is_call_named(readfile, "kernel32.dll", "ReadFile") {
        return Ok(None);
    }
    let AsmInstructionKind::Load { address, .. } = &load.kind else {
        return Ok(None);
    };
    if address != &AsmValue::Register(alloca.id) {
        return Ok(None);
    }

    let AsmInstructionKind::Call { args, .. } = &readfile.kind else {
        return Ok(None);
    };
    if args.len() < 5 {
        return Ok(None);
    }
    if args[3] != AsmValue::Register(alloca.id) {
        return Ok(None);
    }
    let handle_arg = if base == 1 {
        if args[0] != handle_value {
            return Ok(None);
        }
        handle_value
    } else {
        args[0].clone()
    };

    let fd = fd_value.unwrap_or(handle_arg);
    let op = SystemApiOp::Read {
        fd,
        buffer: args[1].clone(),
        len: args[2].clone(),
    };

    let load_index = base + 2;
    let (dest_id, consumed_tail) = match_result_chain_at(instructions, load_index, load.id);
    let kind = lower_system_api_to_syscall(op, convention);

    Ok(Some((
        AsmInstruction {
            id: dest_id,
            opcode: AsmOpcode::Generic(AsmGenericOpcode::Syscall),
            kind,
            ty: AsmType::I64,
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        },
        consumed_tail,
    )))
}

pub(super) fn match_setfilepointerex_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    // Pattern A (stdio):
    //   GetStdHandle; Alloca; SetFilePointerEx; Load; [Eq; Select]
    // Pattern B (direct handle):
    //   Alloca; SetFilePointerEx; Load; [Eq; Select]
    if instructions.len() < 3 {
        return Ok(None);
    }

    let mut base = 0usize;
    let mut fd_value: Option<AsmValue> = None;
    let handle_value: AsmValue;

    if is_call_named(&instructions[0], "kernel32.dll", "GetStdHandle") {
        if instructions.len() < 4 {
            return Ok(None);
        }
        let getstd = &instructions[0];
        let AsmInstructionKind::Call { args, .. } = &getstd.kind else {
            return Ok(None);
        };
        let Some(handle_code) = args.first().and_then(|value| {
            resolve_i64(value, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
        }) else {
            return Ok(None);
        };
        let fd = match handle_code {
            Some(-10) => 0u64,
            Some(-11) => 1u64,
            Some(-12) => 2u64,
            _ => return Ok(None),
        };
        fd_value = Some(AsmValue::Constant(AsmConstant::UInt(fd, AsmType::I64)));
        handle_value = AsmValue::Register(getstd.id);
        base = 1;
    } else {
        handle_value = AsmValue::Undef(AsmType::I64);
    }

    let alloca = &instructions[base];
    let setfp = instructions.get(base + 1).ok_or_else(|| {
        fp_core::error::Error::from("missing SetFilePointerEx instruction in sequence")
    })?;
    let load = instructions
        .get(base + 2)
        .ok_or_else(|| fp_core::error::Error::from("missing Load instruction in sequence"))?;

    if !matches!(alloca.kind, AsmInstructionKind::Alloca { .. }) {
        return Ok(None);
    }
    if !is_call_named(setfp, "kernel32.dll", "SetFilePointerEx") {
        return Ok(None);
    }
    let AsmInstructionKind::Load { address, .. } = &load.kind else {
        return Ok(None);
    };
    if address != &AsmValue::Register(alloca.id) {
        return Ok(None);
    }

    let AsmInstructionKind::Call { args, .. } = &setfp.kind else {
        return Ok(None);
    };
    if args.len() != 4 {
        return Ok(None);
    }
    if args[2] != AsmValue::Register(alloca.id) {
        return Ok(None);
    }
    let handle_arg = if base == 1 {
        if args[0] != handle_value {
            return Ok(None);
        }
        handle_value
    } else {
        args[0].clone()
    };

    let fd = fd_value.unwrap_or(handle_arg);
    let op = SystemApiOp::Seek {
        fd,
        offset: args[1].clone(),
        whence: args[3].clone(),
    };

    let load_index = base + 2;
    let (dest_id, consumed_tail) = match_result_chain_at(instructions, load_index, load.id);
    let kind = lower_system_api_to_syscall(op, convention);

    Ok(Some((
        AsmInstruction {
            id: dest_id,
            opcode: AsmOpcode::Generic(AsmGenericOpcode::Syscall),
            kind,
            ty: AsmType::I64,
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        },
        consumed_tail,
    )))
}

pub(super) fn match_virtualalloc_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    // Pattern:
    //   VirtualAlloc; Eq; Select
    if instructions.len() < 3 {
        return Ok(None);
    }
    let call = &instructions[0];
    let eq = &instructions[1];
    let select = &instructions[2];

    if !is_call_named(call, "kernel32.dll", "VirtualAlloc") {
        return Ok(None);
    }
    if !matches!(eq.kind, AsmInstructionKind::Eq(_, _)) {
        return Ok(None);
    }
    let AsmInstructionKind::Select {
        if_true, if_false, ..
    } = &select.kind
    else {
        return Ok(None);
    };
    if if_false != &AsmValue::Register(call.id) {
        return Ok(None);
    }
    if if_true != &AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)) {
        return Ok(None);
    }

    let AsmInstructionKind::Call { args, .. } = &call.kind else {
        return Ok(None);
    };
    if args.len() != 4 {
        return Ok(None);
    }

    let style = match convention {
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
            PosixFlagStyle::Darwin
        }
        _ => PosixFlagStyle::Linux,
    };

    let page_prot = resolve_i64(&args[3], instructions)
        .map_err(|e| {
            eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
            e
        })
        .ok()
        .flatten()
        .unwrap_or(0x04);
    let prot = match page_prot {
        0x40 | 0x20 => 0x1 | 0x4,
        0x04 => 0x1 | 0x2,
        0x02 => 0x1,
        _ => 0x1 | 0x2,
    };

    let op = SystemApiOp::Mmap {
        addr: args[0].clone(),
        len: args[1].clone(),
        prot: AsmValue::Constant(AsmConstant::Int(prot, AsmType::I64)),
        flags: AsmValue::Constant(AsmConstant::Int(
            posix_mmap_flags_anonymous_private(style),
            AsmType::I64,
        )),
        fd: AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)),
        offset: AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
    };

    let kind = lower_system_api_to_syscall(op, convention);
    Ok(Some((
        AsmInstruction {
            id: select.id,
            opcode: AsmOpcode::Generic(AsmGenericOpcode::Syscall),
            kind,
            ty: AsmType::I64,
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        },
        3,
    )))
}

pub(super) fn match_virtualfree_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    // Pattern:
    //   VirtualFree; Eq; Select
    if instructions.len() < 3 {
        return Ok(None);
    }
    let call = &instructions[0];
    let eq = &instructions[1];
    let select = &instructions[2];

    if !is_call_named(call, "kernel32.dll", "VirtualFree") {
        return Ok(None);
    }
    if !matches!(eq.kind, AsmInstructionKind::Eq(_, _)) {
        return Ok(None);
    }
    let AsmInstructionKind::Select {
        if_true, if_false, ..
    } = &select.kind
    else {
        return Ok(None);
    };
    if if_true != &AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)) {
        return Ok(None);
    }
    if if_false != &AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)) {
        return Ok(None);
    }

    let AsmInstructionKind::Call { args, .. } = &call.kind else {
        return Ok(None);
    };
    if args.len() != 3 {
        return Ok(None);
    }

    let op = SystemApiOp::Munmap {
        addr: args[0].clone(),
        len: args[1].clone(),
    };

    let kind = lower_system_api_to_syscall(op, convention);
    Ok(Some((
        AsmInstruction {
            id: select.id,
            opcode: AsmOpcode::Generic(AsmGenericOpcode::Syscall),
            kind,
            ty: AsmType::I64,
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        },
        3,
    )))
}

pub(super) fn match_kernel32_bool_call_sequence_to_syscall(
    instructions: &[AsmInstruction],
    proc_name: &str,
    op: SystemApiOp,
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    // Pattern:
    //   <proc>; Eq; Select
    if instructions.len() < 3 {
        return Ok(None);
    }
    let call = &instructions[0];
    let eq = &instructions[1];
    let select = &instructions[2];

    if !is_call_named(call, "kernel32.dll", proc_name) {
        return Ok(None);
    }
    if !matches!(eq.kind, AsmInstructionKind::Eq(_, _)) {
        return Ok(None);
    }
    let AsmInstructionKind::Select {
        if_true, if_false, ..
    } = &select.kind
    else {
        return Ok(None);
    };
    if if_true != &AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)) {
        return Ok(None);
    }
    if if_false != &AsmValue::Constant(AsmConstant::Int(0, AsmType::I64))
        && if_false != &AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64))
    {
        return Ok(None);
    }

    let kind = lower_system_api_to_syscall(op, convention);
    Ok(Some((
        AsmInstruction {
            id: select.id,
            opcode: AsmOpcode::Generic(AsmGenericOpcode::Syscall),
            kind,
            ty: AsmType::I64,
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        },
        3,
    )))
}

pub(super) fn match_deletefile_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    let call = instructions.first();
    let Some(call) = call else {
        return Ok(None);
    };
    let AsmInstructionKind::Call { args, .. } = &call.kind else {
        return Ok(None);
    };
    if args.len() != 1 {
        return Ok(None);
    }
    match_kernel32_bool_call_sequence_to_syscall(
        instructions,
        "DeleteFileA",
        SystemApiOp::Unlink {
            path: args[0].clone(),
        },
        convention,
    )
}

pub(super) fn match_createdirectory_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    let call = instructions.first();
    let Some(call) = call else {
        return Ok(None);
    };
    let AsmInstructionKind::Call { args, .. } = &call.kind else {
        return Ok(None);
    };
    if args.len() != 2 {
        return Ok(None);
    }
    match_kernel32_bool_call_sequence_to_syscall(
        instructions,
        "CreateDirectoryA",
        SystemApiOp::Mkdir {
            path: args[0].clone(),
            mode: AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)),
        },
        convention,
    )
}

pub(super) fn match_removedirectory_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    let call = instructions.first();
    let Some(call) = call else {
        return Ok(None);
    };
    let AsmInstructionKind::Call { args, .. } = &call.kind else {
        return Ok(None);
    };
    if args.len() != 1 {
        return Ok(None);
    }
    match_kernel32_bool_call_sequence_to_syscall(
        instructions,
        "RemoveDirectoryA",
        SystemApiOp::Rmdir {
            path: args[0].clone(),
        },
        convention,
    )
}

pub(super) fn match_movefileex_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    let call = instructions.first();
    let Some(call) = call else {
        return Ok(None);
    };
    let AsmInstructionKind::Call { args, .. } = &call.kind else {
        return Ok(None);
    };
    if args.len() != 3 {
        return Ok(None);
    }
    match_kernel32_bool_call_sequence_to_syscall(
        instructions,
        "MoveFileExA",
        SystemApiOp::Rename {
            from: args[0].clone(),
            to: args[1].clone(),
        },
        convention,
    )
}

pub(super) fn match_result_chain_at(
    instructions: &[AsmInstruction],
    load_index: usize,
    load_id: u32,
) -> (u32, usize) {
    // Accept both:
    //   ...; Load
    //   ...; Load; Eq; Select  (Select.if_false == Load)
    if instructions.len() >= load_index + 3 {
        let eq = &instructions[load_index + 1];
        let select = &instructions[load_index + 2];
        if matches!(eq.kind, AsmInstructionKind::Eq(_, _)) {
            if let AsmInstructionKind::Select { if_false, .. } = &select.kind {
                if if_false == &AsmValue::Register(load_id) {
                    return (select.id, load_index + 3);
                }
            }
        }
    }
    (load_id, load_index + 1)
}

pub(super) fn is_call_named(inst: &AsmInstruction, dll: &str, name: &str) -> bool {
    let AsmInstructionKind::Call { function, .. } = &inst.kind else {
        return false;
    };
    let AsmValue::Function(symbol) = function else {
        return false;
    };
    let (sym_dll, sym_name) = split_import_symbol(symbol);
    import_dll_matches(&sym_dll, dll) && sym_name == name
}

pub(super) fn import_dll_matches(actual: &str, expected: &str) -> bool {
    if actual.eq_ignore_ascii_case(expected) {
        return true;
    }

    matches!(
        (
            actual.to_ascii_lowercase().as_str(),
            expected.to_ascii_lowercase().as_str(),
        ),
        ("kernelbase.dll", "kernel32.dll") | ("kernel32.dll", "kernelbase.dll")
    )
}
