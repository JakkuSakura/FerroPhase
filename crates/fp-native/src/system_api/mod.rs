use fp_core::asmir::{
    AsmConstant, AsmGenericOpcode, AsmInstruction, AsmInstructionKind, AsmObjectFormat, AsmOpcode,
    AsmProgram, AsmSyscallConvention, AsmType, AsmValue,
};
use fp_core::error::Result;
use fp_core::lir::CallingConvention;

#[derive(Debug, Clone, PartialEq)]
pub enum SystemApiOp {
    Exit {
        code: AsmValue,
    },
    GetPid,
    GetTid,
    Dlopen {
        path: AsmValue,
        flags: AsmValue,
    },
    Dlsym {
        handle: AsmValue,
        symbol: AsmValue,
    },
    Dlclose {
        handle: AsmValue,
    },
    Write {
        fd: AsmValue,
        buffer: AsmValue,
        len: AsmValue,
    },
    Read {
        fd: AsmValue,
        buffer: AsmValue,
        len: AsmValue,
    },
    Close {
        fd: AsmValue,
    },
    Open {
        path: AsmValue,
        flags: AsmValue,
        mode: AsmValue,
        flag_style: PosixFlagStyle,
    },
    Seek {
        fd: AsmValue,
        offset: AsmValue,
        whence: AsmValue,
    },
    Mmap {
        addr: AsmValue,
        len: AsmValue,
        prot: AsmValue,
        flags: AsmValue,
        fd: AsmValue,
        offset: AsmValue,
    },
    Munmap {
        addr: AsmValue,
        len: AsmValue,
    },
}

fn match_freelibrary_sequence_to_unix_call(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    // Pattern:
    //   FreeLibrary; Eq; Select
    if instructions.len() < 3 {
        return Ok(None);
    }
    let call = &instructions[0];
    let eq = &instructions[1];
    let select = &instructions[2];

    if !is_call_named(call, "kernel32.dll", "FreeLibrary") {
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
    if if_false != &AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)) {
        return Ok(None);
    }

    let AsmInstructionKind::Call { args, .. } = &call.kind else {
        return Ok(None);
    };
    if args.len() != 1 {
        return Ok(None);
    }

    let op = SystemApiOp::Dlclose {
        handle: args[0].clone(),
    };
    let (opcode, kind, type_hint) = lower_system_api_to_unix(op, convention);
    Ok(Some((
        AsmInstruction {
            id: select.id,
            opcode,
            kind,
            type_hint: Some(type_hint),
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

fn normalize_proc_name(symbol: &str) -> String {
    let base = symbol.split('!').last().unwrap_or(symbol).trim();
    base.trim_start_matches('_').to_ascii_lowercase()
}

fn detect_system_api_from_posix_call(kind: &AsmInstructionKind) -> Option<SystemApiOp> {
    let AsmInstructionKind::Call { function, args, .. } = kind else {
        return None;
    };
    let AsmValue::Function(symbol) = function else {
        return None;
    };
    let name = normalize_proc_name(symbol);
    match name.as_str() {
        "dlopen" => Some(SystemApiOp::Dlopen {
            path: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
            flags: args
                .get(1)
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32))),
        }),
        "dlsym" => Some(SystemApiOp::Dlsym {
            handle: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64))),
            symbol: args
                .get(1)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
        }),
        "dlclose" => Some(SystemApiOp::Dlclose {
            handle: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64))),
        }),
        _ => None,
    }
}

fn windows_createfile_disposition_from_flags(style: PosixFlagStyle, flags: i64) -> i64 {
    match style {
        PosixFlagStyle::Linux => windows_createfile_disposition_linux(flags),
        PosixFlagStyle::Darwin => windows_createfile_disposition_darwin(flags),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PosixFlagStyle {
    Linux,
    Darwin,
}

fn posix_mmap_flags_anonymous_private(style: PosixFlagStyle) -> i64 {
    match style {
        // MAP_PRIVATE=0x02, MAP_ANONYMOUS=0x20
        PosixFlagStyle::Linux => 0x02 | 0x20,
        // MAP_PRIVATE=0x02, MAP_ANON=0x1000
        PosixFlagStyle::Darwin => 0x02 | 0x1000,
    }
}

fn windows_page_protection_from_posix(prot: i64) -> i64 {
    // PROT_READ=1, PROT_WRITE=2, PROT_EXEC=4
    // PAGE_NOACCESS=0x01
    // PAGE_READONLY=0x02
    // PAGE_READWRITE=0x04
    // PAGE_EXECUTE_READ=0x20
    // PAGE_EXECUTE_READWRITE=0x40
    let read = (prot & 1) != 0;
    let write = (prot & 2) != 0;
    let exec = (prot & 4) != 0;
    match (exec, write, read) {
        (true, true, _) => 0x40,
        (true, false, true) => 0x20,
        (false, true, _) => 0x04,
        (false, false, true) => 0x02,
        _ => 0x01,
    }
}

fn windows_createfile_desired_access(flags: i64) -> i64 {
    // POSIX: O_RDONLY=0, O_WRONLY=1, O_RDWR=2
    // Win32: GENERIC_READ=0x80000000, GENERIC_WRITE=0x40000000
    const GENERIC_READ: i64 = 0x8000_0000u32 as i64;
    const GENERIC_WRITE: i64 = 0x4000_0000u32 as i64;
    match flags & 0b11 {
        0 => GENERIC_READ,
        1 => GENERIC_WRITE,
        2 => GENERIC_READ | GENERIC_WRITE,
        _ => GENERIC_READ,
    }
}

fn windows_createfile_disposition_linux(flags: i64) -> i64 {
    // Win32 creation disposition values:
    // 1 CREATE_NEW, 2 CREATE_ALWAYS, 3 OPEN_EXISTING, 4 OPEN_ALWAYS, 5 TRUNCATE_EXISTING
    const O_CREAT: i64 = 64;
    const O_EXCL: i64 = 128;
    const O_TRUNC: i64 = 512;
    let has_creat = (flags & O_CREAT) != 0;
    let has_excl = (flags & O_EXCL) != 0;
    let has_trunc = (flags & O_TRUNC) != 0;
    match (has_creat, has_excl, has_trunc) {
        (true, true, _) => 1,
        (true, false, true) => 2,
        (true, false, false) => 4,
        (false, _, true) => 5,
        _ => 3,
    }
}

fn windows_createfile_disposition_darwin(flags: i64) -> i64 {
    // Darwin flag constants differ.
    const O_CREAT: i64 = 0x200;
    const O_EXCL: i64 = 0x800;
    const O_TRUNC: i64 = 0x400;
    let has_creat = (flags & O_CREAT) != 0;
    let has_excl = (flags & O_EXCL) != 0;
    let has_trunc = (flags & O_TRUNC) != 0;
    match (has_creat, has_excl, has_trunc) {
        (true, true, _) => 1,
        (true, false, true) => 2,
        (true, false, false) => 4,
        (false, _, true) => 5,
        _ => 3,
    }
}

fn match_closehandle_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    // Pattern A (stdio):
    //   GetStdHandle; CloseHandle; Eq; Select
    // Pattern B (direct handle):
    //   CloseHandle; Eq; Select
    if instructions.len() < 3 {
        return Ok(None);
    }

    let mut base = 0usize;
    let mut fd_value: Option<AsmValue> = None;

    if is_call_named(&instructions[0], "kernel32.dll", "GetStdHandle") {
        if instructions.len() < 4 {
            return Ok(None);
        }
        let getstd = &instructions[0];
        let AsmInstructionKind::Call {
            args: getstd_args, ..
        } = &getstd.kind
        else {
            return Ok(None);
        };
        let Some(handle_code) = getstd_args
            .first()
            .and_then(|value| resolve_i64(value, instructions).ok())
        else {
            return Ok(None);
        };
        let fd = match handle_code {
            Some(-10) => 0u64,
            Some(-11) => 1u64,
            Some(-12) => 2u64,
            _ => return Ok(None),
        };
        fd_value = Some(AsmValue::Constant(AsmConstant::UInt(fd, AsmType::I64)));
        base = 1;
    }

    let close = &instructions[base];
    let cmp = instructions.get(base + 1).ok_or_else(|| {
        fp_core::error::Error::from("missing Eq instruction in CloseHandle sequence")
    })?;
    let select = instructions.get(base + 2).ok_or_else(|| {
        fp_core::error::Error::from("missing Select instruction in CloseHandle sequence")
    })?;

    if !is_call_named(close, "kernel32.dll", "CloseHandle") {
        return Ok(None);
    }
    if !matches!(cmp.kind, AsmInstructionKind::Eq(_, _)) {
        return Ok(None);
    }
    if !matches!(select.kind, AsmInstructionKind::Select { .. }) {
        return Ok(None);
    }

    let AsmInstructionKind::Call {
        args: close_args, ..
    } = &close.kind
    else {
        return Ok(None);
    };
    if close_args.len() != 1 {
        return Ok(None);
    }
    if base == 1 && close_args[0] != AsmValue::Register(instructions[0].id) {
        return Ok(None);
    }

    let fd = fd_value.unwrap_or_else(|| close_args[0].clone());
    let op = SystemApiOp::Close { fd };
    let kind = lower_system_api_to_syscall(op, convention);

    Ok(Some((
        AsmInstruction {
            id: select.id,
            opcode: AsmOpcode::Generic(AsmGenericOpcode::Syscall),
            kind,
            type_hint: Some(AsmType::I64),
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        },
        base + 3,
    )))
}

fn fd_to_std_handle_code(fd: i64) -> Option<i64> {
    // STD_INPUT_HANDLE=-10, STD_OUTPUT_HANDLE=-11, STD_ERROR_HANDLE=-12
    Some(match fd {
        0 => -10,
        1 => -11,
        2 => -12,
        _ => return None,
    })
}

pub fn rewrite_program_for_target(program: &mut AsmProgram) -> Result<()> {
    if program.target.object_format == AsmObjectFormat::Coff
        || program.target.object_format == AsmObjectFormat::Pe
    {
        // Windows: avoid raw syscalls; prefer stable user-mode APIs.
        rewrite_posix_calls_to_windows_imports(program)?;
        rewrite_syscalls_to_windows_imports(program)?;
    } else {
        // Unix targets: if we see known Windows API patterns, rewrite them back to syscalls.
        rewrite_windows_imports_to_syscalls(program)?;
    }
    Ok(())
}

fn rewrite_posix_calls_to_windows_imports(program: &mut AsmProgram) -> Result<()> {
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
                let Some(op) = detect_system_api_from_posix_call(&inst.kind) else {
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

fn rewrite_syscalls_to_windows_imports(program: &mut AsmProgram) -> Result<()> {
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

fn rewrite_windows_imports_to_syscalls(program: &mut AsmProgram) -> Result<()> {
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
                    match_freelibrary_sequence_to_unix_call(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                let mut inst = block.instructions[i].clone();
                if let Some(op) = detect_system_api_from_windows_import(&inst.kind, convention) {
                    let (opcode, kind, type_hint) = lower_system_api_to_unix(op, convention);
                    inst.kind = kind;
                    inst.opcode = opcode;
                    inst.type_hint = Some(type_hint);
                }
                out.push(inst);
                i += 1;
            }
            block.instructions = out;
        }
    }
    Ok(())
}

fn unix_calling_convention(convention: AsmSyscallConvention) -> CallingConvention {
    match convention {
        AsmSyscallConvention::LinuxX86_64 | AsmSyscallConvention::DarwinX86_64 => {
            CallingConvention::X86_64SysV
        }
        AsmSyscallConvention::LinuxAarch64 | AsmSyscallConvention::DarwinAarch64 => {
            CallingConvention::AAPCS
        }
    }
}

fn lower_system_api_to_unix(
    op: SystemApiOp,
    convention: AsmSyscallConvention,
) -> (AsmOpcode, AsmInstructionKind, AsmType) {
    match op {
        SystemApiOp::Dlopen { path, flags } => (
            AsmOpcode::Generic(AsmGenericOpcode::Call),
            AsmInstructionKind::Call {
                function: AsmValue::Function("dlopen".to_string()),
                args: vec![path, flags],
                calling_convention: unix_calling_convention(convention),
                tail_call: false,
            },
            AsmType::I64,
        ),
        SystemApiOp::Dlsym { handle, symbol } => (
            AsmOpcode::Generic(AsmGenericOpcode::Call),
            AsmInstructionKind::Call {
                function: AsmValue::Function("dlsym".to_string()),
                args: vec![handle, symbol],
                calling_convention: unix_calling_convention(convention),
                tail_call: false,
            },
            AsmType::I64,
        ),
        SystemApiOp::Dlclose { handle } => (
            AsmOpcode::Generic(AsmGenericOpcode::Call),
            AsmInstructionKind::Call {
                function: AsmValue::Function("dlclose".to_string()),
                args: vec![handle],
                calling_convention: unix_calling_convention(convention),
                tail_call: false,
            },
            AsmType::I64,
        ),
        other => (
            AsmOpcode::Generic(AsmGenericOpcode::Syscall),
            lower_system_api_to_syscall(other, convention),
            AsmType::I64,
        ),
    }
}

fn detect_system_api_from_windows_import(
    kind: &AsmInstructionKind,
    convention: AsmSyscallConvention,
) -> Option<SystemApiOp> {
    let AsmInstructionKind::Call { function, args, .. } = kind else {
        return None;
    };
    let AsmValue::Function(name) = function else {
        return None;
    };
    let (dll, proc_name) = split_import_symbol(name);
    if !dll.eq_ignore_ascii_case("kernel32.dll") {
        return None;
    }

    match proc_name.as_str() {
        "ExitProcess" => {
            let code = args
                .first()
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)));
            Some(SystemApiOp::Exit { code })
        }
        "GetCurrentProcessId" => Some(SystemApiOp::GetPid),
        "GetCurrentThreadId"
            if matches!(
                convention,
                AsmSyscallConvention::LinuxX86_64 | AsmSyscallConvention::LinuxAarch64
            ) =>
        {
            Some(SystemApiOp::GetTid)
        }
        "LoadLibraryA" => {
            let path = args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))));
            Some(SystemApiOp::Dlopen {
                path,
                flags: AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)),
            })
        }
        "GetProcAddress" => {
            if args.len() < 2 {
                return None;
            }
            Some(SystemApiOp::Dlsym {
                handle: args[0].clone(),
                symbol: args[1].clone(),
            })
        }
        "FreeLibrary" => {
            if args.len() != 1 {
                return None;
            }
            Some(SystemApiOp::Dlclose {
                handle: args[0].clone(),
            })
        }
        "CreateFileA" => {
            if args.len() != 7 {
                return None;
            }
            let path = args[0].clone();
            let desired_access = resolve_i64(&args[1], &[]).ok().flatten()?;
            let disposition = resolve_i64(&args[4], &[]).ok().flatten()?;
            let flags = posix_flags_from_createfile(convention, desired_access, disposition);
            Some(SystemApiOp::Open {
                path,
                flags: AsmValue::Constant(AsmConstant::Int(flags, AsmType::I64)),
                mode: AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                flag_style: match convention {
                    AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                        PosixFlagStyle::Darwin
                    }
                    _ => PosixFlagStyle::Linux,
                },
            })
        }
        "WriteFile" => {
            if args.len() < 3 {
                return None;
            }
            Some(SystemApiOp::Write {
                fd: args[0].clone(),
                buffer: args[1].clone(),
                len: args[2].clone(),
            })
        }
        "ReadFile" => {
            if args.len() < 3 {
                return None;
            }
            Some(SystemApiOp::Read {
                fd: args[0].clone(),
                buffer: args[1].clone(),
                len: args[2].clone(),
            })
        }
        "CloseHandle" => {
            if args.len() != 1 {
                return None;
            }
            Some(SystemApiOp::Close {
                fd: args[0].clone(),
            })
        }
        "SetFilePointerEx" => {
            if args.len() != 4 {
                return None;
            }
            Some(SystemApiOp::Seek {
                fd: args[0].clone(),
                offset: args[1].clone(),
                // dwMoveMethod
                whence: args[3].clone(),
            })
        }
        "VirtualAlloc" => {
            if args.len() != 4 {
                return None;
            }
            // Treat VirtualAlloc as anonymous mmap.
            let style = match convention {
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    PosixFlagStyle::Darwin
                }
                _ => PosixFlagStyle::Linux,
            };
            let page_prot = resolve_i64(&args[3], &[]).ok().flatten().unwrap_or(0x04);
            let prot = match page_prot {
                0x40 | 0x20 => 0x1 | 0x4,
                0x04 => 0x1 | 0x2,
                0x02 => 0x1,
                _ => 0x1 | 0x2,
            };
            Some(SystemApiOp::Mmap {
                addr: args[0].clone(),
                len: args[1].clone(),
                prot: AsmValue::Constant(AsmConstant::Int(prot, AsmType::I64)),
                flags: AsmValue::Constant(AsmConstant::Int(
                    posix_mmap_flags_anonymous_private(style),
                    AsmType::I64,
                )),
                fd: AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)),
                offset: AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
            })
        }
        "VirtualFree" => {
            if args.len() != 3 {
                return None;
            }
            Some(SystemApiOp::Munmap {
                addr: args[0].clone(),
                len: args[1].clone(),
            })
        }
        _ => None,
    }
}

fn posix_flags_from_createfile(
    convention: AsmSyscallConvention,
    desired_access: i64,
    disposition: i64,
) -> i64 {
    // Win32:
    //   GENERIC_READ=0x80000000, GENERIC_WRITE=0x40000000
    // POSIX:
    //   O_RDONLY=0, O_WRONLY=1, O_RDWR=2
    //   O_CREAT,O_TRUNC,O_EXCL are platform-specific.
    const GENERIC_READ: i64 = 0x8000_0000u32 as i64;
    const GENERIC_WRITE: i64 = 0x4000_0000u32 as i64;

    let mut flags = match (
        (desired_access & GENERIC_READ) != 0,
        (desired_access & GENERIC_WRITE) != 0,
    ) {
        (true, true) => 2,
        (false, true) => 1,
        _ => 0,
    };

    let (o_creat, o_trunc, o_excl) = match convention {
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 =>
        // macOS
        {
            (0x200i64, 0x400i64, 0x800i64)
        }
        _ => (64i64, 512i64, 128i64),
    };

    match disposition {
        1 => flags |= o_creat | o_excl,
        2 => flags |= o_creat | o_trunc,
        4 => flags |= o_creat,
        5 => flags |= o_trunc,
        _ => {}
    }

    flags
}

fn detect_system_api_from_syscall(
    convention: &AsmSyscallConvention,
    number: &AsmValue,
    args: &[AsmValue],
    instructions: &[AsmInstruction],
) -> Option<SystemApiOp> {
    let num = resolve_u64(number, instructions)?;

    match convention {
        AsmSyscallConvention::LinuxX86_64 if num == 60 => Some(SystemApiOp::Exit {
            code: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32))),
        }),
        AsmSyscallConvention::LinuxX86_64 if num == 39 => Some(SystemApiOp::GetPid),
        AsmSyscallConvention::LinuxX86_64 if num == 186 => Some(SystemApiOp::GetTid),
        AsmSyscallConvention::LinuxAarch64 if num == 93 => Some(SystemApiOp::Exit {
            code: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32))),
        }),
        AsmSyscallConvention::LinuxAarch64 if num == 172 => Some(SystemApiOp::GetPid),
        AsmSyscallConvention::LinuxAarch64 if num == 178 => Some(SystemApiOp::GetTid),
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0001 =>
        {
            Some(SystemApiOp::Exit {
                code: args
                    .get(0)
                    .cloned()
                    .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32))),
            })
        }
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0014 =>
        {
            Some(SystemApiOp::GetPid)
        }
        AsmSyscallConvention::LinuxX86_64 if num == 1 => Some(SystemApiOp::Write {
            fd: args.get(0)?.clone(),
            buffer: args.get(1)?.clone(),
            len: args.get(2)?.clone(),
        }),
        AsmSyscallConvention::LinuxAarch64 if num == 64 => Some(SystemApiOp::Write {
            fd: args.get(0)?.clone(),
            buffer: args.get(1)?.clone(),
            len: args.get(2)?.clone(),
        }),
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0004 =>
        {
            Some(SystemApiOp::Write {
                fd: args.get(0)?.clone(),
                buffer: args.get(1)?.clone(),
                len: args.get(2)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 0 => Some(SystemApiOp::Read {
            fd: args.get(0)?.clone(),
            buffer: args.get(1)?.clone(),
            len: args.get(2)?.clone(),
        }),
        AsmSyscallConvention::LinuxAarch64 if num == 63 => Some(SystemApiOp::Read {
            fd: args.get(0)?.clone(),
            buffer: args.get(1)?.clone(),
            len: args.get(2)?.clone(),
        }),
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0003 =>
        {
            Some(SystemApiOp::Read {
                fd: args.get(0)?.clone(),
                buffer: args.get(1)?.clone(),
                len: args.get(2)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 3 => Some(SystemApiOp::Close {
            fd: args.get(0)?.clone(),
        }),
        AsmSyscallConvention::LinuxAarch64 if num == 57 => Some(SystemApiOp::Close {
            fd: args.get(0)?.clone(),
        }),
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0006 =>
        {
            Some(SystemApiOp::Close {
                fd: args.get(0)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 2 => Some(SystemApiOp::Open {
            path: args.get(0)?.clone(),
            flags: args.get(1)?.clone(),
            mode: args.get(2)?.clone(),
            flag_style: PosixFlagStyle::Linux,
        }),
        AsmSyscallConvention::LinuxX86_64 if num == 257 => {
            // openat(dirfd, path, flags, mode)
            let dirfd = args.get(0)?.clone();
            let dirfd = resolve_i64(&dirfd, instructions).ok().flatten()?;
            // AT_FDCWD=-100
            if dirfd != -100 {
                return None;
            }
            Some(SystemApiOp::Open {
                path: args.get(1)?.clone(),
                flags: args.get(2)?.clone(),
                mode: args.get(3)?.clone(),
                flag_style: PosixFlagStyle::Linux,
            })
        }
        AsmSyscallConvention::LinuxAarch64 if num == 56 => {
            // openat(dirfd, path, flags, mode)
            let dirfd = args.get(0)?.clone();
            let dirfd = resolve_i64(&dirfd, instructions).ok().flatten()?;
            if dirfd != -100 {
                return None;
            }
            Some(SystemApiOp::Open {
                path: args.get(1)?.clone(),
                flags: args.get(2)?.clone(),
                mode: args.get(3)?.clone(),
                flag_style: PosixFlagStyle::Linux,
            })
        }
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0005 =>
        {
            Some(SystemApiOp::Open {
                path: args.get(0)?.clone(),
                flags: args.get(1)?.clone(),
                mode: args.get(2)?.clone(),
                flag_style: PosixFlagStyle::Darwin,
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 8 => Some(SystemApiOp::Seek {
            fd: args.get(0)?.clone(),
            offset: args.get(1)?.clone(),
            whence: args.get(2)?.clone(),
        }),
        AsmSyscallConvention::LinuxAarch64 if num == 62 => Some(SystemApiOp::Seek {
            fd: args.get(0)?.clone(),
            offset: args.get(1)?.clone(),
            whence: args.get(2)?.clone(),
        }),
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_00c7 =>
        {
            Some(SystemApiOp::Seek {
                fd: args.get(0)?.clone(),
                offset: args.get(1)?.clone(),
                whence: args.get(2)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 9 => Some(SystemApiOp::Mmap {
            addr: args.get(0)?.clone(),
            len: args.get(1)?.clone(),
            prot: args.get(2)?.clone(),
            flags: args.get(3)?.clone(),
            fd: args.get(4)?.clone(),
            offset: args.get(5)?.clone(),
        }),
        AsmSyscallConvention::LinuxAarch64 if num == 222 => Some(SystemApiOp::Mmap {
            addr: args.get(0)?.clone(),
            len: args.get(1)?.clone(),
            prot: args.get(2)?.clone(),
            flags: args.get(3)?.clone(),
            fd: args.get(4)?.clone(),
            offset: args.get(5)?.clone(),
        }),
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_00c5 =>
        {
            Some(SystemApiOp::Mmap {
                addr: args.get(0)?.clone(),
                len: args.get(1)?.clone(),
                prot: args.get(2)?.clone(),
                flags: args.get(3)?.clone(),
                fd: args.get(4)?.clone(),
                offset: args.get(5)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 11 => Some(SystemApiOp::Munmap {
            addr: args.get(0)?.clone(),
            len: args.get(1)?.clone(),
        }),
        AsmSyscallConvention::LinuxAarch64 if num == 215 => Some(SystemApiOp::Munmap {
            addr: args.get(0)?.clone(),
            len: args.get(1)?.clone(),
        }),
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0049 =>
        {
            Some(SystemApiOp::Munmap {
                addr: args.get(0)?.clone(),
                len: args.get(1)?.clone(),
            })
        }
        _ => None,
    }
}

enum LoweredWindows {
    Unchanged,
    Single(AsmInstruction),
    Sequence(Vec<AsmInstruction>),
}

fn lower_system_api_to_windows_import(
    op: SystemApiOp,
    replaces_id: u32,
    instructions: &[AsmInstruction],
    next_id: &mut u32,
) -> Result<LoweredWindows> {
    match op {
        SystemApiOp::Exit { code } => Ok(LoweredWindows::Single(AsmInstruction {
            id: replaces_id,
            opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
            kind: AsmInstructionKind::Call {
                function: AsmValue::Function("kernel32!ExitProcess".to_string()),
                args: vec![code],
                calling_convention: CallingConvention::Win64,
                tail_call: false,
            },
            type_hint: Some(AsmType::Void),
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        })),
        SystemApiOp::GetPid => Ok(LoweredWindows::Single(AsmInstruction {
            id: replaces_id,
            opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
            kind: AsmInstructionKind::Call {
                function: AsmValue::Function("kernel32!GetCurrentProcessId".to_string()),
                args: Vec::new(),
                calling_convention: CallingConvention::Win64,
                tail_call: false,
            },
            type_hint: Some(AsmType::I64),
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        })),
        SystemApiOp::GetTid => Ok(LoweredWindows::Single(AsmInstruction {
            id: replaces_id,
            opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
            kind: AsmInstructionKind::Call {
                function: AsmValue::Function("kernel32!GetCurrentThreadId".to_string()),
                args: Vec::new(),
                calling_convention: CallingConvention::Win64,
                tail_call: false,
            },
            type_hint: Some(AsmType::I64),
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        })),
        SystemApiOp::Dlopen { path, .. } => Ok(LoweredWindows::Single(AsmInstruction {
            id: replaces_id,
            opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
            kind: AsmInstructionKind::Call {
                function: AsmValue::Function("kernel32!LoadLibraryA".to_string()),
                args: vec![path],
                calling_convention: CallingConvention::Win64,
                tail_call: false,
            },
            type_hint: Some(AsmType::I64),
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        })),
        SystemApiOp::Dlsym { handle, symbol } => Ok(LoweredWindows::Single(AsmInstruction {
            id: replaces_id,
            opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
            kind: AsmInstructionKind::Call {
                function: AsmValue::Function("kernel32!GetProcAddress".to_string()),
                args: vec![handle, symbol],
                calling_convention: CallingConvention::Win64,
                tail_call: false,
            },
            type_hint: Some(AsmType::I64),
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        })),
        SystemApiOp::Dlclose { handle } => {
            let freelib_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

            let freelib = AsmInstruction {
                id: freelib_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function: AsmValue::Function("kernel32!FreeLibrary".to_string()),
                    args: vec![handle],
                    calling_convention: CallingConvention::Win64,
                    tail_call: false,
                },
                type_hint: Some(AsmType::I1),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let cmp = AsmInstruction {
                id: cmp_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Eq),
                kind: AsmInstructionKind::Eq(
                    AsmValue::Register(freelib_id),
                    AsmValue::Constant(AsmConstant::Bool(false)),
                ),
                type_hint: Some(AsmType::I1),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let select = AsmInstruction {
                id: replaces_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Select),
                kind: AsmInstructionKind::Select {
                    condition: AsmValue::Register(cmp_id),
                    if_true: AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)),
                    if_false: AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                },
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            Ok(LoweredWindows::Sequence(vec![freelib, cmp, select]))
        }
        SystemApiOp::Write { fd, buffer, len } => {
            let (handle_value, std_handle_code) =
                match resolve_i64(&fd, instructions).ok().flatten() {
                    Some(fd) => {
                        if fd == 0 {
                            return Ok(LoweredWindows::Unchanged);
                        }
                        let Some(code) = fd_to_std_handle_code(fd) else {
                            return Ok(LoweredWindows::Unchanged);
                        };
                        (None, Some(code))
                    }
                    None => (Some(fd), None),
                };

            let getstd_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let alloca_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let writefile_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let load_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

            let (prefix, handle_arg) = if let Some(std_handle_code) = std_handle_code {
                let getstd = AsmInstruction {
                    id: getstd_id,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("kernel32!GetStdHandle".to_string()),
                        args: vec![AsmValue::Constant(AsmConstant::Int(
                            std_handle_code,
                            AsmType::I64,
                        ))],
                        calling_convention: CallingConvention::Win64,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::Ptr(Box::new(AsmType::I8))),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                };
                (Some(getstd), AsmValue::Register(getstd_id))
            } else {
                (
                    None,
                    handle_value
                        .ok_or_else(|| fp_core::error::Error::from("missing write handle"))?,
                )
            };

            let alloca_written = AsmInstruction {
                id: alloca_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Alloca),
                kind: AsmInstructionKind::Alloca {
                    size: AsmValue::Constant(AsmConstant::UInt(1, AsmType::I64)),
                    alignment: 8,
                },
                type_hint: Some(AsmType::Ptr(Box::new(AsmType::I64))),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let writefile = AsmInstruction {
                id: writefile_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function: AsmValue::Function("kernel32!WriteFile".to_string()),
                    args: vec![
                        handle_arg,
                        buffer,
                        len,
                        AsmValue::Register(alloca_id),
                        AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                    ],
                    calling_convention: CallingConvention::Win64,
                    tail_call: false,
                },
                type_hint: Some(AsmType::I1),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let load_written = AsmInstruction {
                id: load_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: AsmValue::Register(alloca_id),
                    alignment: Some(8),
                    volatile: false,
                },
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let cmp = AsmInstruction {
                id: cmp_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Eq),
                kind: AsmInstructionKind::Eq(
                    AsmValue::Register(writefile_id),
                    AsmValue::Constant(AsmConstant::Bool(false)),
                ),
                type_hint: Some(AsmType::I1),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let select = AsmInstruction {
                id: replaces_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Select),
                kind: AsmInstructionKind::Select {
                    condition: AsmValue::Register(cmp_id),
                    if_true: AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)),
                    if_false: AsmValue::Register(load_id),
                },
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let mut seq = Vec::new();
            if let Some(prefix) = prefix {
                seq.push(prefix);
            }
            seq.extend_from_slice(&[alloca_written, writefile, load_written, cmp, select]);
            Ok(LoweredWindows::Sequence(seq))
        }
        SystemApiOp::Read { fd, buffer, len } => {
            let (handle_value, use_stdio) = match resolve_i64(&fd, instructions).ok().flatten() {
                Some(0) => (None, true),
                Some(_) => return Ok(LoweredWindows::Unchanged),
                None => (Some(fd), false),
            };

            let getstd_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let alloca_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let readfile_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let load_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

            let (prefix, handle_arg) = if use_stdio {
                let getstd = AsmInstruction {
                    id: getstd_id,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("kernel32!GetStdHandle".to_string()),
                        args: vec![AsmValue::Constant(AsmConstant::Int(-10, AsmType::I64))],
                        calling_convention: CallingConvention::Win64,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::Ptr(Box::new(AsmType::I8))),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                };
                (Some(getstd), AsmValue::Register(getstd_id))
            } else {
                (
                    None,
                    handle_value
                        .ok_or_else(|| fp_core::error::Error::from("missing read handle"))?,
                )
            };

            let alloca_read = AsmInstruction {
                id: alloca_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Alloca),
                kind: AsmInstructionKind::Alloca {
                    size: AsmValue::Constant(AsmConstant::UInt(1, AsmType::I64)),
                    alignment: 8,
                },
                type_hint: Some(AsmType::Ptr(Box::new(AsmType::I64))),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let readfile = AsmInstruction {
                id: readfile_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function: AsmValue::Function("kernel32!ReadFile".to_string()),
                    args: vec![
                        handle_arg,
                        buffer,
                        len,
                        AsmValue::Register(alloca_id),
                        AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                    ],
                    calling_convention: CallingConvention::Win64,
                    tail_call: false,
                },
                type_hint: Some(AsmType::I1),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let load_read = AsmInstruction {
                id: load_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: AsmValue::Register(alloca_id),
                    alignment: Some(8),
                    volatile: false,
                },
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let cmp = AsmInstruction {
                id: cmp_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Eq),
                kind: AsmInstructionKind::Eq(
                    AsmValue::Register(readfile_id),
                    AsmValue::Constant(AsmConstant::Bool(false)),
                ),
                type_hint: Some(AsmType::I1),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let select = AsmInstruction {
                id: replaces_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Select),
                kind: AsmInstructionKind::Select {
                    condition: AsmValue::Register(cmp_id),
                    if_true: AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)),
                    if_false: AsmValue::Register(load_id),
                },
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let mut seq = Vec::new();
            if let Some(prefix) = prefix {
                seq.push(prefix);
            }
            seq.extend_from_slice(&[alloca_read, readfile, load_read, cmp, select]);
            Ok(LoweredWindows::Sequence(seq))
        }
        SystemApiOp::Close { fd } => {
            let (handle_value, std_handle_code) =
                match resolve_i64(&fd, instructions).ok().flatten() {
                    Some(fd) => {
                        let Some(code) = fd_to_std_handle_code(fd) else {
                            return Ok(LoweredWindows::Unchanged);
                        };
                        (None, Some(code))
                    }
                    None => (Some(fd), None),
                };

            let getstd_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let close_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

            let (prefix, handle_arg) = if let Some(std_handle_code) = std_handle_code {
                let getstd = AsmInstruction {
                    id: getstd_id,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("kernel32!GetStdHandle".to_string()),
                        args: vec![AsmValue::Constant(AsmConstant::Int(
                            std_handle_code,
                            AsmType::I64,
                        ))],
                        calling_convention: CallingConvention::Win64,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::Ptr(Box::new(AsmType::I8))),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                };
                (Some(getstd), AsmValue::Register(getstd_id))
            } else {
                (
                    None,
                    handle_value
                        .ok_or_else(|| fp_core::error::Error::from("missing close handle"))?,
                )
            };

            let close = AsmInstruction {
                id: close_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function: AsmValue::Function("kernel32!CloseHandle".to_string()),
                    args: vec![handle_arg],
                    calling_convention: CallingConvention::Win64,
                    tail_call: false,
                },
                type_hint: Some(AsmType::I1),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let cmp = AsmInstruction {
                id: cmp_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Eq),
                kind: AsmInstructionKind::Eq(
                    AsmValue::Register(close_id),
                    AsmValue::Constant(AsmConstant::Bool(false)),
                ),
                type_hint: Some(AsmType::I1),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let select = AsmInstruction {
                id: replaces_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Select),
                kind: AsmInstructionKind::Select {
                    condition: AsmValue::Register(cmp_id),
                    if_true: AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)),
                    if_false: AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                },
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let mut seq = Vec::new();
            if let Some(prefix) = prefix {
                seq.push(prefix);
            }
            seq.extend_from_slice(&[close, cmp, select]);
            Ok(LoweredWindows::Sequence(seq))
        }
        SystemApiOp::Open {
            path,
            flags,
            flag_style,
            ..
        } => {
            let Some(flags) = resolve_i64(&flags, instructions)? else {
                return Ok(LoweredWindows::Unchanged);
            };

            // Win32 constants.
            const FILE_SHARE_READ: i64 = 0x0000_0001;
            const FILE_SHARE_WRITE: i64 = 0x0000_0002;
            const FILE_SHARE_DELETE: i64 = 0x0000_0004;
            const FILE_ATTRIBUTE_NORMAL: i64 = 0x0000_0080;

            let desired_access = windows_createfile_desired_access(flags);
            let disposition = windows_createfile_disposition_from_flags(flag_style, flags);

            Ok(LoweredWindows::Single(AsmInstruction {
                id: replaces_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function: AsmValue::Function("kernel32!CreateFileA".to_string()),
                    args: vec![
                        path,
                        AsmValue::Constant(AsmConstant::Int(desired_access, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(
                            FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                            AsmType::I64,
                        )),
                        AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                        AsmValue::Constant(AsmConstant::Int(disposition, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(FILE_ATTRIBUTE_NORMAL, AsmType::I64)),
                        AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                    ],
                    calling_convention: CallingConvention::Win64,
                    tail_call: false,
                },
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            }))
        }
        SystemApiOp::Seek { fd, offset, whence } => {
            let (handle_value, std_handle_code) =
                match resolve_i64(&fd, instructions).ok().flatten() {
                    Some(fd) => {
                        let Some(code) = fd_to_std_handle_code(fd) else {
                            return Ok(LoweredWindows::Unchanged);
                        };
                        (None, Some(code))
                    }
                    None => (Some(fd), None),
                };

            let getstd_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let alloca_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let setfp_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let load_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

            let (prefix, handle_arg) = if let Some(std_handle_code) = std_handle_code {
                let getstd = AsmInstruction {
                    id: getstd_id,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("kernel32!GetStdHandle".to_string()),
                        args: vec![AsmValue::Constant(AsmConstant::Int(
                            std_handle_code,
                            AsmType::I64,
                        ))],
                        calling_convention: CallingConvention::Win64,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::Ptr(Box::new(AsmType::I8))),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                };
                (Some(getstd), AsmValue::Register(getstd_id))
            } else {
                (
                    None,
                    handle_value
                        .ok_or_else(|| fp_core::error::Error::from("missing seek handle"))?,
                )
            };

            let alloca_new_pos = AsmInstruction {
                id: alloca_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Alloca),
                kind: AsmInstructionKind::Alloca {
                    size: AsmValue::Constant(AsmConstant::UInt(1, AsmType::I64)),
                    alignment: 8,
                },
                type_hint: Some(AsmType::Ptr(Box::new(AsmType::I64))),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let setfp = AsmInstruction {
                id: setfp_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function: AsmValue::Function("kernel32!SetFilePointerEx".to_string()),
                    args: vec![handle_arg, offset, AsmValue::Register(alloca_id), whence],
                    calling_convention: CallingConvention::Win64,
                    tail_call: false,
                },
                type_hint: Some(AsmType::I1),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let load_new_pos = AsmInstruction {
                id: load_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: AsmValue::Register(alloca_id),
                    alignment: Some(8),
                    volatile: false,
                },
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let cmp = AsmInstruction {
                id: cmp_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Eq),
                kind: AsmInstructionKind::Eq(
                    AsmValue::Register(setfp_id),
                    AsmValue::Constant(AsmConstant::Bool(false)),
                ),
                type_hint: Some(AsmType::I1),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let select = AsmInstruction {
                id: replaces_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Select),
                kind: AsmInstructionKind::Select {
                    condition: AsmValue::Register(cmp_id),
                    if_true: AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)),
                    if_false: AsmValue::Register(load_id),
                },
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let mut seq = Vec::new();
            if let Some(prefix) = prefix {
                seq.push(prefix);
            }
            seq.extend_from_slice(&[alloca_new_pos, setfp, load_new_pos, cmp, select]);
            Ok(LoweredWindows::Sequence(seq))
        }
        SystemApiOp::Mmap {
            addr,
            len,
            prot,
            flags: _,
            fd,
            offset,
        } => {
            let fd_value = resolve_i64(&fd, instructions).ok().flatten();
            let offset_value = resolve_i64(&offset, instructions).ok().flatten();
            if fd_value != Some(-1) || offset_value != Some(0) {
                return Ok(LoweredWindows::Unchanged);
            }
            let Some(prot) = resolve_i64(&prot, instructions).ok().flatten() else {
                return Ok(LoweredWindows::Unchanged);
            };

            // MEM_COMMIT=0x1000, MEM_RESERVE=0x2000
            const MEM_COMMIT_RESERVE: i64 = 0x3000;
            let protection = windows_page_protection_from_posix(prot);

            let call_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

            let call = AsmInstruction {
                id: call_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function: AsmValue::Function("kernel32!VirtualAlloc".to_string()),
                    args: vec![
                        addr,
                        len,
                        AsmValue::Constant(AsmConstant::Int(MEM_COMMIT_RESERVE, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(protection, AsmType::I64)),
                    ],
                    calling_convention: CallingConvention::Win64,
                    tail_call: false,
                },
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let cmp = AsmInstruction {
                id: cmp_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Eq),
                kind: AsmInstructionKind::Eq(
                    AsmValue::Register(call_id),
                    AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                ),
                type_hint: Some(AsmType::I1),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let select = AsmInstruction {
                id: replaces_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Select),
                kind: AsmInstructionKind::Select {
                    condition: AsmValue::Register(cmp_id),
                    if_true: AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)),
                    if_false: AsmValue::Register(call_id),
                },
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            Ok(LoweredWindows::Sequence(vec![call, cmp, select]))
        }
        SystemApiOp::Munmap { addr, len: _ } => {
            const MEM_RELEASE: i64 = 0x8000;
            let call_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

            let call = AsmInstruction {
                id: call_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function: AsmValue::Function("kernel32!VirtualFree".to_string()),
                    args: vec![
                        addr,
                        AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(MEM_RELEASE, AsmType::I64)),
                    ],
                    calling_convention: CallingConvention::Win64,
                    tail_call: false,
                },
                type_hint: Some(AsmType::I1),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let cmp = AsmInstruction {
                id: cmp_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Eq),
                kind: AsmInstructionKind::Eq(
                    AsmValue::Register(call_id),
                    AsmValue::Constant(AsmConstant::Bool(false)),
                ),
                type_hint: Some(AsmType::I1),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            let select = AsmInstruction {
                id: replaces_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Select),
                kind: AsmInstructionKind::Select {
                    condition: AsmValue::Register(cmp_id),
                    if_true: AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)),
                    if_false: AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                },
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            Ok(LoweredWindows::Sequence(vec![call, cmp, select]))
        }
    }
}

fn match_writefile_sequence_to_syscall(
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
        let Some(handle_code) = args
            .first()
            .and_then(|value| resolve_i64(value, instructions).ok())
        else {
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
            type_hint: Some(AsmType::I64),
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

fn match_readfile_sequence_to_syscall(
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
        let Some(handle_code) = args
            .first()
            .and_then(|value| resolve_i64(value, instructions).ok())
        else {
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
            type_hint: Some(AsmType::I64),
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

fn match_setfilepointerex_sequence_to_syscall(
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
        let Some(handle_code) = args
            .first()
            .and_then(|value| resolve_i64(value, instructions).ok())
        else {
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
            type_hint: Some(AsmType::I64),
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

fn match_virtualalloc_sequence_to_syscall(
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
            type_hint: Some(AsmType::I64),
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

fn match_virtualfree_sequence_to_syscall(
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
            type_hint: Some(AsmType::I64),
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

fn match_result_chain_at(
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

fn is_call_named(inst: &AsmInstruction, dll: &str, name: &str) -> bool {
    let AsmInstructionKind::Call { function, .. } = &inst.kind else {
        return false;
    };
    let AsmValue::Function(symbol) = function else {
        return false;
    };
    let (sym_dll, sym_name) = split_import_symbol(symbol);
    sym_dll.eq_ignore_ascii_case(dll) && sym_name == name
}

fn lower_system_api_to_syscall(
    op: SystemApiOp,
    convention: AsmSyscallConvention,
) -> AsmInstructionKind {
    match op {
        SystemApiOp::Exit { code } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 60,
                AsmSyscallConvention::LinuxAarch64 => 93,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0001
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args: vec![code],
            }
        }
        SystemApiOp::GetPid => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 39,
                AsmSyscallConvention::LinuxAarch64 => 172,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0014
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args: Vec::new(),
            }
        }
        SystemApiOp::GetTid => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 186,
                AsmSyscallConvention::LinuxAarch64 => 178,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    // No stable cross-version darwin thread id syscall.
                    0
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args: Vec::new(),
            }
        }
        SystemApiOp::Write { fd, buffer, len } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 1,
                AsmSyscallConvention::LinuxAarch64 => 64,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0004
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args: vec![fd, buffer, len],
            }
        }
        SystemApiOp::Dlopen { .. } | SystemApiOp::Dlsym { .. } | SystemApiOp::Dlclose { .. } => {
            AsmInstructionKind::Freeze(AsmValue::Undef(AsmType::I64))
        }
        SystemApiOp::Read { fd, buffer, len } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 0,
                AsmSyscallConvention::LinuxAarch64 => 63,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0003
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args: vec![fd, buffer, len],
            }
        }
        SystemApiOp::Close { fd } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 3,
                AsmSyscallConvention::LinuxAarch64 => 57,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0006
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args: vec![fd],
            }
        }
        SystemApiOp::Open {
            path, flags, mode, ..
        } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 2,
                AsmSyscallConvention::LinuxAarch64 => 56,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0005
                }
            };
            let args = match convention {
                AsmSyscallConvention::LinuxAarch64 => vec![
                    AsmValue::Constant(AsmConstant::Int(-100, AsmType::I64)),
                    path,
                    flags,
                    mode,
                ],
                _ => vec![path, flags, mode],
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args,
            }
        }
        SystemApiOp::Seek { fd, offset, whence } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 8,
                AsmSyscallConvention::LinuxAarch64 => 62,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_00c7
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args: vec![fd, offset, whence],
            }
        }
        SystemApiOp::Mmap {
            addr,
            len,
            prot,
            flags,
            fd,
            offset,
        } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 9,
                AsmSyscallConvention::LinuxAarch64 => 222,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_00c5
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args: vec![addr, len, prot, flags, fd, offset],
            }
        }
        SystemApiOp::Munmap { addr, len } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 11,
                AsmSyscallConvention::LinuxAarch64 => 215,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0049
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args: vec![addr, len],
            }
        }
    }
}

fn resolve_u64(value: &AsmValue, instructions: &[AsmInstruction]) -> Option<u64> {
    match value {
        AsmValue::Constant(AsmConstant::UInt(x, _)) => Some(*x),
        AsmValue::Constant(AsmConstant::Int(x, _)) => (*x).try_into().ok(),
        AsmValue::Register(id) => {
            let inst = instructions.iter().find(|inst| inst.id == *id)?;
            match &inst.kind {
                AsmInstructionKind::Freeze(inner) => resolve_u64(inner, instructions),
                _ => None,
            }
        }
        _ => None,
    }
}

fn resolve_i64(value: &AsmValue, instructions: &[AsmInstruction]) -> Result<Option<i64>> {
    Ok(match value {
        AsmValue::Constant(AsmConstant::Int(x, _)) => Some(*x),
        AsmValue::Constant(AsmConstant::UInt(x, _)) => i64::try_from(*x).ok(),
        AsmValue::Register(id) => {
            let Some(inst) = instructions.iter().find(|inst| inst.id == *id) else {
                return Ok(None);
            };
            match &inst.kind {
                AsmInstructionKind::Freeze(inner) => resolve_i64(inner, instructions)?,
                _ => None,
            }
        }
        _ => None,
    })
}

fn split_import_symbol(symbol: &str) -> (String, String) {
    const DEFAULT_DLL: &str = "msvcrt.dll";
    if let Some((dll, name)) = symbol.split_once('!') {
        let mut dll = dll.trim().to_string();
        if !dll.to_ascii_lowercase().ends_with(".dll") {
            dll.push_str(".dll");
        }
        return (dll, name.trim().to_string());
    }
    (DEFAULT_DLL.to_string(), symbol.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::asmir::{AsmArchitecture, AsmEndianness, AsmTarget};

    fn program(target_format: AsmObjectFormat) -> AsmProgram {
        AsmProgram::new(AsmTarget {
            architecture: AsmArchitecture::X86_64,
            object_format: target_format,
            endianness: AsmEndianness::Little,
            pointer_width: 64,
            default_calling_convention: None,
        })
    }

    #[test]
    fn rewrite_linux_write_syscall_to_windows_writefile_sequence() {
        let mut prog = program(AsmObjectFormat::Coff);
        prog.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("main"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Syscall),
                    kind: AsmInstructionKind::Syscall {
                        convention: AsmSyscallConvention::LinuxX86_64,
                        number: AsmValue::Constant(AsmConstant::UInt(1, AsmType::I64)),
                        args: vec![
                            AsmValue::Constant(AsmConstant::UInt(1, AsmType::I64)),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                        ],
                    },
                    type_hint: Some(AsmType::I64),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                }],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: fp_core::lir::Linkage::External,
            visibility: fp_core::lir::Visibility::Default,
            calling_convention: None,
            section: None,
            is_declaration: false,
        });

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| is_call_named(
            inst,
            "kernel32.dll",
            "WriteFile"
        )));
        assert!(block.instructions.iter().any(|inst| is_call_named(
            inst,
            "kernel32.dll",
            "GetStdHandle"
        )));
        assert!(matches!(
            block.terminator,
            fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0)))
        ));
    }

    #[test]
    fn rewrite_windows_writefile_sequence_back_to_linux_syscall() {
        let mut prog = program(AsmObjectFormat::Elf);
        prog.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("main"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: vec![
                    // GetStdHandle(-11)
                    AsmInstruction {
                        id: 1,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("kernel32!GetStdHandle".to_string()),
                            args: vec![AsmValue::Constant(AsmConstant::Int(-11, AsmType::I64))],
                            calling_convention: CallingConvention::Win64,
                            tail_call: false,
                        },
                        type_hint: Some(AsmType::Ptr(Box::new(AsmType::I8))),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    // alloca written
                    AsmInstruction {
                        id: 2,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Alloca),
                        kind: AsmInstructionKind::Alloca {
                            size: AsmValue::Constant(AsmConstant::UInt(1, AsmType::I64)),
                            alignment: 8,
                        },
                        type_hint: Some(AsmType::Ptr(Box::new(AsmType::I64))),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    // WriteFile(handle, null, 0, ptr, null)
                    AsmInstruction {
                        id: 3,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("kernel32!WriteFile".to_string()),
                            args: vec![
                                AsmValue::Register(1),
                                AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                                AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                                AsmValue::Register(2),
                                AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            ],
                            calling_convention: CallingConvention::Win64,
                            tail_call: false,
                        },
                        type_hint: Some(AsmType::I1),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    // load written -> id 0
                    AsmInstruction {
                        id: 0,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Load),
                        kind: AsmInstructionKind::Load {
                            address: AsmValue::Register(2),
                            alignment: Some(8),
                            volatile: false,
                        },
                        type_hint: Some(AsmType::I64),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                ],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: fp_core::lir::Linkage::External,
            visibility: fp_core::lir::Visibility::Default,
            calling_convention: None,
            section: None,
            is_declaration: false,
        });

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| {
            matches!(
                inst.kind,
                AsmInstructionKind::Syscall {
                    number: AsmValue::Constant(AsmConstant::UInt(1, _)),
                    ..
                }
            )
        }));
    }

    #[test]
    fn rewrite_linux_read_syscall_to_windows_readfile_sequence() {
        let mut prog = program(AsmObjectFormat::Coff);
        prog.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("main"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Syscall),
                    kind: AsmInstructionKind::Syscall {
                        convention: AsmSyscallConvention::LinuxX86_64,
                        number: AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                        args: vec![
                            AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                        ],
                    },
                    type_hint: Some(AsmType::I64),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                }],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: fp_core::lir::Linkage::External,
            visibility: fp_core::lir::Visibility::Default,
            calling_convention: None,
            section: None,
            is_declaration: false,
        });

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| is_call_named(
            inst,
            "kernel32.dll",
            "ReadFile"
        )));
        assert!(block.instructions.iter().any(|inst| is_call_named(
            inst,
            "kernel32.dll",
            "GetStdHandle"
        )));
        assert!(matches!(
            block.terminator,
            fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0)))
        ));
    }

    #[test]
    fn rewrite_windows_readfile_sequence_back_to_linux_syscall() {
        let mut prog = program(AsmObjectFormat::Elf);
        prog.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("main"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: vec![
                    // GetStdHandle(-10)
                    AsmInstruction {
                        id: 1,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("kernel32!GetStdHandle".to_string()),
                            args: vec![AsmValue::Constant(AsmConstant::Int(-10, AsmType::I64))],
                            calling_convention: CallingConvention::Win64,
                            tail_call: false,
                        },
                        type_hint: Some(AsmType::Ptr(Box::new(AsmType::I8))),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    // alloca read
                    AsmInstruction {
                        id: 2,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Alloca),
                        kind: AsmInstructionKind::Alloca {
                            size: AsmValue::Constant(AsmConstant::UInt(1, AsmType::I64)),
                            alignment: 8,
                        },
                        type_hint: Some(AsmType::Ptr(Box::new(AsmType::I64))),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    // ReadFile(handle, null, 0, ptr, null)
                    AsmInstruction {
                        id: 3,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("kernel32!ReadFile".to_string()),
                            args: vec![
                                AsmValue::Register(1),
                                AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                                AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                                AsmValue::Register(2),
                                AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            ],
                            calling_convention: CallingConvention::Win64,
                            tail_call: false,
                        },
                        type_hint: Some(AsmType::I1),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    // load read -> id 0
                    AsmInstruction {
                        id: 0,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Load),
                        kind: AsmInstructionKind::Load {
                            address: AsmValue::Register(2),
                            alignment: Some(8),
                            volatile: false,
                        },
                        type_hint: Some(AsmType::I64),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                ],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: fp_core::lir::Linkage::External,
            visibility: fp_core::lir::Visibility::Default,
            calling_convention: None,
            section: None,
            is_declaration: false,
        });

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| {
            matches!(
                inst.kind,
                AsmInstructionKind::Syscall {
                    number: AsmValue::Constant(AsmConstant::UInt(0, _)),
                    ..
                }
            )
        }));
    }

    #[test]
    fn rewrite_linux_close_syscall_to_windows_closehandle_sequence() {
        let mut prog = program(AsmObjectFormat::Coff);
        prog.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("main"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Syscall),
                    kind: AsmInstructionKind::Syscall {
                        convention: AsmSyscallConvention::LinuxX86_64,
                        number: AsmValue::Constant(AsmConstant::UInt(3, AsmType::I64)),
                        args: vec![AsmValue::Constant(AsmConstant::UInt(1, AsmType::I64))],
                    },
                    type_hint: Some(AsmType::I64),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                }],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: fp_core::lir::Linkage::External,
            visibility: fp_core::lir::Visibility::Default,
            calling_convention: None,
            section: None,
            is_declaration: false,
        });

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| is_call_named(
            inst,
            "kernel32.dll",
            "CloseHandle"
        )));
        assert!(block.instructions.iter().any(|inst| is_call_named(
            inst,
            "kernel32.dll",
            "GetStdHandle"
        )));
    }

    #[test]
    fn rewrite_windows_closehandle_sequence_back_to_linux_syscall() {
        let mut prog = program(AsmObjectFormat::Elf);
        prog.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("main"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: vec![
                    AsmInstruction {
                        id: 1,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("kernel32!GetStdHandle".to_string()),
                            args: vec![AsmValue::Constant(AsmConstant::Int(-11, AsmType::I64))],
                            calling_convention: CallingConvention::Win64,
                            tail_call: false,
                        },
                        type_hint: Some(AsmType::Ptr(Box::new(AsmType::I8))),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 2,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("kernel32!CloseHandle".to_string()),
                            args: vec![AsmValue::Register(1)],
                            calling_convention: CallingConvention::Win64,
                            tail_call: false,
                        },
                        type_hint: Some(AsmType::I1),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 3,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Eq),
                        kind: AsmInstructionKind::Eq(
                            AsmValue::Register(2),
                            AsmValue::Constant(AsmConstant::Bool(false)),
                        ),
                        type_hint: Some(AsmType::I1),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 0,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Select),
                        kind: AsmInstructionKind::Select {
                            condition: AsmValue::Register(3),
                            if_true: AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)),
                            if_false: AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        },
                        type_hint: Some(AsmType::I64),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                ],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: fp_core::lir::Linkage::External,
            visibility: fp_core::lir::Visibility::Default,
            calling_convention: None,
            section: None,
            is_declaration: false,
        });

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| {
            matches!(
                inst.kind,
                AsmInstructionKind::Syscall {
                    number: AsmValue::Constant(AsmConstant::UInt(3, _)),
                    ..
                }
            )
        }));
    }
}
