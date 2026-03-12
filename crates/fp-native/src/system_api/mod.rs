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
}

fn match_closehandle_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    if instructions.len() < 4 {
        return Ok(None);
    }
    let getstd = &instructions[0];
    let close = &instructions[1];
    let cmp = &instructions[2];
    let select = &instructions[3];

    if !is_call_named(getstd, "kernel32.dll", "GetStdHandle") {
        return Ok(None);
    }
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

    let AsmInstructionKind::Call {
        args: close_args, ..
    } = &close.kind
    else {
        return Ok(None);
    };
    if close_args.len() != 1 {
        return Ok(None);
    }
    if close_args[0] != AsmValue::Register(getstd.id) {
        return Ok(None);
    }

    let op = SystemApiOp::Close {
        fd: AsmValue::Constant(AsmConstant::UInt(fd, AsmType::I64)),
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
        4,
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
        rewrite_syscalls_to_windows_imports(program)?;
    } else {
        // Unix targets: if we see known Windows API patterns, rewrite them back to syscalls.
        rewrite_windows_imports_to_syscalls(program)?;
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

                let mut inst = block.instructions[i].clone();
                if let Some(op) = detect_system_api_from_windows_import(&inst.kind) {
                    inst.kind = lower_system_api_to_syscall(op, convention);
                    inst.opcode = AsmOpcode::Generic(AsmGenericOpcode::Syscall);
                    inst.type_hint = Some(AsmType::I64);
                }
                out.push(inst);
                i += 1;
            }
            block.instructions = out;
        }
    }
    Ok(())
}

fn detect_system_api_from_windows_import(kind: &AsmInstructionKind) -> Option<SystemApiOp> {
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
        _ => None,
    }
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
        AsmSyscallConvention::LinuxAarch64 if num == 93 => Some(SystemApiOp::Exit {
            code: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32))),
        }),
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
        SystemApiOp::Write { fd, buffer, len } => {
            let Some(fd) = resolve_i64(&fd, instructions)? else {
                return Ok(LoweredWindows::Unchanged);
            };
            let Some(std_handle_code) = fd_to_std_handle_code(fd) else {
                return Ok(LoweredWindows::Unchanged);
            };
            if fd == 0 {
                return Ok(LoweredWindows::Unchanged);
            }

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
                        AsmValue::Register(getstd_id),
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

            Ok(LoweredWindows::Sequence(vec![
                getstd,
                alloca_written,
                writefile,
                load_written,
                cmp,
                select,
            ]))
        }
        SystemApiOp::Read { fd, buffer, len } => {
            let Some(fd) = resolve_i64(&fd, instructions)? else {
                return Ok(LoweredWindows::Unchanged);
            };
            if fd != 0 {
                return Ok(LoweredWindows::Unchanged);
            }

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
                        AsmValue::Register(getstd_id),
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

            Ok(LoweredWindows::Sequence(vec![
                getstd,
                alloca_read,
                readfile,
                load_read,
                cmp,
                select,
            ]))
        }
        SystemApiOp::Close { fd } => {
            let Some(fd) = resolve_i64(&fd, instructions)? else {
                return Ok(LoweredWindows::Unchanged);
            };
            let Some(std_handle_code) = fd_to_std_handle_code(fd) else {
                return Ok(LoweredWindows::Unchanged);
            };

            let getstd_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let close_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

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

            let close = AsmInstruction {
                id: close_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function: AsmValue::Function("kernel32!CloseHandle".to_string()),
                    args: vec![AsmValue::Register(getstd_id)],
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

            Ok(LoweredWindows::Sequence(vec![getstd, close, cmp, select]))
        }
    }
}

fn match_writefile_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    if instructions.len() < 4 {
        return Ok(None);
    }
    let getstd = &instructions[0];
    let alloca = &instructions[1];
    let writefile = &instructions[2];
    let load = &instructions[3];

    if !is_call_named(getstd, "kernel32.dll", "GetStdHandle") {
        return Ok(None);
    }
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

    let AsmInstructionKind::Call { args, .. } = &writefile.kind else {
        return Ok(None);
    };
    if args.len() < 5 {
        return Ok(None);
    }

    // args: (handle, buffer, len, written_ptr, overlapped)
    if args[0] != AsmValue::Register(getstd.id) {
        return Ok(None);
    }
    if args[3] != AsmValue::Register(alloca.id) {
        return Ok(None);
    }

    let op = SystemApiOp::Write {
        fd: AsmValue::Constant(AsmConstant::UInt(fd, AsmType::I64)),
        buffer: args[1].clone(),
        len: args[2].clone(),
    };

    let (dest_id, consumed) = match_result_chain(instructions, load.id);
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
        consumed,
    )))
}

fn match_readfile_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    if instructions.len() < 4 {
        return Ok(None);
    }
    let getstd = &instructions[0];
    let alloca = &instructions[1];
    let readfile = &instructions[2];
    let load = &instructions[3];

    if !is_call_named(getstd, "kernel32.dll", "GetStdHandle") {
        return Ok(None);
    }
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

    let AsmInstructionKind::Call { args, .. } = &readfile.kind else {
        return Ok(None);
    };
    if args.len() < 5 {
        return Ok(None);
    }

    if args[0] != AsmValue::Register(getstd.id) {
        return Ok(None);
    }
    if args[3] != AsmValue::Register(alloca.id) {
        return Ok(None);
    }

    let op = SystemApiOp::Read {
        fd: AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
        buffer: args[1].clone(),
        len: args[2].clone(),
    };

    let (dest_id, consumed) = match_result_chain(instructions, load.id);
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
        consumed,
    )))
}

fn match_result_chain(instructions: &[AsmInstruction], load_id: u32) -> (u32, usize) {
    // Accept both:
    //   GetStdHandle; alloca; FileOp; Load
    //   GetStdHandle; alloca; FileOp; Load; Eq; Select
    if instructions.len() >= 6 {
        if !matches!(instructions[4].kind, AsmInstructionKind::Eq(_, _)) {
            return (load_id, 4);
        }
        let select = &instructions[5];
        if let AsmInstructionKind::Select { if_false, .. } = &select.kind {
            if if_false == &AsmValue::Register(load_id) {
                return (select.id, 6);
            }
        }
    }
    (load_id, 4)
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
