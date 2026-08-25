use super::*;

pub(super) fn lower_system_api_to_windows_import(
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
            // ExitProcess is `noreturn` at the OS ABI level, but our AsmIR currently
            // models call results as SSA values that may be referenced by later
            // instructions (e.g. through generic lowering patterns). Keep this typed
            // as an integer to avoid codegen attempting to materialize a `Void` value.
            ty: AsmType::I64,
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
            ty: AsmType::I64,
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
            ty: AsmType::I64,
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
            ty: AsmType::I64,
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
            ty: AsmType::I64,
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
                ty: AsmType::I1,
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
                ty: AsmType::I1,
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
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            Ok(LoweredWindows::Sequence(vec![freelib, cmp, select]))
        }
        SystemApiOp::Unlink { path } => {
            let call_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

            let call = AsmInstruction {
                id: call_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function: AsmValue::Function("kernel32!DeleteFileA".to_string()),
                    args: vec![path],
                    calling_convention: CallingConvention::Win64,
                    tail_call: false,
                },
                ty: AsmType::I1,
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
                ty: AsmType::I1,
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
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };
            Ok(LoweredWindows::Sequence(vec![call, cmp, select]))
        }
        SystemApiOp::Mkdir { path, .. } => {
            let call_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

            let call = AsmInstruction {
                id: call_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function: AsmValue::Function("kernel32!CreateDirectoryA".to_string()),
                    args: vec![path, AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))],
                    calling_convention: CallingConvention::Win64,
                    tail_call: false,
                },
                ty: AsmType::I1,
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
                ty: AsmType::I1,
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
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };
            Ok(LoweredWindows::Sequence(vec![call, cmp, select]))
        }
        SystemApiOp::Rmdir { path } => {
            let call_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

            let call = AsmInstruction {
                id: call_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function: AsmValue::Function("kernel32!RemoveDirectoryA".to_string()),
                    args: vec![path],
                    calling_convention: CallingConvention::Win64,
                    tail_call: false,
                },
                ty: AsmType::I1,
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
                ty: AsmType::I1,
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
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };
            Ok(LoweredWindows::Sequence(vec![call, cmp, select]))
        }
        SystemApiOp::Rename { from, to } => {
            // MOVEFILE_REPLACE_EXISTING=1
            const MOVEFILE_REPLACE_EXISTING: i64 = 1;

            let call_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

            let call = AsmInstruction {
                id: call_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function: AsmValue::Function("kernel32!MoveFileExA".to_string()),
                    args: vec![
                        from,
                        to,
                        AsmValue::Constant(AsmConstant::Int(
                            MOVEFILE_REPLACE_EXISTING,
                            AsmType::I64,
                        )),
                    ],
                    calling_convention: CallingConvention::Win64,
                    tail_call: false,
                },
                ty: AsmType::I1,
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
                ty: AsmType::I1,
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
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };
            Ok(LoweredWindows::Sequence(vec![call, cmp, select]))
        }
        SystemApiOp::Access { path, .. } => {
            let call_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

            let call = AsmInstruction {
                id: call_id,
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function: AsmValue::Function("kernel32!GetFileAttributesA".to_string()),
                    args: vec![path],
                    calling_convention: CallingConvention::Win64,
                    tail_call: false,
                },
                ty: AsmType::I64,
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
                    AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)),
                ),
                ty: AsmType::I1,
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
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            Ok(LoweredWindows::Sequence(vec![call, cmp, select]))
        }
        SystemApiOp::Write { fd, buffer, len } => {
            let (handle_value, std_handle_code) = match resolve_i64(&fd, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()
            {
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
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
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
                ty: AsmType::Ptr(Box::new(AsmType::I64)),
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
                ty: AsmType::I1,
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
                ty: AsmType::I64,
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
                ty: AsmType::I1,
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
                ty: AsmType::I64,
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
            let (handle_value, use_stdio) = match resolve_i64(&fd, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()
            {
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
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
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
                ty: AsmType::Ptr(Box::new(AsmType::I64)),
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
                ty: AsmType::I1,
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
                ty: AsmType::I64,
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
                ty: AsmType::I1,
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
                ty: AsmType::I64,
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
            let (handle_value, std_handle_code) = match resolve_i64(&fd, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()
            {
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
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
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
                ty: AsmType::I1,
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
                ty: AsmType::I1,
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
                ty: AsmType::I64,
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
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            }))
        }
        SystemApiOp::Seek { fd, offset, whence } => {
            let (handle_value, std_handle_code) = match resolve_i64(&fd, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()
            {
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
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
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
                ty: AsmType::Ptr(Box::new(AsmType::I64)),
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
                ty: AsmType::I1,
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
                ty: AsmType::I64,
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
                ty: AsmType::I1,
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
                ty: AsmType::I64,
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
            let fd_value = resolve_i64(&fd, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten();
            let offset_value = resolve_i64(&offset, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten();
            if fd_value != Some(-1) || offset_value != Some(0) {
                return Ok(LoweredWindows::Unchanged);
            }
            let Some(prot) = resolve_i64(&prot, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()
            else {
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
                ty: AsmType::I64,
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
                ty: AsmType::I1,
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
                ty: AsmType::I64,
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
                ty: AsmType::I1,
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
                ty: AsmType::I1,
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
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            };

            Ok(LoweredWindows::Sequence(vec![call, cmp, select]))
        }

        SystemApiOp::Opendir { .. }
        | SystemApiOp::Readdir { .. }
        | SystemApiOp::Closedir { .. } => Err(Error::from(
            "directory SysOps are not supported for Windows targets yet",
        )),
    }
}
