use super::*;

pub(super) fn match_getfileattributes_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    // Pattern:
    //   GetFileAttributesA; Eq; Select
    if instructions.len() < 3 {
        return Ok(None);
    }

    let call = &instructions[0];
    if !is_call_named(call, "kernel32.dll", "GetFileAttributesA") {
        return Ok(None);
    }
    let AsmInstructionKind::Call { args, .. } = &call.kind else {
        return Ok(None);
    };
    if args.len() != 1 {
        return Ok(None);
    }

    match_kernel32_bool_call_sequence_to_syscall(
        instructions,
        "GetFileAttributesA",
        SystemApiOp::Access {
            path: args[0].clone(),
            mode: AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)),
        },
        convention,
    )
}

pub(super) fn ensure_glibc_progname_globals(program: &mut AsmProgram) {
    ensure_global(
        program,
        AsmGlobal {
            name: Name::new("fp_linux_progname_default"),
            ty: AsmType::Array(Box::new(AsmType::I8), 1),
            initializer: Some(AsmConstant::Bytes(vec![0])),
            relocations: Vec::new(),
            section: Some(".rodata".to_string()),
            linkage: Linkage::Private,
            visibility: Visibility::Default,
            alignment: Some(1),
            is_constant: true,
        },
    );

    for name in [
        "__progname",
        "__progname_full",
        "program_invocation_name",
        "program_invocation_short_name",
    ] {
        ensure_global(
            program,
            AsmGlobal {
                name: Name::new(name),
                ty: AsmType::Ptr(Box::new(AsmType::I8)),
                initializer: Some(AsmConstant::Bytes(vec![0; 8])),
                relocations: vec![AsmGlobalRelocation {
                    offset: 0,
                    kind: AsmRelocationKind::Abs64,
                    symbol: Name::new("fp_linux_progname_default"),
                    addend: 0,
                }],
                section: Some(".data".to_string()),
                linkage: Linkage::External,
                visibility: Visibility::Default,
                alignment: Some(8),
                is_constant: false,
            },
        );
    }
}

pub(super) fn ensure_glibc_overflow(program: &mut AsmProgram) -> Result<()> {
    // glibc uses `__overflow(FILE*, int)` as an internal stdio helper.
    // Provide a compatibility definition that forwards to libc `fputc`.
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("__overflow"),
            signature: AsmFunctionSignature {
                params: vec![AsmType::Ptr(Box::new(AsmType::I8)), AsmType::I32],
                return_type: AsmType::I32,
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("fputc".to_string()),
                        args: vec![AsmValue::Local(1), AsmValue::Local(0)],
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    ty: AsmType::I32,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                }],
                terminator: AsmTerminator::Return(Some(AsmValue::Register(0))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: vec![
                AsmLocal {
                    id: 0,
                    name: Some("stream".to_string()),
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("ch".to_string()),
                    ty: AsmType::I32,
                    is_argument: true,
                },
            ],
            stack_slots: Vec::new(),
            frame: None,
            linkage: Linkage::External,
            visibility: Visibility::Default,
            calling_convention: Some(CallingConvention::C),
            section: Some(".text".to_string()),
            is_declaration: false,
        },
    );
    Ok(())
}

pub(super) fn ensure_glibc_mempcpy(program: &mut AsmProgram) -> Result<()> {
    // Darwin libc doesn't provide mempcpy, but glibc-compiled binaries may.
    // This is a minimal, unsafe compatibility implementation.
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("mempcpy"),
            signature: AsmFunctionSignature {
                params: vec![
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    AsmType::I64,
                ],
                return_type: AsmType::Ptr(Box::new(AsmType::I8)),
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: vec![
                    AsmInstruction {
                        id: 0,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("memcpy".to_string()),
                            args: vec![AsmValue::Local(0), AsmValue::Local(1), AsmValue::Local(2)],
                            calling_convention: CallingConvention::C,
                            tail_call: false,
                        },
                        ty: AsmType::Ptr(Box::new(AsmType::I8)),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 1,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::GetElementPtr),
                        kind: AsmInstructionKind::GetElementPtr {
                            ptr: AsmValue::Local(0),
                            indices: vec![AsmValue::Local(2)],
                            inbounds: false,
                        },
                        ty: AsmType::Ptr(Box::new(AsmType::I8)),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                ],
                terminator: AsmTerminator::Return(Some(AsmValue::Register(1))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: vec![
                AsmLocal {
                    id: 0,
                    name: Some("dest".to_string()),
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("src".to_string()),
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
                    is_argument: true,
                },
                AsmLocal {
                    id: 2,
                    name: Some("len".to_string()),
                    ty: AsmType::I64,
                    is_argument: true,
                },
            ],
            stack_slots: Vec::new(),
            frame: None,
            linkage: Linkage::External,
            visibility: Visibility::Default,
            calling_convention: Some(CallingConvention::C),
            section: Some(".text".to_string()),
            is_declaration: false,
        },
    );
    Ok(())
}

pub(super) fn ensure_glibc_start_main(program: &mut AsmProgram) -> Result<()> {
    // Minimal Linux/glibc entry shim for Darwin targets.
    //
    // We only need this to satisfy references from lifted ELF `_start` code paths.
    // The fp-cli wrapper prefers calling `fp_lifted_main` directly.
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("__libc_start_main"),
            signature: AsmFunctionSignature {
                params: vec![
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    AsmType::I32,
                    AsmType::Ptr(Box::new(AsmType::Ptr(Box::new(AsmType::I8)))),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                ],
                return_type: AsmType::I32,
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: vec![
                    AsmInstruction {
                        id: 0,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::SExt),
                        kind: AsmInstructionKind::SExt(AsmValue::Local(1), AsmType::I64),
                        ty: AsmType::I64,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 1,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Add),
                        kind: AsmInstructionKind::Add(
                            AsmValue::Register(0),
                            AsmValue::Constant(AsmConstant::UInt(1, AsmType::I64)),
                        ),
                        ty: AsmType::I64,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 2,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::GetElementPtr),
                        kind: AsmInstructionKind::GetElementPtr {
                            ptr: AsmValue::Local(2),
                            indices: vec![AsmValue::Register(1)],
                            inbounds: false,
                        },
                        ty: AsmType::Ptr(Box::new(AsmType::Ptr(Box::new(AsmType::I8)))),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 3,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Local(0),
                            args: vec![
                                AsmValue::Local(1),
                                AsmValue::Local(2),
                                AsmValue::Register(2),
                            ],
                            calling_convention: CallingConvention::C,
                            tail_call: false,
                        },
                        ty: AsmType::I32,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 4,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("exit".to_string()),
                            args: vec![AsmValue::Register(3)],
                            calling_convention: CallingConvention::C,
                            tail_call: false,
                        },
                        ty: AsmType::Void,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                ],
                terminator: AsmTerminator::Unreachable,
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: vec![
                AsmLocal {
                    id: 0,
                    name: Some("main".to_string()),
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("argc".to_string()),
                    ty: AsmType::I32,
                    is_argument: true,
                },
                AsmLocal {
                    id: 2,
                    name: Some("argv".to_string()),
                    ty: AsmType::Ptr(Box::new(AsmType::Ptr(Box::new(AsmType::I8)))),
                    is_argument: true,
                },
                AsmLocal {
                    id: 3,
                    name: Some("init".to_string()),
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
                    is_argument: true,
                },
                AsmLocal {
                    id: 4,
                    name: Some("fini".to_string()),
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
                    is_argument: true,
                },
                AsmLocal {
                    id: 5,
                    name: Some("rtld_fini".to_string()),
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
                    is_argument: true,
                },
                AsmLocal {
                    id: 6,
                    name: Some("stack_end".to_string()),
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
                    is_argument: true,
                },
            ],
            stack_slots: Vec::new(),
            frame: None,
            linkage: Linkage::External,
            visibility: Visibility::Default,
            calling_convention: Some(CallingConvention::C),
            section: Some(".text".to_string()),
            is_declaration: false,
        },
    );
    Ok(())
}

pub(crate) fn rewrite_program_to_sys_ops(program: &mut AsmProgram) -> Result<()> {
    let syscall_convention = target_syscall_convention(program);
    let target_object_format = program.target.object_format.clone();
    let source_format = program
        .container
        .as_ref()
        .map(|container| container.format.clone())
        .unwrap_or(target_object_format);
    let posix_dirent_style = match source_format {
        AsmObjectFormat::MachO => PosixDirentStyle::Darwin,
        _ => PosixDirentStyle::Linux,
    };
    for func in &mut program.functions {
        if func.is_declaration {
            continue;
        }
        for block in &mut func.basic_blocks {
            let snapshot = block.instructions.clone();
            for inst in &mut block.instructions {
                if let AsmInstructionKind::Syscall {
                    convention,
                    number,
                    args,
                } = &inst.kind
                {
                    if let Some(op) =
                        detect_system_api_from_syscall(convention, number, args, &snapshot)
                    {
                        inst.kind = AsmInstructionKind::SysOp(op);
                        inst.opcode = AsmOpcode::Generic(AsmGenericOpcode::SysOp);
                    }
                    continue;
                }

                if let Some(op) = detect_system_api_from_posix_call(&inst.kind, posix_dirent_style)
                {
                    inst.kind = AsmInstructionKind::SysOp(op);
                    inst.opcode = AsmOpcode::Generic(AsmGenericOpcode::SysOp);
                    continue;
                }

                if let Some(convention) = syscall_convention {
                    if let Some(op) = detect_system_api_from_windows_import(&inst.kind, convention)
                    {
                        inst.kind = AsmInstructionKind::SysOp(op);
                        inst.opcode = AsmOpcode::Generic(AsmGenericOpcode::SysOp);
                    }
                }
            }
        }
    }
    Ok(())
}

pub(super) fn rewrite_glibc_chk_calls_to_libc(program: &mut AsmProgram) {
    fn chk_call_rewrite(name: &str) -> Option<(&'static str, &'static [usize])> {
        Some(match name {
            "__fprintf_chk" => ("fprintf", &[1]),
            "__printf_chk" => ("printf", &[0]),
            "__sprintf_chk" => ("sprintf", &[1, 2]),
            "__snprintf_chk" => ("snprintf", &[2, 3]),
            "__vfprintf_chk" => ("vfprintf", &[1]),
            "__vsprintf_chk" => ("vsprintf", &[1, 2]),
            "__vsnprintf_chk" => ("vsnprintf", &[2, 3]),
            "__memcpy_chk" => ("memcpy", &[3]),
            "__mempcpy_chk" => ("mempcpy", &[3]),
            "__memmove_chk" => ("memmove", &[3]),
            "__memset_chk" => ("memset", &[3]),
            "__strcpy_chk" => ("strcpy", &[2]),
            "__stpcpy_chk" => ("stpcpy", &[2]),
            "__strncpy_chk" => ("strncpy", &[3]),
            "__strcat_chk" => ("strcat", &[2]),
            "__strncat_chk" => ("strncat", &[3]),
            "__readlink_chk" => ("readlink", &[3]),

            // glibc symbol aliases that exist on Linux but not Darwin.
            "__isoc23_strtoumax" => ("strtoumax", &[]),
            "__isoc23_strtoul" => ("strtoul", &[]),
            "__isoc23_strtol" => ("strtol", &[]),
            "__isoc23_strtoll" => ("strtoll", &[]),
            "__isoc23_strtoull" => ("strtoull", &[]),
            "__dcgettext" => ("dcgettext", &[]),
            "__dgettext" => ("dgettext", &[]),
            "__gettext" => ("gettext", &[]),
            _ => return None,
        })
    }

    fn chk_symbol_rewrite(name: &str) -> Option<&'static str> {
        Some(match name {
            "__fprintf_chk" => "fprintf",
            "__printf_chk" => "printf",
            "__sprintf_chk" => "sprintf",
            "__snprintf_chk" => "snprintf",
            "__vfprintf_chk" => "vfprintf",
            "__vsprintf_chk" => "vsprintf",
            "__vsnprintf_chk" => "vsnprintf",
            "__memcpy_chk" => "memcpy",
            "__mempcpy_chk" => "mempcpy",
            "__memmove_chk" => "memmove",
            "__memset_chk" => "memset",
            "__strcpy_chk" => "strcpy",
            "__stpcpy_chk" => "stpcpy",
            "__strncpy_chk" => "strncpy",
            "__strcat_chk" => "strcat",
            "__strncat_chk" => "strncat",
            "__readlink_chk" => "readlink",

            "__isoc23_strtoumax" => "strtoumax",
            "__isoc23_strtoul" => "strtoul",
            "__isoc23_strtol" => "strtol",
            "__isoc23_strtoll" => "strtoll",
            "__isoc23_strtoull" => "strtoull",
            "__dcgettext" => "dcgettext",
            "__dgettext" => "dgettext",
            "__gettext" => "gettext",
            _ => return None,
        })
    }

    fn rewrite_variadic_call(
        function: &mut AsmValue,
        args: &mut Vec<AsmValue>,
        new_name: &str,
        drop_indices: &[usize],
    ) {
        if drop_indices.is_empty() {
            *function = AsmValue::Function(new_name.to_string());
            return;
        }

        let mut next_args = Vec::with_capacity(args.len());
        for (index, arg) in args.iter().cloned().enumerate() {
            if drop_indices.contains(&index) {
                continue;
            }
            next_args.push(arg);
        }

        *function = AsmValue::Function(new_name.to_string());
        *args = next_args;
    }

    for func in &mut program.functions {
        if func.is_declaration {
            continue;
        }
        for block in &mut func.basic_blocks {
            for inst in &mut block.instructions {
                let AsmInstructionKind::Call { function, args, .. } = &mut inst.kind else {
                    continue;
                };

                let name = match function {
                    AsmValue::Function(name) => name.clone(),
                    _ => continue,
                };

                let candidates = [
                    name.as_str(),
                    name.strip_prefix('_').unwrap_or(name.as_str()),
                ];
                for candidate in candidates {
                    if let Some((new_name, drop_indices)) = chk_call_rewrite(candidate) {
                        rewrite_variadic_call(function, args, new_name, drop_indices);
                        break;
                    }
                }
            }
        }
    }

    for global in &mut program.globals {
        for reloc in &mut global.relocations {
            let symbol = reloc.symbol.as_str().to_string();
            let candidates = [
                symbol.as_str(),
                symbol.strip_prefix('_').unwrap_or(symbol.as_str()),
            ];
            for candidate in candidates {
                if let Some(new_name) = chk_symbol_rewrite(candidate) {
                    reloc.symbol = Name::new(new_name);
                    break;
                }
            }
        }
    }
}

pub(super) fn ensure_glibc_fpending(program: &mut AsmProgram) -> Result<()> {
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("__fpending"),
            signature: AsmFunctionSignature {
                params: vec![AsmType::Ptr(Box::new(AsmType::I8))],
                return_type: AsmType::I64,
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: Vec::new(),
                terminator: AsmTerminator::Return(Some(AsmValue::Constant(AsmConstant::UInt(
                    0,
                    AsmType::I64,
                )))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: Linkage::External,
            visibility: Visibility::Default,
            calling_convention: Some(CallingConvention::C),
            section: Some(".text".to_string()),
            is_declaration: false,
        },
    );
    Ok(())
}

pub(super) fn ensure_glibc_errno_location(program: &mut AsmProgram) -> Result<()> {
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("__errno_location"),
            signature: AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Ptr(Box::new(AsmType::I32)),
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("__error".to_string()),
                        args: Vec::new(),
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    ty: AsmType::Ptr(Box::new(AsmType::I32)),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                }],
                terminator: AsmTerminator::Return(Some(AsmValue::Register(0))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: Linkage::External,
            visibility: Visibility::Default,
            calling_convention: Some(CallingConvention::C),
            section: Some(".text".to_string()),
            is_declaration: false,
        },
    );
    Ok(())
}

pub(super) fn ensure_glibc_assert_fail(program: &mut AsmProgram) -> Result<()> {
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("__assert_fail"),
            signature: AsmFunctionSignature {
                params: vec![
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    AsmType::I32,
                    AsmType::Ptr(Box::new(AsmType::I8)),
                ],
                return_type: AsmType::Void,
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("abort".to_string()),
                        args: Vec::new(),
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    ty: AsmType::Void,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                }],
                terminator: AsmTerminator::Unreachable,
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: Linkage::External,
            visibility: Visibility::Default,
            calling_convention: Some(CallingConvention::C),
            section: Some(".text".to_string()),
            is_declaration: false,
        },
    );
    Ok(())
}

pub(super) fn match_freelibrary_sequence_to_unix_call(
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
    let (opcode, kind, ty) = lower_system_api_to_unix(op, convention);
    Ok(Some((
        AsmInstruction {
            id: select.id,
            opcode,
            kind,
            ty: ty,
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

pub(crate) fn rewrite_program_for_target(program: &mut AsmProgram) -> Result<()> {
    rewrite_program_to_sys_ops(program)?;
    lower_sys_ops_for_target(program)?;
    inject_linux_compat_runtime_for_darwin(program)?;
    Ok(())
}
