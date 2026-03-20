#![allow(dead_code)]

use fp_core::asmir::{
    AsmBlock, AsmConstant, AsmFunction, AsmFunctionSignature, AsmGenericOpcode, AsmGlobal,
    AsmGlobalRelocation, AsmInstruction, AsmInstructionKind, AsmLocal, AsmObjectFormat, AsmOpcode,
    AsmProgram, AsmRelocationKind, AsmSection, AsmSectionFlag, AsmSectionKind, AsmSysOp,
    AsmSyscallConvention, AsmTerminator, AsmType, AsmValue, PosixDirentStyle, PosixFlagStyle,
};
use fp_core::error::{Error, Result};
use fp_core::lir::{CallingConvention, Linkage, Name, Visibility};

type SystemApiOp = AsmSysOp;

fn match_getfileattributes_sequence_to_syscall(
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

fn ensure_glibc_progname_globals(program: &mut AsmProgram) {
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
                initializer: Some(AsmConstant::UInt(0, AsmType::I64)),
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

fn ensure_glibc_overflow(program: &mut AsmProgram) -> Result<()> {
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
                    type_hint: Some(AsmType::I32),
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

fn ensure_glibc_mempcpy(program: &mut AsmProgram) -> Result<()> {
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
                        type_hint: Some(AsmType::Ptr(Box::new(AsmType::I8))),
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
                        type_hint: Some(AsmType::Ptr(Box::new(AsmType::I8))),
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

fn ensure_glibc_start_main(program: &mut AsmProgram) -> Result<()> {
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
                        type_hint: Some(AsmType::I64),
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
                        type_hint: Some(AsmType::I64),
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
                        type_hint: Some(AsmType::Ptr(Box::new(AsmType::Ptr(Box::new(
                            AsmType::I8,
                        ))))),
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
                        type_hint: Some(AsmType::I32),
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
                        type_hint: Some(AsmType::Void),
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

pub fn rewrite_program_to_sys_ops(program: &mut AsmProgram) -> Result<()> {
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

fn rewrite_glibc_chk_calls_to_libc(program: &mut AsmProgram) {
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

fn ensure_glibc_fpending(program: &mut AsmProgram) -> Result<()> {
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

fn ensure_glibc_errno_location(program: &mut AsmProgram) -> Result<()> {
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
                    type_hint: Some(AsmType::Ptr(Box::new(AsmType::I32))),
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

fn ensure_glibc_assert_fail(program: &mut AsmProgram) -> Result<()> {
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
                    type_hint: Some(AsmType::Void),
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

fn detect_system_api_from_posix_call(
    kind: &AsmInstructionKind,
    dirent_style: PosixDirentStyle,
) -> Option<SystemApiOp> {
    let AsmInstructionKind::Call { function, args, .. } = kind else {
        return None;
    };
    let AsmValue::Function(symbol) = function else {
        return None;
    };
    let name = normalize_proc_name(symbol);
    match name.as_str() {
        "opendir" => Some(SystemApiOp::Opendir {
            path: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
        }),
        "readdir" | "readdir64" => Some(SystemApiOp::Readdir {
            dir: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
            dirent_style,
        }),
        "closedir" => Some(SystemApiOp::Closedir {
            dir: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
        }),
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
        "unlink" => Some(SystemApiOp::Unlink {
            path: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
        }),
        "mkdir" => Some(SystemApiOp::Mkdir {
            path: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
            mode: args
                .get(1)
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32))),
        }),
        "rmdir" => Some(SystemApiOp::Rmdir {
            path: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
        }),
        "rename" => Some(SystemApiOp::Rename {
            from: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
            to: args
                .get(1)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
        }),
        "access" => Some(SystemApiOp::Access {
            path: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
            mode: args
                .get(1)
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32))),
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
    rewrite_program_to_sys_ops(program)?;
    lower_sys_ops_for_target(program)?;
    inject_linux_compat_runtime_for_darwin(program)?;
    Ok(())
}

fn inject_linux_compat_runtime_for_darwin(program: &mut AsmProgram) -> Result<()> {
    if program.target.object_format != AsmObjectFormat::MachO {
        return Ok(());
    }
    let Some(container) = program.container.as_ref() else {
        return Ok(());
    };
    if container.format != AsmObjectFormat::Elf {
        return Ok(());
    }

    rewrite_glibc_chk_calls_to_libc(program);

    ensure_section(
        program,
        ".rodata",
        AsmSectionKind::ReadOnlyData,
        vec![AsmSectionFlag::Allocate],
    );
    ensure_section(
        program,
        ".data",
        AsmSectionKind::Data,
        vec![AsmSectionFlag::Allocate, AsmSectionFlag::Write],
    );

    ensure_ctype_tables(program);
    ensure_ctype_loc_functions(program)?;
    ensure_ctype_mb_cur_max(program)?;
    ensure_glibc_assert_fail(program)?;
    ensure_glibc_errno_location(program)?;
    ensure_glibc_fpending(program)?;
    ensure_glibc_start_main(program)?;
    ensure_glibc_mempcpy(program)?;
    ensure_glibc_overflow(program)?;
    ensure_glibc_progname_globals(program);
    ensure_glibc_gettext_stubs(program)?;
    ensure_linux_libcap_stubs(program)?;
    ensure_glibc_stdio_unlocked(program)?;
    ensure_linux_xattr_wrappers(program)?;
    ensure_glibc_mbrtoc32(program)?;
    ensure_glibc_rawmemchr(program)?;
    ensure_linux_statx_stub(program)?;

    Ok(())
}

fn ensure_glibc_rawmemchr(program: &mut AsmProgram) -> Result<()> {
    // rawmemchr(const void *s, int c) -> memchr(s, c, SIZE_MAX)
    let void_ptr = AsmType::Ptr(Box::new(AsmType::I8));

    ensure_function(
        program,
        AsmFunction {
            name: Name::new("rawmemchr"),
            signature: AsmFunctionSignature {
                params: vec![void_ptr.clone(), AsmType::I32],
                return_type: void_ptr.clone(),
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("memchr".to_string()),
                        args: vec![
                            AsmValue::Local(0),
                            AsmValue::Local(1),
                            AsmValue::Constant(AsmConstant::UInt(u64::MAX, AsmType::I64)),
                        ],
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    type_hint: Some(void_ptr.clone()),
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
                    name: Some("s".to_string()),
                    ty: void_ptr,
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("c".to_string()),
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

fn ensure_linux_statx_stub(program: &mut AsmProgram) -> Result<()> {
    // Linux `statx` is used by newer coreutils binaries.
    //
    // For now we intentionally force a fallback path by returning -1 and setting
    // errno=ENOSYS (38 on Linux). This keeps the function ABI-correct without
    // committing to a Linux `struct statx` layout translation on Darwin yet.
    //
    // int statx(int dirfd, const char *pathname, int flags, unsigned int mask, struct statx *buf);
    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
    let ptr_i32 = AsmType::Ptr(Box::new(AsmType::I32));

    ensure_function(
        program,
        AsmFunction {
            name: Name::new("statx"),
            signature: AsmFunctionSignature {
                params: vec![
                    AsmType::I32,
                    ptr_i8.clone(),
                    AsmType::I32,
                    AsmType::I32,
                    ptr_i8.clone(),
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
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("__errno_location".to_string()),
                            args: Vec::new(),
                            calling_convention: CallingConvention::C,
                            tail_call: false,
                        },
                        type_hint: Some(ptr_i32.clone()),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 1,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value: AsmValue::Constant(AsmConstant::UInt(38, AsmType::I32)),
                            address: AsmValue::Register(0),
                            alignment: Some(4),
                            volatile: false,
                        },
                        type_hint: Some(AsmType::Void),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                ],
                terminator: AsmTerminator::Return(Some(AsmValue::Constant(AsmConstant::Int(
                    -1,
                    AsmType::I32,
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

fn ensure_glibc_mbrtoc32(program: &mut AsmProgram) -> Result<()> {
    // A pragmatic ASCII-only implementation.
    // size_t mbrtoc32(char32_t *pc32, const char *s, size_t n, mbstate_t *ps)

    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
    let ptr_i32 = AsmType::Ptr(Box::new(AsmType::I32));

    ensure_function(
        program,
        AsmFunction {
            name: Name::new("mbrtoc32"),
            signature: AsmFunctionSignature {
                params: vec![
                    ptr_i32.clone(),
                    ptr_i8.clone(),
                    AsmType::I64,
                    ptr_i8.clone(),
                ],
                return_type: AsmType::I64,
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: vec![
                    AsmInstruction {
                        id: 0,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Load),
                        kind: AsmInstructionKind::Load {
                            address: AsmValue::Local(1),
                            alignment: Some(1),
                            volatile: false,
                        },
                        type_hint: Some(AsmType::I8),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 1,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::ZExt),
                        kind: AsmInstructionKind::ZExt(AsmValue::Register(0), AsmType::I32),
                        type_hint: Some(AsmType::I32),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 2,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value: AsmValue::Register(1),
                            address: AsmValue::Local(0),
                            alignment: Some(4),
                            volatile: false,
                        },
                        type_hint: Some(AsmType::Void),
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
                            AsmValue::Register(0),
                            AsmValue::Constant(AsmConstant::UInt(0, AsmType::I8)),
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
                        id: 4,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Select),
                        kind: AsmInstructionKind::Select {
                            condition: AsmValue::Register(3),
                            if_true: AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                            if_false: AsmValue::Constant(AsmConstant::UInt(1, AsmType::I64)),
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
                terminator: AsmTerminator::Return(Some(AsmValue::Register(4))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: vec![
                AsmLocal {
                    id: 0,
                    name: Some("pc32".to_string()),
                    ty: ptr_i32,
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("s".to_string()),
                    ty: ptr_i8.clone(),
                    is_argument: true,
                },
                AsmLocal {
                    id: 2,
                    name: Some("n".to_string()),
                    ty: AsmType::I64,
                    is_argument: true,
                },
                AsmLocal {
                    id: 3,
                    name: Some("ps".to_string()),
                    ty: ptr_i8,
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

fn ensure_linux_xattr_wrappers(program: &mut AsmProgram) -> Result<()> {
    // Linux/glibc exposes `l* xattr` entrypoints that are absent on Darwin.
    // Provide wrappers over Darwin's xattr APIs.

    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
    let void_ptr = AsmType::Ptr(Box::new(AsmType::I8));

    // ssize_t lgetxattr(const char *path, const char *name, void *value, size_t size)
    // -> getxattr(path, name, value, size, 0, 0)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("lgetxattr"),
            signature: AsmFunctionSignature {
                params: vec![
                    ptr_i8.clone(),
                    ptr_i8.clone(),
                    void_ptr.clone(),
                    AsmType::I64,
                ],
                return_type: AsmType::I64,
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("getxattr".to_string()),
                        args: vec![
                            AsmValue::Local(0),
                            AsmValue::Local(1),
                            AsmValue::Local(2),
                            AsmValue::Local(3),
                            AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)),
                            AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)),
                        ],
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::I64),
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
                    name: Some("path".to_string()),
                    ty: ptr_i8.clone(),
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("name".to_string()),
                    ty: ptr_i8.clone(),
                    is_argument: true,
                },
                AsmLocal {
                    id: 2,
                    name: Some("value".to_string()),
                    ty: void_ptr.clone(),
                    is_argument: true,
                },
                AsmLocal {
                    id: 3,
                    name: Some("size".to_string()),
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

    // ssize_t llistxattr(const char *path, char *list, size_t size)
    // -> listxattr(path, list, size, 0)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("llistxattr"),
            signature: AsmFunctionSignature {
                params: vec![ptr_i8.clone(), void_ptr.clone(), AsmType::I64],
                return_type: AsmType::I64,
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("listxattr".to_string()),
                        args: vec![
                            AsmValue::Local(0),
                            AsmValue::Local(1),
                            AsmValue::Local(2),
                            AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)),
                        ],
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::I64),
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
                    name: Some("path".to_string()),
                    ty: ptr_i8.clone(),
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("list".to_string()),
                    ty: void_ptr.clone(),
                    is_argument: true,
                },
                AsmLocal {
                    id: 2,
                    name: Some("size".to_string()),
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

    // int lsetxattr(const char *path, const char *name, const void *value, size_t size, int flags)
    // -> setxattr(path, name, value, size, 0, flags)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("lsetxattr"),
            signature: AsmFunctionSignature {
                params: vec![
                    ptr_i8.clone(),
                    ptr_i8,
                    void_ptr.clone(),
                    AsmType::I64,
                    AsmType::I32,
                ],
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
                        function: AsmValue::Function("setxattr".to_string()),
                        args: vec![
                            AsmValue::Local(0),
                            AsmValue::Local(1),
                            AsmValue::Local(2),
                            AsmValue::Local(3),
                            AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)),
                            AsmValue::Local(4),
                        ],
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::I32),
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
                    name: Some("path".to_string()),
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("name".to_string()),
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
                    is_argument: true,
                },
                AsmLocal {
                    id: 2,
                    name: Some("value".to_string()),
                    ty: void_ptr.clone(),
                    is_argument: true,
                },
                AsmLocal {
                    id: 3,
                    name: Some("size".to_string()),
                    ty: AsmType::I64,
                    is_argument: true,
                },
                AsmLocal {
                    id: 4,
                    name: Some("flags".to_string()),
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

    // int lremovexattr(const char *path, const char *name)
    // -> removexattr(path, name, 0)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("lremovexattr"),
            signature: AsmFunctionSignature {
                params: vec![
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                ],
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
                        function: AsmValue::Function("removexattr".to_string()),
                        args: vec![
                            AsmValue::Local(0),
                            AsmValue::Local(1),
                            AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)),
                        ],
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::I32),
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
                    name: Some("path".to_string()),
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("name".to_string()),
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

fn ensure_glibc_stdio_unlocked(program: &mut AsmProgram) -> Result<()> {
    // glibc provides *_unlocked stdio functions; Darwin libc typically doesn't.
    // Implement them as thin wrappers over their locked counterparts.

    let file_ptr = AsmType::Ptr(Box::new(AsmType::I8));
    let void_ptr = AsmType::Ptr(Box::new(AsmType::I8));

    // int fflush_unlocked(FILE *stream)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("fflush_unlocked"),
            signature: AsmFunctionSignature {
                params: vec![file_ptr.clone()],
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
                        function: AsmValue::Function("fflush".to_string()),
                        args: vec![AsmValue::Local(0)],
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::I32),
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
            locals: vec![AsmLocal {
                id: 0,
                name: Some("stream".to_string()),
                ty: file_ptr.clone(),
                is_argument: true,
            }],
            stack_slots: Vec::new(),
            frame: None,
            linkage: Linkage::External,
            visibility: Visibility::Default,
            calling_convention: Some(CallingConvention::C),
            section: Some(".text".to_string()),
            is_declaration: false,
        },
    );

    // size_t fwrite_unlocked(const void *ptr, size_t size, size_t nmemb, FILE *stream)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("fwrite_unlocked"),
            signature: AsmFunctionSignature {
                params: vec![
                    void_ptr.clone(),
                    AsmType::I64,
                    AsmType::I64,
                    file_ptr.clone(),
                ],
                return_type: AsmType::I64,
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("fwrite".to_string()),
                        args: vec![
                            AsmValue::Local(0),
                            AsmValue::Local(1),
                            AsmValue::Local(2),
                            AsmValue::Local(3),
                        ],
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::I64),
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
                    name: Some("ptr".to_string()),
                    ty: void_ptr.clone(),
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("size".to_string()),
                    ty: AsmType::I64,
                    is_argument: true,
                },
                AsmLocal {
                    id: 2,
                    name: Some("nmemb".to_string()),
                    ty: AsmType::I64,
                    is_argument: true,
                },
                AsmLocal {
                    id: 3,
                    name: Some("stream".to_string()),
                    ty: file_ptr.clone(),
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

    // size_t fread_unlocked(void *ptr, size_t size, size_t nmemb, FILE *stream)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("fread_unlocked"),
            signature: AsmFunctionSignature {
                params: vec![
                    void_ptr.clone(),
                    AsmType::I64,
                    AsmType::I64,
                    file_ptr.clone(),
                ],
                return_type: AsmType::I64,
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("fread".to_string()),
                        args: vec![
                            AsmValue::Local(0),
                            AsmValue::Local(1),
                            AsmValue::Local(2),
                            AsmValue::Local(3),
                        ],
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::I64),
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
                    name: Some("ptr".to_string()),
                    ty: void_ptr.clone(),
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("size".to_string()),
                    ty: AsmType::I64,
                    is_argument: true,
                },
                AsmLocal {
                    id: 2,
                    name: Some("nmemb".to_string()),
                    ty: AsmType::I64,
                    is_argument: true,
                },
                AsmLocal {
                    id: 3,
                    name: Some("stream".to_string()),
                    ty: file_ptr.clone(),
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

    // int fputc_unlocked(int c, FILE *stream)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("fputc_unlocked"),
            signature: AsmFunctionSignature {
                params: vec![AsmType::I32, file_ptr.clone()],
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
                        args: vec![AsmValue::Local(0), AsmValue::Local(1)],
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::I32),
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
                    name: Some("c".to_string()),
                    ty: AsmType::I32,
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("stream".to_string()),
                    ty: file_ptr.clone(),
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

    // int fputs_unlocked(const char *s, FILE *stream)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("fputs_unlocked"),
            signature: AsmFunctionSignature {
                params: vec![void_ptr.clone(), file_ptr.clone()],
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
                        function: AsmValue::Function("fputs".to_string()),
                        args: vec![AsmValue::Local(0), AsmValue::Local(1)],
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::I32),
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
                    name: Some("s".to_string()),
                    ty: void_ptr.clone(),
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("stream".to_string()),
                    ty: file_ptr.clone(),
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

    // int getc_unlocked(FILE *stream)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("getc_unlocked"),
            signature: AsmFunctionSignature {
                params: vec![file_ptr.clone()],
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
                        function: AsmValue::Function("getc".to_string()),
                        args: vec![AsmValue::Local(0)],
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::I32),
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
            locals: vec![AsmLocal {
                id: 0,
                name: Some("stream".to_string()),
                ty: file_ptr.clone(),
                is_argument: true,
            }],
            stack_slots: Vec::new(),
            frame: None,
            linkage: Linkage::External,
            visibility: Visibility::Default,
            calling_convention: Some(CallingConvention::C),
            section: Some(".text".to_string()),
            is_declaration: false,
        },
    );

    // int putc_unlocked(int c, FILE *stream)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("putc_unlocked"),
            signature: AsmFunctionSignature {
                params: vec![AsmType::I32, file_ptr.clone()],
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
                        function: AsmValue::Function("putc".to_string()),
                        args: vec![AsmValue::Local(0), AsmValue::Local(1)],
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::I32),
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
                    name: Some("c".to_string()),
                    ty: AsmType::I32,
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("stream".to_string()),
                    ty: file_ptr.clone(),
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

fn ensure_linux_libcap_stubs(program: &mut AsmProgram) -> Result<()> {
    // coreutils may be built with libcap support. Darwin doesn't ship libcap.
    // Provide no-op stubs so capability-aware paths degrade gracefully.

    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));

    ensure_global(
        program,
        AsmGlobal {
            name: Name::new("fp_linux_empty_cstring"),
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

    // int cap_free(void *ptr)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("cap_free"),
            signature: AsmFunctionSignature {
                params: vec![ptr_i8.clone()],
                return_type: AsmType::I32,
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: Vec::new(),
                terminator: AsmTerminator::Return(Some(AsmValue::Constant(AsmConstant::UInt(
                    0,
                    AsmType::I32,
                )))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: vec![AsmLocal {
                id: 0,
                name: Some("ptr".to_string()),
                ty: ptr_i8.clone(),
                is_argument: true,
            }],
            stack_slots: Vec::new(),
            frame: None,
            linkage: Linkage::External,
            visibility: Visibility::Default,
            calling_convention: Some(CallingConvention::C),
            section: Some(".text".to_string()),
            is_declaration: false,
        },
    );

    // void *cap_get_file(const char *path)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("cap_get_file"),
            signature: AsmFunctionSignature {
                params: vec![ptr_i8.clone()],
                return_type: ptr_i8.clone(),
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: Vec::new(),
                terminator: AsmTerminator::Return(Some(AsmValue::Null(ptr_i8.clone()))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: vec![AsmLocal {
                id: 0,
                name: Some("path".to_string()),
                ty: ptr_i8.clone(),
                is_argument: true,
            }],
            stack_slots: Vec::new(),
            frame: None,
            linkage: Linkage::External,
            visibility: Visibility::Default,
            calling_convention: Some(CallingConvention::C),
            section: Some(".text".to_string()),
            is_declaration: false,
        },
    );

    // int cap_set_file(const char *path, void *cap)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("cap_set_file"),
            signature: AsmFunctionSignature {
                params: vec![ptr_i8.clone(), ptr_i8.clone()],
                return_type: AsmType::I32,
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: Vec::new(),
                terminator: AsmTerminator::Return(Some(AsmValue::Constant(AsmConstant::UInt(
                    0,
                    AsmType::I32,
                )))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: vec![
                AsmLocal {
                    id: 0,
                    name: Some("path".to_string()),
                    ty: ptr_i8.clone(),
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("cap".to_string()),
                    ty: ptr_i8.clone(),
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

    // char *cap_to_text(void *cap, ssize_t *len)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("cap_to_text"),
            signature: AsmFunctionSignature {
                params: vec![ptr_i8.clone(), AsmType::Ptr(Box::new(AsmType::I64))],
                return_type: ptr_i8.clone(),
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: Vec::new(),
                terminator: AsmTerminator::Return(Some(AsmValue::Global(
                    "fp_linux_empty_cstring".to_string(),
                    ptr_i8.clone(),
                ))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: vec![
                AsmLocal {
                    id: 0,
                    name: Some("cap".to_string()),
                    ty: ptr_i8.clone(),
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("len".to_string()),
                    ty: AsmType::Ptr(Box::new(AsmType::I64)),
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

fn ensure_glibc_gettext_stubs(program: &mut AsmProgram) -> Result<()> {
    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));

    // const char *bindtextdomain(const char *domain, const char *dir)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("bindtextdomain"),
            signature: AsmFunctionSignature {
                params: vec![ptr_i8.clone(), ptr_i8.clone()],
                return_type: ptr_i8.clone(),
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: Vec::new(),
                terminator: AsmTerminator::Return(Some(AsmValue::Local(1))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: vec![
                AsmLocal {
                    id: 0,
                    name: Some("domain".to_string()),
                    ty: ptr_i8.clone(),
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("dir".to_string()),
                    ty: ptr_i8.clone(),
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

    // const char *textdomain(const char *domain)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("textdomain"),
            signature: AsmFunctionSignature {
                params: vec![ptr_i8.clone()],
                return_type: ptr_i8.clone(),
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: Vec::new(),
                terminator: AsmTerminator::Return(Some(AsmValue::Local(0))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: vec![AsmLocal {
                id: 0,
                name: Some("domain".to_string()),
                ty: ptr_i8.clone(),
                is_argument: true,
            }],
            stack_slots: Vec::new(),
            frame: None,
            linkage: Linkage::External,
            visibility: Visibility::Default,
            calling_convention: Some(CallingConvention::C),
            section: Some(".text".to_string()),
            is_declaration: false,
        },
    );

    // const char *dcgettext(const char *domain, const char *msgid, int category)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("dcgettext"),
            signature: AsmFunctionSignature {
                params: vec![ptr_i8.clone(), ptr_i8.clone(), AsmType::I32],
                return_type: ptr_i8.clone(),
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: Vec::new(),
                terminator: AsmTerminator::Return(Some(AsmValue::Local(1))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: vec![
                AsmLocal {
                    id: 0,
                    name: Some("domain".to_string()),
                    ty: ptr_i8.clone(),
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("msgid".to_string()),
                    ty: ptr_i8.clone(),
                    is_argument: true,
                },
                AsmLocal {
                    id: 2,
                    name: Some("category".to_string()),
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

    // const char *dgettext(const char *domain, const char *msgid)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("dgettext"),
            signature: AsmFunctionSignature {
                params: vec![ptr_i8.clone(), ptr_i8.clone()],
                return_type: ptr_i8.clone(),
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: Vec::new(),
                terminator: AsmTerminator::Return(Some(AsmValue::Local(1))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: vec![
                AsmLocal {
                    id: 0,
                    name: Some("domain".to_string()),
                    ty: ptr_i8.clone(),
                    is_argument: true,
                },
                AsmLocal {
                    id: 1,
                    name: Some("msgid".to_string()),
                    ty: ptr_i8.clone(),
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

    // const char *gettext(const char *msgid)
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("gettext"),
            signature: AsmFunctionSignature {
                params: vec![ptr_i8.clone()],
                return_type: ptr_i8,
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: Vec::new(),
                terminator: AsmTerminator::Return(Some(AsmValue::Local(0))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: vec![AsmLocal {
                id: 0,
                name: Some("msgid".to_string()),
                ty: AsmType::Ptr(Box::new(AsmType::I8)),
                is_argument: true,
            }],
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

fn ensure_section(
    program: &mut AsmProgram,
    name: &str,
    kind: AsmSectionKind,
    flags: Vec<AsmSectionFlag>,
) {
    if program.sections.iter().any(|section| section.name == name) {
        return;
    }
    program.sections.push(AsmSection {
        name: name.to_string(),
        kind,
        flags,
        alignment: Some(16),
    });
}

fn ensure_global(program: &mut AsmProgram, global: AsmGlobal) {
    if let Some(existing) = program
        .globals
        .iter_mut()
        .find(|item| item.name.as_str() == global.name.as_str())
    {
        *existing = global;
        return;
    }
    program.globals.push(global);
}

fn ensure_function(program: &mut AsmProgram, function: AsmFunction) {
    if let Some(existing) = program
        .functions
        .iter_mut()
        .find(|item| item.name.as_str() == function.name.as_str())
    {
        if existing.is_declaration {
            *existing = function;
        }
        return;
    }
    program.functions.push(function);
}

fn build_ascii_tolower_table_bytes() -> Vec<u8> {
    let mut out = Vec::with_capacity(256 * 4);
    for byte in 0u8..=255 {
        let lowered = if (b'A'..=b'Z').contains(&byte) {
            byte + 32
        } else {
            byte
        };
        out.extend_from_slice(&(lowered as i32).to_le_bytes());
    }
    out
}

fn build_ascii_toupper_table_bytes() -> Vec<u8> {
    let mut out = Vec::with_capacity(256 * 4);
    for byte in 0u8..=255 {
        let upper = if (b'a'..=b'z').contains(&byte) {
            byte - 32
        } else {
            byte
        };
        out.extend_from_slice(&(upper as i32).to_le_bytes());
    }
    out
}

fn ensure_ctype_tables(program: &mut AsmProgram) {
    ensure_global(
        program,
        AsmGlobal {
            name: Name::new("fp_linux_ctype_tolower_table"),
            ty: AsmType::Array(Box::new(AsmType::I8), 256 * 4),
            initializer: Some(AsmConstant::Bytes(build_ascii_tolower_table_bytes())),
            relocations: Vec::new(),
            section: Some(".rodata".to_string()),
            linkage: Linkage::Private,
            visibility: Visibility::Default,
            alignment: Some(16),
            is_constant: true,
        },
    );
    ensure_global(
        program,
        AsmGlobal {
            name: Name::new("fp_linux_ctype_tolower_ptr"),
            ty: AsmType::I64,
            initializer: Some(AsmConstant::UInt(0, AsmType::I64)),
            relocations: vec![AsmGlobalRelocation {
                offset: 0,
                kind: AsmRelocationKind::Abs64,
                symbol: Name::new("fp_linux_ctype_tolower_table"),
                addend: 0,
            }],
            section: Some(".data".to_string()),
            linkage: Linkage::Private,
            visibility: Visibility::Default,
            alignment: Some(8),
            is_constant: false,
        },
    );

    ensure_global(
        program,
        AsmGlobal {
            name: Name::new("fp_linux_ctype_toupper_table"),
            ty: AsmType::Array(Box::new(AsmType::I8), 256 * 4),
            initializer: Some(AsmConstant::Bytes(build_ascii_toupper_table_bytes())),
            relocations: Vec::new(),
            section: Some(".rodata".to_string()),
            linkage: Linkage::Private,
            visibility: Visibility::Default,
            alignment: Some(16),
            is_constant: true,
        },
    );
    ensure_global(
        program,
        AsmGlobal {
            name: Name::new("fp_linux_ctype_toupper_ptr"),
            ty: AsmType::I64,
            initializer: Some(AsmConstant::UInt(0, AsmType::I64)),
            relocations: vec![AsmGlobalRelocation {
                offset: 0,
                kind: AsmRelocationKind::Abs64,
                symbol: Name::new("fp_linux_ctype_toupper_table"),
                addend: 0,
            }],
            section: Some(".data".to_string()),
            linkage: Linkage::Private,
            visibility: Visibility::Default,
            alignment: Some(8),
            is_constant: false,
        },
    );

    ensure_global(
        program,
        AsmGlobal {
            name: Name::new("fp_linux_ctype_b_table"),
            ty: AsmType::Array(Box::new(AsmType::I8), 256 * 2),
            initializer: Some(AsmConstant::Bytes(vec![0xffu8; 256 * 2])),
            relocations: Vec::new(),
            section: Some(".rodata".to_string()),
            linkage: Linkage::Private,
            visibility: Visibility::Default,
            alignment: Some(16),
            is_constant: true,
        },
    );
    ensure_global(
        program,
        AsmGlobal {
            name: Name::new("fp_linux_ctype_b_ptr"),
            ty: AsmType::I64,
            initializer: Some(AsmConstant::UInt(0, AsmType::I64)),
            relocations: vec![AsmGlobalRelocation {
                offset: 0,
                kind: AsmRelocationKind::Abs64,
                symbol: Name::new("fp_linux_ctype_b_table"),
                addend: 0,
            }],
            section: Some(".data".to_string()),
            linkage: Linkage::Private,
            visibility: Visibility::Default,
            alignment: Some(8),
            is_constant: false,
        },
    );
}

fn ensure_ctype_loc_functions(program: &mut AsmProgram) -> Result<()> {
    let ptr_return = AsmType::Ptr(Box::new(AsmType::Ptr(Box::new(AsmType::I8))));

    ensure_function(
        program,
        AsmFunction {
            name: Name::new("__ctype_tolower_loc"),
            signature: AsmFunctionSignature {
                params: Vec::new(),
                return_type: ptr_return.clone(),
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: Vec::new(),
                terminator: AsmTerminator::Return(Some(AsmValue::Constant(
                    AsmConstant::GlobalRef(
                        Name::new("fp_linux_ctype_tolower_ptr"),
                        AsmType::Ptr(Box::new(AsmType::I8)),
                        vec![0],
                    ),
                ))),
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

    ensure_function(
        program,
        AsmFunction {
            name: Name::new("__ctype_toupper_loc"),
            signature: AsmFunctionSignature {
                params: Vec::new(),
                return_type: ptr_return.clone(),
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: Vec::new(),
                terminator: AsmTerminator::Return(Some(AsmValue::Constant(
                    AsmConstant::GlobalRef(
                        Name::new("fp_linux_ctype_toupper_ptr"),
                        AsmType::Ptr(Box::new(AsmType::I8)),
                        vec![0],
                    ),
                ))),
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

    ensure_function(
        program,
        AsmFunction {
            name: Name::new("__ctype_b_loc"),
            signature: AsmFunctionSignature {
                params: Vec::new(),
                return_type: ptr_return,
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: Vec::new(),
                terminator: AsmTerminator::Return(Some(AsmValue::Constant(
                    AsmConstant::GlobalRef(
                        Name::new("fp_linux_ctype_b_ptr"),
                        AsmType::Ptr(Box::new(AsmType::I8)),
                        vec![0],
                    ),
                ))),
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

fn ensure_ctype_mb_cur_max(program: &mut AsmProgram) -> Result<()> {
    ensure_function(
        program,
        AsmFunction {
            name: Name::new("__ctype_get_mb_cur_max"),
            signature: AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::I64,
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: None,
                instructions: Vec::new(),
                terminator: AsmTerminator::Return(Some(AsmValue::Constant(AsmConstant::UInt(
                    1,
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

pub fn lower_sys_ops_for_target(program: &mut AsmProgram) -> Result<()> {
    if program.target.object_format == AsmObjectFormat::Coff
        || program.target.object_format == AsmObjectFormat::Pe
    {
        lower_sys_ops_to_windows_imports(program)
    } else {
        lower_sys_ops_to_unix_syscalls(program)
    }
}

fn lower_sys_ops_to_unix_syscalls(program: &mut AsmProgram) -> Result<()> {
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
                        inst.type_hint = Some(AsmType::Ptr(Box::new(AsmType::I8)));
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
                        inst.type_hint = Some(AsmType::Ptr(Box::new(AsmType::I8)));
                    }
                    AsmSysOp::Closedir { dir } => {
                        inst.kind = AsmInstructionKind::Call {
                            function: AsmValue::Function("closedir".to_string()),
                            args: vec![dir.clone()],
                            calling_convention: default_cc.clone(),
                            tail_call: false,
                        };
                        inst.opcode = AsmOpcode::Generic(AsmGenericOpcode::Call);
                        inst.type_hint = Some(AsmType::I64);
                    }
                    _ => {
                        inst.kind = lower_system_api_to_syscall(op.clone(), target_convention);
                        inst.opcode = AsmOpcode::Generic(AsmGenericOpcode::Syscall);
                        inst.type_hint = Some(AsmType::I64);
                    }
                }
            }
        }
    }
    Ok(())
}

fn inject_linux_readdir_shim(program: &mut AsmProgram, cc: CallingConvention) -> Result<()> {
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
            type_hint: Some(ret),
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
            type_hint: Some(ty),
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
            type_hint: Some(ty),
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
            type_hint: Some(AsmType::Void),
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
            type_hint: Some(AsmType::I1),
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

fn lower_sys_ops_to_windows_imports(program: &mut AsmProgram) -> Result<()> {
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

fn target_syscall_convention(program: &AsmProgram) -> Option<AsmSyscallConvention> {
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

fn rewrite_syscalls_to_target_unix_convention(program: &mut AsmProgram) -> Result<()> {
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
                inst.type_hint = Some(AsmType::I64);
            }
        }
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
    let is_win32_dll =
        dll.eq_ignore_ascii_case("kernel32.dll") || dll.eq_ignore_ascii_case("kernelbase.dll");
    let is_ntdll = dll.eq_ignore_ascii_case("ntdll.dll");

    match proc_name.as_str() {
        "ExitProcess" => {
            if !is_win32_dll {
                return None;
            }
            let code = args
                .first()
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)));
            Some(SystemApiOp::Exit { code })
        }
        "RtlExitUserProcess" => {
            if !is_ntdll {
                return None;
            }
            let code = args
                .first()
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)));
            Some(SystemApiOp::Exit { code })
        }
        "GetCurrentProcessId" => {
            if !is_win32_dll {
                return None;
            }
            Some(SystemApiOp::GetPid)
        }
        "GetCurrentThreadId"
            if matches!(
                convention,
                AsmSyscallConvention::LinuxX86_64 | AsmSyscallConvention::LinuxAarch64
            ) =>
        {
            if !is_win32_dll {
                return None;
            }
            Some(SystemApiOp::GetTid)
        }
        "LoadLibraryA" => {
            if !is_win32_dll {
                return None;
            }
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
            if !is_win32_dll {
                return None;
            }
            if args.len() < 2 {
                return None;
            }
            Some(SystemApiOp::Dlsym {
                handle: args[0].clone(),
                symbol: args[1].clone(),
            })
        }
        "FreeLibrary" => {
            if !is_win32_dll {
                return None;
            }
            if args.len() != 1 {
                return None;
            }
            Some(SystemApiOp::Dlclose {
                handle: args[0].clone(),
            })
        }
        "DeleteFileA" => {
            if !is_win32_dll {
                return None;
            }
            let path = args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))));
            Some(SystemApiOp::Unlink { path })
        }
        "CreateDirectoryA" => {
            if !is_win32_dll {
                return None;
            }
            if args.len() < 1 {
                return None;
            }
            Some(SystemApiOp::Mkdir {
                path: args[0].clone(),
                mode: AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)),
            })
        }
        "RemoveDirectoryA" => {
            if !is_win32_dll {
                return None;
            }
            if args.len() < 1 {
                return None;
            }
            Some(SystemApiOp::Rmdir {
                path: args[0].clone(),
            })
        }
        "MoveFileExA" => {
            if !is_win32_dll {
                return None;
            }
            if args.len() < 2 {
                return None;
            }
            Some(SystemApiOp::Rename {
                from: args[0].clone(),
                to: args[1].clone(),
            })
        }
        "GetFileAttributesA" => {
            if !is_win32_dll {
                return None;
            }
            let path = args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))));
            Some(SystemApiOp::Access {
                path,
                mode: AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)),
            })
        }
        "CreateFileA" => {
            if !is_win32_dll {
                return None;
            }
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
            if !is_win32_dll {
                return None;
            }
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
            if !is_win32_dll {
                return None;
            }
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
            if !is_win32_dll {
                return None;
            }
            if args.len() != 1 {
                return None;
            }
            Some(SystemApiOp::Close {
                fd: args[0].clone(),
            })
        }
        "SetFilePointerEx" => {
            if !is_win32_dll {
                return None;
            }
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
            if !is_win32_dll {
                return None;
            }
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
            if !is_win32_dll {
                return None;
            }
            if args.len() != 3 {
                return None;
            }
            Some(SystemApiOp::Munmap {
                addr: args[0].clone(),
                len: args[1].clone(),
            })
        }
        "NtClose" | "ZwClose" => {
            if !is_ntdll || args.len() != 1 {
                return None;
            }
            Some(SystemApiOp::Close {
                fd: args[0].clone(),
            })
        }
        "NtWriteFile" | "ZwWriteFile" => {
            if !is_ntdll || args.len() < 7 {
                return None;
            }
            Some(SystemApiOp::Write {
                fd: args[0].clone(),
                buffer: args[5].clone(),
                len: args[6].clone(),
            })
        }
        "NtReadFile" | "ZwReadFile" => {
            if !is_ntdll || args.len() < 7 {
                return None;
            }
            Some(SystemApiOp::Read {
                fd: args[0].clone(),
                buffer: args[5].clone(),
                len: args[6].clone(),
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
        AsmSyscallConvention::LinuxX86_64 if num == 87 => Some(SystemApiOp::Unlink {
            path: args.get(0)?.clone(),
        }),
        AsmSyscallConvention::LinuxX86_64 if num == 263 => {
            // unlinkat(dirfd, path, flags)
            let dirfd = args.get(0)?.clone();
            let dirfd = resolve_i64(&dirfd, instructions).ok().flatten()?;
            if dirfd != -100 {
                return None;
            }
            let flags = args.get(2)?.clone();
            let flags = resolve_i64(&flags, instructions).ok().flatten()?;
            // AT_REMOVEDIR=0x200
            if (flags & 0x200) != 0 {
                return Some(SystemApiOp::Rmdir {
                    path: args.get(1)?.clone(),
                });
            }
            Some(SystemApiOp::Unlink {
                path: args.get(1)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxAarch64 if num == 35 => {
            // unlinkat(dirfd, path, flags)
            let dirfd = args.get(0)?.clone();
            let dirfd = resolve_i64(&dirfd, instructions).ok().flatten()?;
            if dirfd != -100 {
                return None;
            }
            let flags = args.get(2)?.clone();
            let flags = resolve_i64(&flags, instructions).ok().flatten()?;
            if (flags & 0x200) != 0 {
                return Some(SystemApiOp::Rmdir {
                    path: args.get(1)?.clone(),
                });
            }
            Some(SystemApiOp::Unlink {
                path: args.get(1)?.clone(),
            })
        }
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_000a =>
        {
            Some(SystemApiOp::Unlink {
                path: args.get(0)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 83 => Some(SystemApiOp::Mkdir {
            path: args.get(0)?.clone(),
            mode: args.get(1)?.clone(),
        }),
        AsmSyscallConvention::LinuxX86_64 if num == 258 => {
            // mkdirat(dirfd, path, mode)
            let dirfd = args.get(0)?.clone();
            let dirfd = resolve_i64(&dirfd, instructions).ok().flatten()?;
            if dirfd != -100 {
                return None;
            }
            Some(SystemApiOp::Mkdir {
                path: args.get(1)?.clone(),
                mode: args.get(2)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxAarch64 if num == 34 => {
            // mkdirat(dirfd, path, mode)
            let dirfd = args.get(0)?.clone();
            let dirfd = resolve_i64(&dirfd, instructions).ok().flatten()?;
            if dirfd != -100 {
                return None;
            }
            Some(SystemApiOp::Mkdir {
                path: args.get(1)?.clone(),
                mode: args.get(2)?.clone(),
            })
        }
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0088 =>
        {
            Some(SystemApiOp::Mkdir {
                path: args.get(0)?.clone(),
                mode: args.get(1)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 84 => Some(SystemApiOp::Rmdir {
            path: args.get(0)?.clone(),
        }),
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0089 =>
        {
            Some(SystemApiOp::Rmdir {
                path: args.get(0)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 82 => Some(SystemApiOp::Rename {
            from: args.get(0)?.clone(),
            to: args.get(1)?.clone(),
        }),
        AsmSyscallConvention::LinuxX86_64 if num == 264 => {
            // renameat(olddirfd, oldpath, newdirfd, newpath)
            let olddirfd = resolve_i64(args.get(0)?, instructions).ok().flatten()?;
            let newdirfd = resolve_i64(args.get(2)?, instructions).ok().flatten()?;
            if olddirfd != -100 || newdirfd != -100 {
                return None;
            }
            Some(SystemApiOp::Rename {
                from: args.get(1)?.clone(),
                to: args.get(3)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxAarch64 if num == 38 => {
            let olddirfd = resolve_i64(args.get(0)?, instructions).ok().flatten()?;
            let newdirfd = resolve_i64(args.get(2)?, instructions).ok().flatten()?;
            if olddirfd != -100 || newdirfd != -100 {
                return None;
            }
            Some(SystemApiOp::Rename {
                from: args.get(1)?.clone(),
                to: args.get(3)?.clone(),
            })
        }
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0080 =>
        {
            Some(SystemApiOp::Rename {
                from: args.get(0)?.clone(),
                to: args.get(1)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 21 => Some(SystemApiOp::Access {
            path: args.get(0)?.clone(),
            mode: args.get(1)?.clone(),
        }),
        AsmSyscallConvention::LinuxX86_64 if num == 269 => {
            // faccessat(dirfd, path, mode, flags)
            let dirfd = resolve_i64(args.get(0)?, instructions).ok().flatten()?;
            if dirfd != -100 {
                return None;
            }
            Some(SystemApiOp::Access {
                path: args.get(1)?.clone(),
                mode: args.get(2)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxAarch64 if num == 48 => {
            // faccessat(dirfd, path, mode, flags)
            let dirfd = resolve_i64(args.get(0)?, instructions).ok().flatten()?;
            if dirfd != -100 {
                return None;
            }
            Some(SystemApiOp::Access {
                path: args.get(1)?.clone(),
                mode: args.get(2)?.clone(),
            })
        }
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0021 =>
        {
            Some(SystemApiOp::Access {
                path: args.get(0)?.clone(),
                mode: args.get(1)?.clone(),
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
            // ExitProcess is `noreturn` at the OS ABI level, but our AsmIR currently
            // models call results as SSA values that may be referenced by later
            // instructions (e.g. through generic lowering patterns). Keep this typed
            // as an integer to avoid codegen attempting to materialize a `Void` value.
            type_hint: Some(AsmType::I64),
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
                    AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)),
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

            Ok(LoweredWindows::Sequence(vec![call, cmp, select]))
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

        SystemApiOp::Opendir { .. }
        | SystemApiOp::Readdir { .. }
        | SystemApiOp::Closedir { .. } => Err(Error::from(
            "directory SysOps are not supported for Windows targets yet",
        )),
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

fn match_kernel32_bool_call_sequence_to_syscall(
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

fn match_deletefile_sequence_to_syscall(
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

fn match_createdirectory_sequence_to_syscall(
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

fn match_removedirectory_sequence_to_syscall(
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

fn match_movefileex_sequence_to_syscall(
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
    import_dll_matches(&sym_dll, dll) && sym_name == name
}

fn import_dll_matches(actual: &str, expected: &str) -> bool {
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
        SystemApiOp::Opendir { .. }
        | SystemApiOp::Readdir { .. }
        | SystemApiOp::Closedir { .. } => {
            unreachable!("directory SysOps must not be lowered via syscalls")
        }
        SystemApiOp::Unlink { path } => {
            let (number, args) = match convention {
                AsmSyscallConvention::LinuxX86_64 => (87, vec![path]),
                AsmSyscallConvention::LinuxAarch64 => (
                    35,
                    vec![
                        AsmValue::Constant(AsmConstant::Int(-100, AsmType::I64)),
                        path,
                        AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                    ],
                ),
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    (0x2000_000a, vec![path])
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args,
            }
        }
        SystemApiOp::Mkdir { path, mode } => {
            let (number, args) = match convention {
                AsmSyscallConvention::LinuxX86_64 => (83, vec![path, mode]),
                AsmSyscallConvention::LinuxAarch64 => (
                    34,
                    vec![
                        AsmValue::Constant(AsmConstant::Int(-100, AsmType::I64)),
                        path,
                        mode,
                    ],
                ),
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    (0x2000_0088, vec![path, mode])
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args,
            }
        }
        SystemApiOp::Rmdir { path } => {
            let (number, args) = match convention {
                AsmSyscallConvention::LinuxX86_64 => (84, vec![path]),
                AsmSyscallConvention::LinuxAarch64 => (
                    35,
                    vec![
                        AsmValue::Constant(AsmConstant::Int(-100, AsmType::I64)),
                        path,
                        AsmValue::Constant(AsmConstant::Int(0x200, AsmType::I64)),
                    ],
                ),
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    (0x2000_0089, vec![path])
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args,
            }
        }
        SystemApiOp::Rename { from, to } => {
            let (number, args) = match convention {
                AsmSyscallConvention::LinuxX86_64 => (82, vec![from, to]),
                AsmSyscallConvention::LinuxAarch64 => (
                    38,
                    vec![
                        AsmValue::Constant(AsmConstant::Int(-100, AsmType::I64)),
                        from,
                        AsmValue::Constant(AsmConstant::Int(-100, AsmType::I64)),
                        to,
                    ],
                ),
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    (0x2000_0080, vec![from, to])
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args,
            }
        }
        SystemApiOp::Access { path, mode } => {
            let (number, args) = match convention {
                AsmSyscallConvention::LinuxX86_64 => (21, vec![path, mode]),
                AsmSyscallConvention::LinuxAarch64 => (
                    48,
                    vec![
                        AsmValue::Constant(AsmConstant::Int(-100, AsmType::I64)),
                        path,
                        mode,
                        AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                    ],
                ),
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    (0x2000_0021, vec![path, mode])
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args,
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
    use fp_core::container::{
        ContainerArchitecture, ContainerEndianness, ContainerFile, ContainerKind,
    };

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
    fn rewrite_linux_readdir_call_to_darwin_shim() {
        let mut prog = program(AsmObjectFormat::MachO);
        prog.container = Some(ContainerFile::new(
            ContainerKind::Object,
            AsmObjectFormat::Elf,
            ContainerArchitecture::X86_64,
            ContainerEndianness::Little,
        ));

        let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
        prog.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("main"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: Vec::new(),
                return_type: ptr_i8.clone(),
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: vec![
                    AsmInstruction {
                        id: 0,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("opendir".to_string()),
                            args: vec![AsmValue::Null(ptr_i8.clone())],
                            calling_convention: CallingConvention::C,
                            tail_call: false,
                        },
                        type_hint: Some(ptr_i8.clone()),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 1,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("readdir".to_string()),
                            args: vec![AsmValue::Register(0)],
                            calling_convention: CallingConvention::C,
                            tail_call: false,
                        },
                        type_hint: Some(ptr_i8.clone()),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                ],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(1))),
                terminator_encoding: None,
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
        assert!(
            prog.functions
                .iter()
                .any(|f| f.name.as_str() == "fp_linux_readdir"),
            "expected fp_linux_readdir shim to be injected"
        );

        let block = &prog
            .functions
            .iter()
            .find(|f| f.name.as_str() == "main")
            .unwrap()
            .basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| {
            matches!(
                &inst.kind,
                AsmInstructionKind::Call { function: AsmValue::Function(name), .. }
                    if name == "fp_linux_readdir"
            )
        }));
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
                terminator_encoding: None,
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
                terminator_encoding: None,
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
    fn rewrite_windows_kernelbase_writefile_sequence_back_to_linux_syscall() {
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
                            function: AsmValue::Function("kernelbase!GetStdHandle".to_string()),
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
                    AsmInstruction {
                        id: 3,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("kernelbase!WriteFile".to_string()),
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
                terminator_encoding: None,
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
    fn rewrite_ntdll_writefile_import_to_linux_syscall() {
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
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("ntdll!NtWriteFile".to_string()),
                        args: vec![
                            AsmValue::Constant(AsmConstant::UInt(1, AsmType::I64)),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
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
                }],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                terminator_encoding: None,
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
    fn rewrite_ntdll_close_import_to_linux_syscall() {
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
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("ntdll!ZwClose".to_string()),
                        args: vec![AsmValue::Constant(AsmConstant::UInt(3, AsmType::I64))],
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
                }],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                terminator_encoding: None,
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

    #[test]
    fn rewrite_kernelbase_createfile_import_to_linux_open_syscall() {
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
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("kernelbase!CreateFileA".to_string()),
                        args: vec![
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Constant(AsmConstant::Int(
                                0x8000_0000u32 as i64,
                                AsmType::I64,
                            )),
                            AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Constant(AsmConstant::Int(3, AsmType::I64)),
                            AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
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
                }],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                terminator_encoding: None,
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
                    number: AsmValue::Constant(AsmConstant::UInt(2, _)),
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
                terminator_encoding: None,
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
                terminator_encoding: None,
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
                terminator_encoding: None,
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
                terminator_encoding: None,
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
