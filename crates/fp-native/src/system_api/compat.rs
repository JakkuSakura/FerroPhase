use super::*;

pub(super) fn inject_linux_compat_runtime_for_darwin(program: &mut AsmProgram) -> Result<()> {
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

pub(super) fn ensure_glibc_rawmemchr(program: &mut AsmProgram) -> Result<()> {
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
                    ty: void_ptr.clone(),
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

pub(super) fn ensure_linux_statx_stub(program: &mut AsmProgram) -> Result<()> {
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
                        ty: ptr_i32.clone(),
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
                        ty: AsmType::Void,
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

pub(super) fn ensure_glibc_mbrtoc32(program: &mut AsmProgram) -> Result<()> {
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
                        ty: AsmType::I8,
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
                        ty: AsmType::I32,
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
                        ty: AsmType::Void,
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
                        ty: AsmType::I1,
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
                        ty: AsmType::I64,
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

pub(super) fn ensure_linux_xattr_wrappers(program: &mut AsmProgram) -> Result<()> {
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
                    ty: AsmType::I64,
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
                    ty: AsmType::I64,
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

pub(super) fn ensure_glibc_stdio_unlocked(program: &mut AsmProgram) -> Result<()> {
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
                    ty: AsmType::I64,
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
                    ty: AsmType::I64,
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

pub(super) fn ensure_linux_libcap_stubs(program: &mut AsmProgram) -> Result<()> {
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

pub(super) fn ensure_glibc_gettext_stubs(program: &mut AsmProgram) -> Result<()> {
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
