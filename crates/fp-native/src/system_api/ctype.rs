use super::*;

pub(super) fn build_ascii_tolower_table_bytes() -> Vec<u8> {
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

pub(super) fn build_ascii_toupper_table_bytes() -> Vec<u8> {
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

pub(super) fn ensure_ctype_tables(program: &mut AsmProgram) {
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
            initializer: Some(AsmConstant::Bytes(vec![0; 8])),
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
            initializer: Some(AsmConstant::Bytes(vec![0; 8])),
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
            initializer: Some(AsmConstant::Bytes(vec![0; 8])),
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

pub(super) fn ensure_ctype_loc_functions(program: &mut AsmProgram) -> Result<()> {
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

pub(super) fn ensure_ctype_mb_cur_max(program: &mut AsmProgram) -> Result<()> {
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
