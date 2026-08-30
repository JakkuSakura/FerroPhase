use super::*;
use fp_core::lir::{
    CallingConvention, LirBasicBlock, LirBlob, LirConstant, LirFunction, LirFunctionSignature,
    LirGlobal, LirInstruction, LirInstructionKind, LirInteger, LirRegister, LirTerminator, LirType,
    LirValue, Name,
};

fn make(f: LirFunction) -> LirBlob {
    LirBlob {
        data_layout: LirDataLayout::new(
            64,
            8,
            vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
        )
        .expect("valid test data layout"),
        functions: vec![f],
        globals: vec![],
        type_definitions: vec![],
        queries: vec![],
    }
}

fn make_with_globals(f: LirFunction, globals: Vec<LirGlobal>) -> LirBlob {
    let mut program = make(f);
    program.globals = globals;
    program
}

fn make_with_functions_and_globals(
    functions: Vec<LirFunction>,
    globals: Vec<LirGlobal>,
) -> LirBlob {
    let mut program = make(functions.first().cloned().expect("entry function"));
    program.functions = functions;
    program.globals = globals;
    program
}

fn int(v: i64) -> LirValue {
    LirValue::constant(
        LirConstant::integer(LirType::I64, LirInteger::I64(v as u64)).expect("valid i64 constant"),
    )
}

#[test]
fn materializes_constant_gep_as_an_address() {
    let mut interpreter = LirInterpreter::new();
    interpreter.global_values.insert("bytes".into(), 17);
    let address = LirValue::constant(LirConstant::get_element_ptr(
        LirType::Ptr(Box::new(LirType::I8)),
        LirConstant::global_address(LirType::Ptr(Box::new(LirType::I8)), Name::new("bytes")),
        Vec::new(),
        true,
    ));

    assert_eq!(
        interpreter
            .constant_to_value(&address)
            .expect("resolve GEP"),
        Value::Pointer(fp_core::ast::ValuePointer::managed(17))
    );
}

fn reg(id: u32) -> LirValue {
    LirValue::register(id, LirType::I64)
}

fn ins(k: LirInstructionKind) -> LirInstruction {
    LirInstruction {
        id: 0,
        kind: k,
        result: Some(LirRegister {
            id: 0,
            ty: LirType::I64,
        }),
        debug_info: None,
    }
}

fn bb(id: u32, instrs: Vec<LirInstruction>, term: LirTerminator) -> LirBasicBlock {
    LirBasicBlock {
        id,
        label: None,
        instructions: instrs,
        terminator: term,
        predecessors: vec![],
        successors: vec![],
    }
}

fn ret(v: LirValue) -> LirTerminator {
    LirTerminator::Return(Some(v))
}

fn sig(p: &[LirType], r: LirType) -> LirFunctionSignature {
    LirFunctionSignature {
        params: p.to_vec(),
        return_type: r,
        is_variadic: false,
    }
}

fn i(id: u32, k: LirInstructionKind) -> LirInstruction {
    LirInstruction {
        id,
        kind: k,
        result: Some(LirRegister {
            id,
            ty: LirType::I64,
        }),
        debug_info: None,
    }
}

#[test]
fn binds_arguments_by_local_id_not_local_vector_order() {
    let f = LirFunction {
        def_id: None,
        name: Name::new("ordered_arguments"),
        signature: sig(&[LirType::I64, LirType::I64], LirType::I64),
        basic_blocks: vec![bb(0, vec![], ret(LirValue::local(1, LirType::I64)))],
        // Allocation order is deliberately not ABI order. MIR parameter IDs
        // remain 1 and 2 regardless of this representation order.
        locals: vec![
            fp_core::lir::LirLocal {
                id: 2,
                ty: LirType::I64,
                name: None,
                is_argument: true,
            },
            fp_core::lir::LirLocal {
                id: 1,
                ty: LirType::I64,
                name: None,
                is_argument: true,
            },
        ],
        stack_slots: vec![],
        calling_convention: CallingConvention::C,
        linkage: fp_core::lir::Linkage::Internal,
        is_declaration: false,
    };

    assert_eq!(
        LirInterpreter::new()
            .run_function(&f, &[Value::int(11), Value::int(22)])
            .expect("arguments bind by local ID"),
        Value::int(11)
    );
}

#[test]
fn preserves_type_handle_argument_after_aggregate_arguments() {
    use fp_core::ast::{Ty, TypePrimitive, ValuePointer, ValueTuple};

    let type_handle_ty = LirType::Ptr(Box::new(LirType::Void));
    let builder_ty = LirType::Struct {
        fields: vec![type_handle_ty.clone()],
        packed: false,
        name: Some("TypeBuilder".into()),
    };
    let str_ty = LirType::Struct {
        fields: vec![LirType::Ptr(Box::new(LirType::I8)), LirType::I64],
        packed: false,
        name: Some("__slice".into()),
    };
    let f = LirFunction {
        def_id: None,
        name: Name::new("with_field_shape"),
        signature: sig(
            &[builder_ty.clone(), str_ty.clone(), type_handle_ty.clone()],
            type_handle_ty.clone(),
        ),
        basic_blocks: vec![bb(
            0,
            vec![],
            ret(LirValue::local(3, type_handle_ty.clone())),
        )],
        locals: vec![
            fp_core::lir::LirLocal {
                id: 0,
                ty: type_handle_ty.clone(),
                name: None,
                is_argument: false,
            },
            fp_core::lir::LirLocal {
                id: 1,
                ty: builder_ty,
                name: None,
                is_argument: true,
            },
            fp_core::lir::LirLocal {
                id: 2,
                ty: str_ty,
                name: None,
                is_argument: true,
            },
            fp_core::lir::LirLocal {
                id: 3,
                ty: type_handle_ty,
                name: None,
                is_argument: true,
            },
        ],
        stack_slots: vec![],
        calling_convention: CallingConvention::C,
        linkage: fp_core::lir::Linkage::Internal,
        is_declaration: false,
    };

    assert_eq!(
        LirInterpreter::new()
            .run_function(
                &f,
                &[
                    Value::Tuple(ValueTuple::new(vec![Value::Pointer(
                        ValuePointer::managed(0)
                    )])),
                    Value::Tuple(ValueTuple::new(vec![
                        Value::Pointer(ValuePointer::managed(4096)),
                        Value::int(2),
                    ])),
                    Value::Type(Ty::Primitive(TypePrimitive::i64())),
                ],
            )
            .expect("type handle survives aggregate parameters"),
        Value::Type(Ty::Primitive(TypePrimitive::i64()))
    );
}

#[test]
fn constant() {
    let f = LirFunction {
        def_id: None,
        name: Name::new("main"),
        signature: sig(&[], LirType::I64),
        basic_blocks: vec![bb(
            0,
            vec![ins(LirInstructionKind::Add(int(40), int(2)))],
            ret(reg(0)),
        )],
        locals: vec![],
        stack_slots: vec![],
        calling_convention: CallingConvention::C,
        linkage: fp_core::lir::Linkage::Internal,
        is_declaration: false,
    };
    assert_eq!(
        LirInterpreter::new().run_main(&make(f)).unwrap(),
        Value::int(42)
    );
}

#[test]
fn alloca_preserves_typed_pointer_result() {
    let bool_ty = LirType::I1;
    let bool_ptr_ty = LirType::Ptr(Box::new(bool_ty.clone()));
    let f = LirFunction {
        def_id: None,
        name: Name::new("main"),
        signature: sig(&[], bool_ty.clone()),
        basic_blocks: vec![bb(
            0,
            vec![
                LirInstruction {
                    id: 0,
                    kind: LirInstructionKind::Alloca {
                        size: int(1),
                        alignment: 1,
                    },
                    result: Some(LirRegister {
                        id: 0,
                        ty: bool_ptr_ty.clone(),
                    }),
                    debug_info: None,
                },
                LirInstruction {
                    id: 1,
                    kind: LirInstructionKind::Store {
                        value: LirValue::constant(
                            LirConstant::integer(LirType::I1, LirInteger::I1(true))
                                .expect("valid i1 constant"),
                        ),
                        address: LirValue::register(0, bool_ptr_ty.clone()),
                        alignment: Some(1),
                        volatile: false,
                    },
                    result: None,
                    debug_info: None,
                },
                LirInstruction {
                    id: 2,
                    kind: LirInstructionKind::Load {
                        address: LirValue::register(0, bool_ptr_ty),
                        alignment: Some(1),
                        volatile: false,
                    },
                    result: Some(LirRegister {
                        id: 2,
                        ty: bool_ty.clone(),
                    }),
                    debug_info: None,
                },
            ],
            ret(LirValue::register(2, bool_ty)),
        )],
        locals: vec![],
        stack_slots: vec![],
        calling_convention: CallingConvention::C,
        linkage: fp_core::lir::Linkage::Internal,
        is_declaration: false,
    };

    assert_eq!(
        LirInterpreter::new().run_main(&make(f)).unwrap(),
        Value::bool(true)
    );
}

#[cfg(unix)]
#[test]
fn calls_libc_function_through_extern_c_declaration() {
    let getpid = LirFunction {
        def_id: None,
        name: Name::new("getpid"),
        signature: sig(&[], LirType::I32),
        basic_blocks: vec![],
        locals: vec![],
        stack_slots: vec![],
        calling_convention: CallingConvention::C,
        linkage: fp_core::lir::Linkage::External,
        is_declaration: true,
    };
    let main = LirFunction {
        def_id: None,
        name: Name::new("main"),
        signature: sig(&[], LirType::I64),
        basic_blocks: vec![bb(
            0,
            vec![i(
                0,
                LirInstructionKind::Call {
                    function: LirValue::function(
                        LirFunctionRef::Name(Name::new("getpid")),
                        LirType::Function {
                            return_type: Box::new(LirType::I32),
                            param_types: vec![],
                            is_variadic: false,
                        },
                    ),
                    args: vec![],
                    calling_convention: CallingConvention::C,
                    tail_call: false,
                },
            )],
            ret(reg(0)),
        )],
        locals: vec![],
        stack_slots: vec![],
        calling_convention: CallingConvention::C,
        linkage: fp_core::lir::Linkage::Internal,
        is_declaration: false,
    };

    let value = LirInterpreter::new().run_main(&LirBlob {
        data_layout: LirDataLayout::new(
            64,
            8,
            vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
        )
        .expect("valid test data layout"),
        functions: vec![main, getpid],
        globals: vec![],
        type_definitions: vec![],
        queries: vec![],
    });

    assert_eq!(value.unwrap(), Value::int(i64::from(std::process::id())));
}

#[test]
fn calls_registered_host_function_pointer() {
    extern "C" fn host_answer() -> i64 {
        42
    }

    let host = LirFunction {
        def_id: None,
        name: Name::new("host_answer"),
        signature: sig(&[], LirType::I64),
        basic_blocks: vec![],
        locals: vec![],
        stack_slots: vec![],
        calling_convention: CallingConvention::C,
        linkage: fp_core::lir::Linkage::External,
        is_declaration: true,
    };
    let main = LirFunction {
        def_id: None,
        name: Name::new("main"),
        signature: sig(&[], LirType::I64),
        basic_blocks: vec![bb(
            0,
            vec![i(
                0,
                LirInstructionKind::Call {
                    function: LirValue::function(
                        LirFunctionRef::Name(Name::new("host_answer")),
                        LirType::Function {
                            return_type: Box::new(LirType::I64),
                            param_types: vec![],
                            is_variadic: false,
                        },
                    ),
                    args: vec![],
                    calling_convention: CallingConvention::C,
                    tail_call: false,
                },
            )],
            ret(reg(0)),
        )],
        locals: vec![],
        stack_slots: vec![],
        calling_convention: CallingConvention::C,
        linkage: fp_core::lir::Linkage::Internal,
        is_declaration: false,
    };

    let mut registry = HostFunctionRegistry::new();
    registry
        .register(
            fp_core::HostFunctionDescriptor::new("host_answer", sig(&[], LirType::I64)),
            host_answer as *const std::ffi::c_void,
        )
        .unwrap();
    let mut interpreter = LirInterpreter::new();
    interpreter.set_host_functions(registry);
    assert_eq!(
        interpreter
            .run_main(&make_with_functions_and_globals(vec![main, host], vec![]))
            .unwrap(),
        Value::int(42)
    );
}

#[cfg(unix)]
#[test]
fn passes_interpreter_string_data_to_libc() {
    let strlen = LirFunction {
        def_id: None,
        name: Name::new("strlen"),
        signature: sig(&[LirType::Ptr(Box::new(LirType::I8))], LirType::I64),
        basic_blocks: vec![],
        locals: vec![],
        stack_slots: vec![],
        calling_convention: CallingConvention::C,
        linkage: fp_core::lir::Linkage::External,
        is_declaration: true,
    };
    let main = LirFunction {
        def_id: None,
        name: Name::new("main"),
        signature: sig(&[], LirType::I64),
        basic_blocks: vec![bb(
            0,
            vec![i(
                0,
                LirInstructionKind::Call {
                    function: LirValue::function(
                        LirFunctionRef::Name(Name::new("strlen")),
                        LirType::Function {
                            return_type: Box::new(LirType::I64),
                            param_types: vec![LirType::Ptr(Box::new(LirType::I8))],
                            is_variadic: false,
                        },
                    ),
                    args: vec![LirValue::constant(LirConstant::get_element_ptr(
                        LirType::Ptr(Box::new(LirType::I8)),
                        LirConstant::global_address(
                            LirType::Ptr(Box::new(LirType::I8)),
                            Name::new("hello"),
                        ),
                        vec![],
                        true,
                    ))],
                    calling_convention: CallingConvention::C,
                    tail_call: false,
                },
            )],
            ret(reg(0)),
        )],
        locals: vec![],
        stack_slots: vec![],
        calling_convention: CallingConvention::C,
        linkage: fp_core::lir::Linkage::Internal,
        is_declaration: false,
    };
    let global = LirGlobal {
        name: Name::new("hello"),
        ty: LirType::Array(Box::new(LirType::I8), 6),
        initializer: Some(LirConstant::bytes(
            LirType::Array(Box::new(LirType::I8), 6),
            b"hello\0".to_vec(),
        )),
        relocations: vec![],
        linkage: fp_core::lir::Linkage::Internal,
        visibility: fp_core::lir::Visibility::Default,
        is_constant: true,
        alignment: None,
        section: None,
    };

    assert_eq!(
        LirInterpreter::new()
            .run_main(&make_with_functions_and_globals(
                vec![main, strlen],
                vec![global]
            ))
            .unwrap(),
        Value::int(5)
    );
}

#[test]
fn arith_chain() {
    let f = LirFunction {
        def_id: None,
        name: Name::new("main"),
        signature: sig(&[], LirType::I64),
        basic_blocks: vec![bb(
            0,
            vec![
                i(10, LirInstructionKind::Mul(int(5), int(4))),
                i(11, LirInstructionKind::Mul(reg(10), int(3))),
                i(12, LirInstructionKind::Mul(reg(11), int(2))),
                i(13, LirInstructionKind::Mul(reg(12), int(1))),
            ],
            ret(reg(13)),
        )],
        locals: vec![],
        stack_slots: vec![],
        calling_convention: CallingConvention::C,
        linkage: fp_core::lir::Linkage::Internal,
        is_declaration: false,
    };
    assert_eq!(
        LirInterpreter::new().run_main(&make(f)).unwrap(),
        Value::int(120)
    );
}

fn cond_br_f(take: bool) -> LirBlob {
    make(LirFunction {
        def_id: None,
        name: Name::new("main"),
        signature: sig(&[], LirType::I64),
        basic_blocks: vec![
            LirBasicBlock {
                id: 0,
                label: None,
                instructions: vec![LirInstruction {
                    id: 0,
                    kind: LirInstructionKind::Eq(int(if take { 1 } else { 0 }), int(1)),
                    result: Some(LirRegister {
                        id: 0,
                        ty: LirType::I1,
                    }),
                    debug_info: None,
                }],
                terminator: LirTerminator::CondBr {
                    condition: LirValue::register(0, LirType::I1),
                    if_true: 1,
                    if_false: 2,
                },
                predecessors: vec![],
                successors: vec![1, 2],
            },
            bb(1, vec![], ret(int(7))),
            bb(2, vec![], ret(int(9))),
        ],
        locals: vec![],
        stack_slots: vec![],
        calling_convention: CallingConvention::C,
        linkage: fp_core::lir::Linkage::Internal,
        is_declaration: false,
    })
}

#[test]
fn cond_br_true() {
    assert_eq!(
        LirInterpreter::new().run_main(&cond_br_f(true)).unwrap(),
        Value::int(7)
    );
}

#[test]
fn cond_br_false() {
    assert_eq!(
        LirInterpreter::new().run_main(&cond_br_f(false)).unwrap(),
        Value::int(9)
    );
}

#[test]
fn insert_and_extract_struct_field() {
    let slice_ty = LirType::Struct {
        fields: vec![LirType::Ptr(Box::new(LirType::I8)), LirType::I64],
        packed: false,
        name: Some("slice".into()),
    };
    let f = LirFunction {
        def_id: None,
        name: Name::new("main"),
        signature: sig(&[], LirType::I64),
        basic_blocks: vec![bb(
            0,
            vec![
                LirInstruction::new(
                    10,
                    LirInstructionKind::InsertValue {
                        aggregate: LirValue::constant(LirConstant::undef(slice_ty.clone())),
                        element: LirValue::constant(
                            LirConstant::integer(LirType::I64, LirInteger::I64(0x1234))
                                .expect("valid i64 constant"),
                        ),
                        indices: vec![0],
                    },
                )
                .with_result(slice_ty.clone()),
                LirInstruction::new(
                    11,
                    LirInstructionKind::InsertValue {
                        aggregate: LirValue::register(10, slice_ty.clone()),
                        element: int(5),
                        indices: vec![1],
                    },
                )
                .with_result(slice_ty.clone()),
                LirInstruction::new(
                    12,
                    LirInstructionKind::ExtractValue {
                        aggregate: LirValue::register(11, slice_ty.clone()),
                        indices: vec![1],
                    },
                )
                .with_result(LirType::I64),
            ],
            ret(reg(12)),
        )],
        locals: vec![],
        stack_slots: vec![],
        calling_convention: CallingConvention::C,
        linkage: fp_core::lir::Linkage::Internal,
        is_declaration: false,
    };

    assert_eq!(
        LirInterpreter::new().run_main(&make(f)).unwrap(),
        Value::int(5)
    );
}

#[test]
fn extract_string_pointer_from_aggregate() {
    let array_ty = LirType::Array(Box::new(LirType::Ptr(Box::new(LirType::I8))), 1);
    let f = LirFunction {
        def_id: None,
        name: Name::new("main"),
        signature: sig(&[], LirType::Ptr(Box::new(LirType::I8))),
        basic_blocks: vec![bb(
            0,
            vec![
                LirInstruction::new(
                    10,
                    LirInstructionKind::InsertValue {
                        aggregate: LirValue::constant(LirConstant::undef(array_ty.clone())),
                        element: LirValue::constant(LirConstant::global_address(
                            LirType::Ptr(Box::new(LirType::I8)),
                            Name::new("abc"),
                        )),
                        indices: vec![0],
                    },
                )
                .with_result(array_ty.clone()),
                LirInstruction::new(
                    11,
                    LirInstructionKind::ExtractValue {
                        aggregate: LirValue::register(10, array_ty),
                        indices: vec![0],
                    },
                )
                .with_result(LirType::Ptr(Box::new(LirType::I8))),
            ],
            ret(LirValue::register(11, LirType::Ptr(Box::new(LirType::I8)))),
        )],
        locals: vec![],
        stack_slots: vec![],
        calling_convention: CallingConvention::C,
        linkage: fp_core::lir::Linkage::Internal,
        is_declaration: false,
    };

    let value = LirInterpreter::new()
        .run_main(&make_with_globals(
            f,
            vec![LirGlobal {
                name: Name::new("abc"),
                ty: LirType::Array(Box::new(LirType::I8), 3),
                initializer: Some(LirConstant::bytes(
                    LirType::Array(Box::new(LirType::I8), 3),
                    b"abc".to_vec(),
                )),
                relocations: vec![],
                linkage: fp_core::lir::Linkage::Internal,
                visibility: fp_core::lir::Visibility::Default,
                is_constant: true,
                alignment: None,
                section: None,
            }],
        ))
        .unwrap();
    let Value::Pointer(pointer) = value else {
        panic!("expected a VM pointer");
    };
    assert!(pointer.value >= 0x1000);
}

/// End-to-end check of `unionify(f)(u)`: `f` is a real (if trivial) LIR
/// function that transforms its `&str` argument, `u` is a reflected
/// `Ty::TypeBinaryOp(Union)` of two `Ty::Literal`s — the same shapes
/// `ComptimeOp::Unionify`/`handle_unionify_closure_call` operate on.
/// Exercises the closure-return + indirect-call path directly (not
/// currying — `unionify` itself is only ever called with its one
/// argument, `f`).
#[test]
fn unionify_closure_maps_over_union_members() {
    use fp_core::ast::{Ty, TypeBinaryOp, TypeBinaryOpKind, TypeLiteralString};

    let str_ty = LirType::Ptr(Box::new(LirType::I8));
    let shout_def_id = fp_core::hir::DefId::local(1);
    let shout_fn = LirFunction {
        def_id: Some(shout_def_id.clone()),
        name: Name::new("shout"),
        signature: sig(&[str_ty.clone()], str_ty.clone()),
        basic_blocks: vec![bb(
            0,
            vec![
                LirInstruction::new(
                    0,
                    LirInstructionKind::IntrinsicCall {
                        kind: fp_core::lir::LirIntrinsicKind::Format,
                        format: "{}!".to_string(),
                        args: vec![LirValue::register(1, str_ty.clone())],
                    },
                )
                .with_result(str_ty.clone()),
            ],
            ret(LirValue::register(0, str_ty.clone())),
        )],
        locals: vec![],
        stack_slots: vec![],
        calling_convention: CallingConvention::C,
        linkage: fp_core::lir::Linkage::Internal,
        is_declaration: false,
    };

    let mut interpreter = LirInterpreter::new();
    let program = fp_core::lir::LirProgram::from_single_blob(PackageId::new(""), make(shout_fn));
    interpreter.load_program(Rc::new(program)).unwrap();

    // Register 1: the closure `unionify(shout)` would have produced.
    interpreter.register_values.insert(
        1,
        TypedValue {
            ty: str_ty.clone(),
            value: Value::UnionifyClosure(shout_def_id),
        },
    );
    // Register 2: the reflected union type `"a" | "b"`.
    let union_ty = Ty::TypeBinaryOp(Box::new(TypeBinaryOp {
        kind: TypeBinaryOpKind::Union,
        lhs: Box::new(Ty::Literal(TypeLiteralString { value: "a".into() })),
        rhs: Box::new(Ty::Literal(TypeLiteralString { value: "b".into() })),
    }));
    interpreter.register_values.insert(
        2,
        TypedValue {
            ty: str_ty.clone(),
            value: Value::Type(union_ty),
        },
    );

    interpreter
        .handle_unionify_closure_call(
            3,
            &LirValue::register(1, str_ty.clone()),
            &[LirValue::register(2, str_ty.clone())],
            Some(&str_ty),
        )
        .expect("unionify closure call succeeds");

    let result = interpreter
        .register_values
        .get(&3)
        .expect("result register written")
        .value
        .clone();
    let Value::Type(Ty::TypeBinaryOp(op)) = result else {
        panic!("expected a reflected union type, got {result:?}");
    };
    assert_eq!(op.kind, TypeBinaryOpKind::Union);
    let Ty::Literal(lhs) = op.lhs.as_ref() else {
        panic!("expected literal lhs");
    };
    let Ty::Literal(rhs) = op.rhs.as_ref() else {
        panic!("expected literal rhs");
    };
    assert_eq!(lhs.value, "a!");
    assert_eq!(rhs.value, "b!");
}
