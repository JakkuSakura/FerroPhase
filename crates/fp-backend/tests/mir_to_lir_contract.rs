use fp_backend::transformations::LirGenerator;
use fp_core::lir::{
    LirDataLayout, LirInstructionKind, LirRelocationKind, LirRelocationTarget, LirTerminator,
    LirType, LirValue,
};
use fp_core::mir::LocalInfo;
use fp_core::mir::ty::{IntTy, Ty, TyKind, UintTy};
use fp_core::mir::{self, FunctionSig, Item, ItemKind, Mutability, Operand};
use fp_core::span::Span;
use std::collections::HashMap;

mod support;

fn generator() -> LirGenerator {
    LirGenerator::new(
        LirDataLayout::new(
            64,
            8,
            vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
        )
        .unwrap(),
    )
}

fn local_decl(ty: Ty, mutability: Mutability) -> mir::LocalDecl {
    mir::LocalDecl {
        mutability,
        local_info: LocalInfo::Other,
        internal: false,
        is_block_tail: None,
        ty,
        user_ty: None,
        source_info: Span::new(0, 0, 0),
    }
}

#[test]
fn lowers_return_function_from_support_helpers() {
    let mut program = support::mir::empty_program();
    let (body_id, mut body) = support::mir::body_with_blocks(vec![support::mir::return_block()]);
    body.return_local = 0;

    let mut bodies = HashMap::new();
    bodies.insert(body_id, body);
    program.bodies = bodies;
    program.items.push(support::mir::function_item(body_id));

    let mut generator = generator();
    let lir_program = generator
        .transform(program)
        .expect("lowering should succeed");

    assert_eq!(lir_program.functions.len(), 1);
    let func = &lir_program.functions[0];
    assert_eq!(func.basic_blocks.len(), 1);
    let block = &func.basic_blocks[0];
    assert_eq!(block.label.as_ref().map(|name| name.as_str()), Some("bb0"));
    assert!(matches!(block.terminator, LirTerminator::Return(_)));
}

#[test]
fn mangles_function_path_into_lir_name() {
    let mut bodies = HashMap::new();
    let (body_id, mut body) = support::mir::body_with_blocks(vec![support::mir::return_block()]);
    body.return_local = 0;
    bodies.insert(body_id, body);

    let return_ty = Ty::int(IntTy::I32);
    let function = mir::Function {
        name: mir::Symbol::new("leaf"),
        path: vec![mir::Symbol::new("module"), mir::Symbol::new("leaf")],
        def_id: None,
        substs: Vec::new(),
        sig: FunctionSig {
            inputs: Vec::new(),
            output: return_ty.clone(),
        },
        body_id,
        abi: mir::ty::Abi::Rust,
        is_extern: false,
        attrs: Vec::new(),
    };

    let program = mir::Program {
        items: vec![Item {
            mir_id: 0,
            kind: ItemKind::Function(function),
        }],
        bodies,
    };

    let mut generator = generator();
    let lir_program = generator
        .transform(program.clone())
        .expect("lowering should succeed");

    assert_eq!(lir_program.functions.len(), 1);
    let func = &lir_program.functions[0];
    assert_eq!(func.name.as_str(), "module__leaf");
    assert_eq!(func.signature.return_type, LirType::I32);
}

#[test]
fn lowers_static_integer_initializer_into_global_constant() {
    let ty = Ty::int(IntTy::I32);
    let constant = mir::Constant {
        span: Span::new(0, 0, 0),
        ty: ty.clone(),
        user_ty: None,
        literal: mir::ConstantKind::Int(7),
    };

    let static_item = mir::Static {
        ty: ty.clone(),
        init: Operand::Constant(constant),
        mutability: Mutability::Not,
    };

    let program = mir::Program {
        items: vec![Item {
            mir_id: 0,
            kind: ItemKind::Static(static_item),
        }],
        bodies: HashMap::new(),
    };

    let mut generator = generator();
    let lir_program = generator
        .transform(program)
        .expect("lowering should succeed");

    assert!(lir_program.functions.is_empty());
    assert_eq!(lir_program.globals.len(), 1);
    let global = &lir_program.globals[0];
    assert_eq!(global.ty, LirType::I32);
    assert!(global.is_constant);
    match &global.initializer {
        Some(constant) => {
            assert_eq!(constant.ty, LirType::I32);
            assert!(matches!(
                constant.kind,
                fp_core::lir::LirConstantKind::Data(fp_core::lir::LirConstantData::Integer(
                    fp_core::lir::LirInteger::I32(7)
                ))
            ));
        }
        other => panic!("expected integer initializer, got {:?}", other),
    }
}

#[test]
fn rejects_non_constant_static_initializer_operand() {
    let ty = Ty::int(IntTy::I32);
    let static_item = mir::Static {
        ty: ty.clone(),
        init: Operand::Copy(mir::Place::from_local(0)),
        mutability: Mutability::Not,
    };

    let program = mir::Program {
        items: vec![Item {
            mir_id: 0,
            kind: ItemKind::Static(static_item),
        }],
        bodies: HashMap::new(),
    };

    let mut generator = generator();
    let err = generator
        .transform(program)
        .expect_err("lowering should reject non-constant static initializers");
    let message = err.to_string();
    assert!(
        message.contains("unsupported static initializer operand"),
        "unexpected error: {message}"
    );
}

#[test]
fn rejects_tuple_constant_with_non_tuple_ty() {
    let ty = Ty::int(IntTy::I32);
    let constant = mir::Constant {
        span: Span::new(0, 0, 0),
        ty: ty.clone(),
        user_ty: None,
        literal: mir::ConstantKind::Val(mir::ConstValue::Tuple(vec![mir::ConstValue::Int(7)])),
    };

    let static_item = mir::Static {
        ty: ty.clone(),
        init: Operand::Constant(constant),
        mutability: Mutability::Not,
    };

    let program = mir::Program {
        items: vec![Item {
            mir_id: 0,
            kind: ItemKind::Static(static_item),
        }],
        bodies: HashMap::new(),
    };

    let mut generator = generator();
    let err = generator
        .transform(program)
        .expect_err("lowering should reject tuple constants with scalar type hints");
    let message = err.to_string();
    assert!(
        message.contains("tuple constant requires tuple type hint"),
        "unexpected error: {message}"
    );
}

#[test]
fn lowers_slice_static_into_bytes_with_relocation() {
    let ty = Ty {
        kind: TyKind::Slice(Box::new(Ty::uint(UintTy::U8))),
    };
    let constant = mir::Constant {
        span: Span::new(0, 0, 0),
        ty: ty.clone(),
        user_ty: None,
        literal: mir::ConstantKind::Str("hi".to_string()),
    };

    let static_item = mir::Static {
        ty: ty.clone(),
        init: Operand::Constant(constant),
        mutability: Mutability::Not,
    };

    let program = mir::Program {
        items: vec![Item {
            mir_id: 0,
            kind: ItemKind::Static(static_item),
        }],
        bodies: HashMap::new(),
    };

    let mut generator = generator();
    let lir_program = generator
        .transform(program)
        .expect("lowering should succeed");

    assert_eq!(lir_program.globals.len(), 2);
    let slice_global = &lir_program.globals[0];
    let data_global = &lir_program.globals[1];

    match &slice_global.initializer {
        Some(constant)
            if matches!(
                &constant.kind,
                fp_core::lir::LirConstantKind::Data(fp_core::lir::LirConstantData::Bytes(_))
            ) =>
        {
            let fp_core::lir::LirConstantKind::Data(fp_core::lir::LirConstantData::Bytes(bytes)) =
                &constant.kind
            else {
                unreachable!()
            };
            assert_eq!(bytes.len(), 16);
            assert_eq!(&bytes[8..16], &(2u64).to_le_bytes());
        }
        other => panic!(
            "expected byte initializer for slice global, got {:?}",
            other
        ),
    }
    assert_eq!(slice_global.relocations.len(), 1);
    let reloc = &slice_global.relocations[0];
    assert_eq!(reloc.offset, 0);
    assert_eq!(reloc.kind, LirRelocationKind::Abs64);
    assert_eq!(
        reloc.target,
        LirRelocationTarget::Global(data_global.name.clone())
    );
    assert_eq!(reloc.addend, 0);

    match &data_global.initializer {
        Some(constant) => {
            let fp_core::lir::LirConstantKind::Data(fp_core::lir::LirConstantData::Bytes(bytes)) =
                &constant.kind
            else {
                panic!("expected bytes")
            };
            assert_eq!(bytes, b"hi\0")
        }
        other => panic!(
            "expected byte initializer for backing string, got {:?}",
            other
        ),
    }
    assert!(data_global.relocations.is_empty());
}

#[test]
fn lowers_single_case_switchint_as_equality_compare() {
    let switch_ty = Ty::int(IntTy::I32);
    let discr = Operand::Constant(mir::Constant {
        span: Span::new(0, 0, 0),
        ty: switch_ty.clone(),
        user_ty: None,
        literal: mir::ConstantKind::Int(5),
    });
    let terminator = mir::Terminator {
        source_info: Span::new(0, 0, 0),
        kind: mir::TerminatorKind::SwitchInt {
            discr,
            switch_ty: switch_ty.clone(),
            targets: mir::SwitchTargets {
                values: vec![5],
                targets: vec![1],
                otherwise: 2,
            },
        },
    };

    let bb0 = mir::BasicBlockData::new(Some(terminator));
    let bb1 = support::mir::return_block();
    let bb2 = support::mir::return_block();
    let body = mir::Body::new(vec![bb0, bb1, bb2], Vec::new(), 0, Span::new(0, 0, 0));

    let body_id = mir::BodyId(0);
    let function = mir::Function {
        name: mir::Symbol::new("switch_test"),
        path: vec![mir::Symbol::new("switch_test")],
        def_id: None,
        substs: Vec::new(),
        sig: FunctionSig {
            inputs: Vec::new(),
            output: Ty {
                kind: TyKind::Tuple(Vec::new()),
            },
        },
        body_id,
        abi: mir::ty::Abi::Rust,
        is_extern: false,
        attrs: Vec::new(),
    };
    let program = mir::Program {
        items: vec![Item {
            mir_id: 0,
            kind: ItemKind::Function(function),
        }],
        bodies: HashMap::from([(body_id, body)]),
    };

    let mut generator = generator();
    let lir_program = generator
        .transform(program)
        .expect("lowering should succeed");

    let func = &lir_program.functions[0];
    let block = &func.basic_blocks[0];
    assert_eq!(block.instructions.len(), 1);
    let instr = &block.instructions[0];
    assert!(matches!(instr.kind, LirInstructionKind::Eq(_, _)));

    match &block.terminator {
        LirTerminator::CondBr { condition, .. } => {
            assert_eq!(*condition, LirValue::register(instr.id, LirType::I1));
        }
        other => panic!("expected CondBr terminator, got {:?}", other),
    }
}

#[test]
fn rejects_slice_intrinsic_assignment_with_wrong_arity() {
    let result_ty = Ty::int(IntTy::I32);
    let stmt = mir::Statement {
        source_info: Span::new(0, 0, 0),
        kind: mir::StatementKind::Assign(
            mir::Place::from_local(0),
            mir::Rvalue::IntrinsicCall {
                kind: fp_core::intrinsics::IntrinsicKind::Slice,
                format: String::new(),
                args: vec![
                    Operand::Constant(mir::Constant {
                        span: Span::new(0, 0, 0),
                        ty: Ty::int(IntTy::I32),
                        user_ty: None,
                        literal: mir::ConstantKind::Int(1),
                    }),
                    Operand::Constant(mir::Constant {
                        span: Span::new(0, 0, 0),
                        ty: Ty::int(IntTy::I32),
                        user_ty: None,
                        literal: mir::ConstantKind::Int(2),
                    }),
                ],
            },
        ),
    };
    let mut block = mir::BasicBlockData::new(Some(mir::Terminator {
        source_info: Span::new(0, 0, 0),
        kind: mir::TerminatorKind::Return,
    }));
    block.statements.push(stmt);
    let body = mir::Body::new(
        vec![block],
        vec![local_decl(result_ty.clone(), Mutability::Mut)],
        0,
        Span::new(0, 0, 0),
    );
    let body_id = mir::BodyId(0);
    let function = mir::Function {
        name: mir::Symbol::new("bad_slice"),
        path: vec![mir::Symbol::new("bad_slice")],
        def_id: None,
        substs: Vec::new(),
        sig: FunctionSig {
            inputs: Vec::new(),
            output: result_ty,
        },
        body_id,
        abi: mir::ty::Abi::Rust,
        is_extern: false,
        attrs: Vec::new(),
    };
    let program = mir::Program {
        items: vec![Item {
            mir_id: 0,
            kind: ItemKind::Function(function),
        }],
        bodies: HashMap::from([(body_id, body)]),
    };

    let mut generator = generator();
    let err = generator
        .transform(program)
        .expect_err("lowering should reject malformed slice intrinsic assignments");
    let message = err.to_string();
    assert!(
        message.contains("slice intrinsic expects 3 arguments, got 2"),
        "unexpected error: {message}"
    );
}

#[test]
fn rejects_unsupported_intrinsic_assignment() {
    let result_ty = Ty::int(IntTy::I32);
    let stmt = mir::Statement {
        source_info: Span::new(0, 0, 0),
        kind: mir::StatementKind::Assign(
            mir::Place::from_local(0),
            mir::Rvalue::IntrinsicCall {
                kind: fp_core::intrinsics::IntrinsicKind::Len,
                format: String::new(),
                args: Vec::new(),
            },
        ),
    };
    let mut block = mir::BasicBlockData::new(Some(mir::Terminator {
        source_info: Span::new(0, 0, 0),
        kind: mir::TerminatorKind::Return,
    }));
    block.statements.push(stmt);
    let body = mir::Body::new(
        vec![block],
        vec![local_decl(result_ty.clone(), Mutability::Mut)],
        0,
        Span::new(0, 0, 0),
    );
    let body_id = mir::BodyId(0);
    let function = mir::Function {
        name: mir::Symbol::new("bad_intrinsic"),
        path: vec![mir::Symbol::new("bad_intrinsic")],
        def_id: None,
        substs: Vec::new(),
        sig: FunctionSig {
            inputs: Vec::new(),
            output: result_ty,
        },
        body_id,
        abi: mir::ty::Abi::Rust,
        is_extern: false,
        attrs: Vec::new(),
    };
    let program = mir::Program {
        items: vec![Item {
            mir_id: 0,
            kind: ItemKind::Function(function),
        }],
        bodies: HashMap::from([(body_id, body)]),
    };

    let mut generator = generator();
    let err = generator
        .transform(program)
        .expect_err("lowering should reject unsupported intrinsic assignments");
    let message = err.to_string();
    assert!(
        message.contains("unsupported intrinsic in assignment: Len"),
        "unexpected error: {message}"
    );
}

#[test]
fn rejects_unhandled_mir_terminator() {
    let terminator = mir::Terminator {
        source_info: Span::new(0, 0, 0),
        kind: mir::TerminatorKind::Abort,
    };
    let block = mir::BasicBlockData::new(Some(terminator));
    let body = mir::Body::new(
        vec![block],
        vec![local_decl(Ty::int(IntTy::I32), Mutability::Not)],
        0,
        Span::new(0, 0, 0),
    );
    let body_id = mir::BodyId(0);
    let function = mir::Function {
        name: mir::Symbol::new("bad_term"),
        path: vec![mir::Symbol::new("bad_term")],
        def_id: None,
        substs: Vec::new(),
        sig: FunctionSig {
            inputs: Vec::new(),
            output: Ty::int(IntTy::I32),
        },
        body_id,
        abi: mir::ty::Abi::Rust,
        is_extern: false,
        attrs: Vec::new(),
    };
    let program = mir::Program {
        items: vec![Item {
            mir_id: 0,
            kind: ItemKind::Function(function),
        }],
        bodies: HashMap::from([(body_id, body)]),
    };

    let mut generator = generator();
    let err = generator
        .transform(program)
        .expect_err("lowering should reject unhandled MIR terminators");
    let message = err.to_string();
    assert!(
        message.contains("unhandled MIR terminator"),
        "unexpected error: {message}"
    );
}

#[test]
fn rejects_call_terminator_without_destination() {
    let call_terminator = mir::Terminator {
        source_info: Span::new(0, 0, 0),
        kind: mir::TerminatorKind::Call {
            func: Operand::Constant(mir::Constant {
                span: Span::new(0, 0, 0),
                ty: Ty::int(IntTy::I32),
                user_ty: None,
                literal: mir::ConstantKind::Str("no_dest_fn".to_string()),
            }),
            args: Vec::new(),
            destination: None,
            cleanup: None,
            from_hir_call: false,
            fn_span: Span::new(0, 0, 0),
        },
    };
    let block = mir::BasicBlockData::new(Some(call_terminator));
    let body = mir::Body::new(
        vec![block],
        vec![local_decl(Ty::int(IntTy::I32), Mutability::Not)],
        0,
        Span::new(0, 0, 0),
    );
    let body_id = mir::BodyId(0);
    let function = mir::Function {
        name: mir::Symbol::new("bad_call"),
        path: vec![mir::Symbol::new("bad_call")],
        def_id: None,
        substs: Vec::new(),
        sig: FunctionSig {
            inputs: Vec::new(),
            output: Ty::int(IntTy::I32),
        },
        body_id,
        abi: mir::ty::Abi::Rust,
        is_extern: false,
        attrs: Vec::new(),
    };
    let program = mir::Program {
        items: vec![Item {
            mir_id: 0,
            kind: ItemKind::Function(function),
        }],
        bodies: HashMap::from([(body_id, body)]),
    };

    let mut generator = generator();
    let err = generator
        .transform(program)
        .expect_err("lowering should reject call terminators without destination");
    let message = err.to_string();
    assert!(
        message.contains("call terminator without destination"),
        "unexpected error: {message}"
    );
}

#[test]
fn rejects_downcast_place_projection() {
    let place = mir::Place {
        local: 0,
        projection: vec![mir::PlaceElem::Downcast(None, 0)],
    };
    let stmt = mir::Statement {
        source_info: Span::new(0, 0, 0),
        kind: mir::StatementKind::Assign(
            mir::Place::from_local(1),
            mir::Rvalue::Use(Operand::Copy(place)),
        ),
    };
    let mut block = mir::BasicBlockData::new(Some(mir::Terminator {
        source_info: Span::new(0, 0, 0),
        kind: mir::TerminatorKind::Return,
    }));
    block.statements.push(stmt);
    let body = mir::Body::new(
        vec![block],
        vec![
            local_decl(Ty::int(IntTy::I32), Mutability::Not),
            local_decl(Ty::int(IntTy::I32), Mutability::Mut),
        ],
        0,
        Span::new(0, 0, 0),
    );
    let body_id = mir::BodyId(0);
    let function = mir::Function {
        name: mir::Symbol::new("downcast_test"),
        path: vec![mir::Symbol::new("downcast_test")],
        def_id: None,
        substs: Vec::new(),
        sig: FunctionSig {
            inputs: Vec::new(),
            output: Ty::int(IntTy::I32),
        },
        body_id,
        abi: mir::ty::Abi::Rust,
        is_extern: false,
        attrs: Vec::new(),
    };
    let program = mir::Program {
        items: vec![Item {
            mir_id: 0,
            kind: ItemKind::Function(function),
        }],
        bodies: HashMap::from([(body_id, body)]),
    };

    let mut generator = generator();
    let err = generator
        .transform(program)
        .expect_err("lowering should reject downcast place projections");
    let message = err.to_string();
    assert!(
        message.contains("downcast place projection is not supported"),
        "unexpected error: {message}"
    );
}
