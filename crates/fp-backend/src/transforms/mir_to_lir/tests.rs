use super::*;
use fp_core::{
    lir,
    mir::{
        self, Symbol as MirSymbol,
        ty::{IntTy, Ty},
    },
    span::Span,
};
use std::collections::HashMap;

fn test_layout() -> lir::LirDataLayout {
    lir::LirDataLayout::new(
        64,
        8,
        vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
    )
    .expect("valid test layout")
}

fn local_decl(ty: Ty) -> mir::LocalDecl {
    mir::LocalDecl {
        mutability: mir::Mutability::Not,
        local_info: mir::LocalInfo::Other,
        internal: false,
        is_block_tail: None,
        ty,
        user_ty: None,
        source_info: Span::new(0, 0, 0),
    }
}

#[test]
fn builds_function_signature_and_locals() {
    let mut bodies = HashMap::new();

    let return_ty = Ty::int(IntTy::I32);
    let param_ty = Ty::int(IntTy::I32);

    let mut body = mir::Body::new(
        vec![mir::BasicBlockData::new(Some(mir::Terminator {
            source_info: Span::new(0, 0, 0),
            kind: mir::TerminatorKind::Return,
        }))],
        vec![local_decl(return_ty.clone()), local_decl(param_ty.clone())],
        1,
        Span::new(0, 0, 0),
    );
    body.return_local = 0;

    bodies.insert(mir::BodyId::new(0), body);

    let mir_program = mir::MirCodeUnit {
        items: vec![mir::Item {
            mir_id: 0,
            kind: mir::ItemKind::Function(mir::Function {
                name: MirSymbol::new("test_fn"),
                def_id: None,
                substs: Vec::new(),
                sig: mir::FunctionSig {
                    inputs: vec![param_ty.clone()],
                    output: return_ty.clone(),
                },
                body_id: mir::BodyId::new(0),
                abi: mir::ty::Abi::Rust,
                is_extern: false,
                attrs: Vec::new(),
            }),
        }],
        bodies,
    };

    let mut generator = MirToLirLowerer::new(test_layout());
    let lir_program = generator
        .transform(mir_program)
        .expect("lowering should succeed");

    assert_eq!(lir_program.functions.len(), 1);
    let lir_func = &lir_program.functions[0];

    assert_eq!(lir_func.signature.params, vec![lir::LirType::I32]);
    assert_eq!(lir_func.signature.return_type, lir::LirType::I32);
    assert_eq!(lir_func.locals.len(), 2);
    assert!(!lir_func.locals[0].is_argument);
    assert!(lir_func.locals[1].is_argument);
}

#[test]
fn lowers_general_call_and_branches() {
    let mut bodies = HashMap::new();

    let return_ty = Ty::int(IntTy::I32);
    let param_ty = Ty::int(IntTy::I32);
    let temp_ty = Ty::int(IntTy::I32);

    let mut block0 = mir::BasicBlockData::new(None);
    block0.terminator = Some(mir::Terminator {
        source_info: Span::new(0, 0, 0),
        kind: mir::TerminatorKind::Call {
            func: mir::Operand::Constant(mir::Constant {
                span: Span::new(0, 0, 0),
                user_ty: None,
                ty: return_ty.clone(),
                literal: mir::ConstantKind::Fn(MirSymbol::new("foo")),
            }),
            args: vec![mir::Operand::Constant(mir::Constant {
                span: Span::new(0, 0, 0),
                ty: Ty::int(IntTy::I32),
                user_ty: None,
                literal: mir::ConstantKind::Int(1),
            })],
            destination: Some((mir::Place::from_local(2), 1)),
            cleanup: None,
            from_hir_call: false,
            fn_span: Span::new(0, 0, 0),
        },
    });

    let mut block1 = mir::BasicBlockData::new(None);
    block1.statements.push(mir::Statement {
        source_info: Span::new(0, 0, 0),
        kind: mir::StatementKind::Assign(
            mir::Place::from_local(0),
            mir::Rvalue::Use(mir::Operand::Move(mir::Place::from_local(2))),
        ),
    });
    block1.terminator = Some(mir::Terminator {
        source_info: Span::new(0, 0, 0),
        kind: mir::TerminatorKind::Return,
    });

    let mut body = mir::Body::new(
        vec![block0, block1],
        vec![
            local_decl(return_ty.clone()),
            local_decl(param_ty.clone()),
            local_decl(temp_ty),
        ],
        1,
        Span::new(0, 0, 0),
    );
    body.return_local = 0;

    bodies.insert(mir::BodyId::new(0), body);

    let foo = mir::Function {
        name: MirSymbol::new("foo"),
        def_id: None,
        substs: Vec::new(),
        sig: mir::FunctionSig {
            inputs: vec![param_ty.clone()],
            output: return_ty.clone(),
        },
        body_id: mir::BodyId::new(1),
        abi: mir::ty::Abi::Rust,
        is_extern: true,
        attrs: Vec::new(),
    };

    let mir_program = mir::MirCodeUnit {
        items: vec![
            mir::Item {
                mir_id: 1,
                kind: mir::ItemKind::Function(foo),
            },
            mir::Item {
                mir_id: 0,
                kind: mir::ItemKind::Function(mir::Function {
                    name: MirSymbol::new("test_fn"),
                    def_id: None,
                    substs: Vec::new(),
                    sig: mir::FunctionSig {
                        inputs: vec![param_ty.clone()],
                        output: return_ty.clone(),
                    },
                    body_id: mir::BodyId::new(0),
                    abi: mir::ty::Abi::Rust,
                    is_extern: false,
                    attrs: Vec::new(),
                }),
            },
        ],
        bodies,
    };

    let mut generator = MirToLirLowerer::new(test_layout());
    let lir_program = generator
        .transform(mir_program)
        .expect("lowering should succeed");

    let lir_func = lir_program
        .functions
        .iter()
        .find(|function| function.name.as_str() == "test_fn")
        .expect("test function should be lowered");
    assert_eq!(lir_func.basic_blocks.len(), 2);

    let entry_block = &lir_func.basic_blocks[0];
    assert_eq!(entry_block.successors, vec![1]);
    assert_eq!(entry_block.instructions.len(), 2);
    assert!(matches!(
        entry_block.instructions[0].kind,
        lir::LirInstructionKind::Alloca { .. }
    ));
    match &entry_block.instructions[1].kind {
        lir::LirInstructionKind::Call { function, .. } => match function {
            lir::LirValue {
                kind: lir::LirValueKind::Function(lir::LirFunctionRef::Name(name)),
                ..
            }
            | lir::LirValue {
                kind: lir::LirValueKind::Function(lir::LirFunctionRef::Package { name, .. }),
                ..
            }
            | lir::LirValue {
                kind: lir::LirValueKind::Global(name),
                ..
            } => assert_eq!(name.as_str(), "foo"),
            other => panic!("expected function call, got {:?}", other),
        },
        other => panic!("expected call instruction, got {:?}", other),
    }
    assert!(matches!(entry_block.terminator, lir::LirTerminator::Br(1)));

    let successor_block = &lir_func.basic_blocks[1];
    assert_eq!(successor_block.predecessors, vec![0]);
    match &successor_block.terminator {
        lir::LirTerminator::Return(Some(lir::LirValue {
            kind: lir::LirValueKind::Register(_),
            ..
        })) => {}
        other => panic!("expected return with register value, got {:?}", other),
    }
}
