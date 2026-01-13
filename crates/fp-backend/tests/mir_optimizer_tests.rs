use fp_backend::optimizer::{MirOptimizer, OptimizationPlan};
use fp_core::mir::{
    self,
    ty::{IntTy, Ty},
};
use fp_core::query::{QueryDocument, SqlDialect};
use fp_core::span::Span;
use std::collections::HashMap;

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

fn int_constant(value: i64) -> mir::Constant {
    mir::Constant {
        span: Span::new(0, 0, 0),
        user_ty: None,
        literal: mir::ConstantKind::Int(value),
    }
}

fn build_program(statements: Vec<mir::Statement>) -> mir::Program {
    let mut bodies = HashMap::new();
    let return_ty = Ty::int(IntTy::I32);
    let temp_ty = Ty::int(IntTy::I32);

    let mut block = mir::BasicBlockData::new(Some(mir::Terminator {
        source_info: Span::new(0, 0, 0),
        kind: mir::TerminatorKind::Return,
    }));
    block.statements = statements;

    let mut body = mir::Body::new(
        vec![block],
        vec![local_decl(return_ty.clone()), local_decl(temp_ty)],
        0,
        Span::new(0, 0, 0),
    );
    body.return_local = 0;

    bodies.insert(mir::BodyId::new(0), body);

    mir::Program {
        items: vec![mir::Item {
            mir_id: 0,
            kind: mir::ItemKind::Function(mir::Function {
                name: mir::Symbol::new("test_fn"),
                path: vec![mir::Symbol::new("test_fn")],
                def_id: None,
                sig: mir::FunctionSig {
                    inputs: Vec::new(),
                    output: return_ty,
                },
                body_id: mir::BodyId::new(0),
            }),
        }],
        bodies,
    }
}

#[test]
fn const_fold_rewrites_binary_op() {
    let statements = vec![mir::Statement {
        source_info: Span::new(0, 0, 0),
        kind: mir::StatementKind::Assign(
            mir::Place::from_local(1),
            mir::Rvalue::BinaryOp(
                mir::BinOp::Add,
                mir::Operand::Constant(int_constant(1)),
                mir::Operand::Constant(int_constant(2)),
            ),
        ),
    }];

    let mut program = build_program(statements);
    let query = QueryDocument::sql("SELECT const_fold FROM mir", SqlDialect::Generic);
    let plan = OptimizationPlan::from_query(query).expect("plan should parse");
    let optimizer = MirOptimizer::new();
    optimizer
        .apply_plan(&mut program, &plan)
        .expect("optimizer should succeed");

    let body = program.bodies.get(&mir::BodyId::new(0)).unwrap();
    let stmt = &body.basic_blocks[0].statements[0];
    let mir::StatementKind::Assign(_, rvalue) = &stmt.kind else {
        panic!("expected assign");
    };
    let mir::Rvalue::Use(mir::Operand::Constant(constant)) = rvalue else {
        panic!("expected constant use");
    };
    assert!(matches!(constant.literal, mir::ConstantKind::Int(3)));
}

#[test]
fn remove_nop_eliminates_empty_statements() {
    let statements = vec![
        mir::Statement {
            source_info: Span::new(0, 0, 0),
            kind: mir::StatementKind::Nop,
        },
        mir::Statement {
            source_info: Span::new(0, 0, 0),
            kind: mir::StatementKind::Assign(
                mir::Place::from_local(1),
                mir::Rvalue::Use(mir::Operand::Constant(int_constant(7))),
            ),
        },
    ];

    let mut program = build_program(statements);
    let query = QueryDocument::sql("SELECT remove_nop FROM mir", SqlDialect::Generic);
    let plan = OptimizationPlan::from_query(query).expect("plan should parse");
    let optimizer = MirOptimizer::new();
    optimizer
        .apply_plan(&mut program, &plan)
        .expect("optimizer should succeed");

    let body = program.bodies.get(&mir::BodyId::new(0)).unwrap();
    assert_eq!(body.basic_blocks[0].statements.len(), 1);
}
