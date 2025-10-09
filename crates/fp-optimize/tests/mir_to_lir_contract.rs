use fp_core::lir::{LirConstant, LirTerminator, LirType};
use fp_core::mir::ty::{IntTy, Ty};
use fp_core::mir::{self, FunctionSig, Item, ItemKind, Mutability, Operand};
use fp_core::span::Span;
use fp_optimize::transformations::LirGenerator;
use std::collections::HashMap;

mod support;

#[test]
fn lowers_return_function_from_support_helpers() {
    let mut program = support::mir::empty_program();
    let (body_id, mut body) = support::mir::body_with_blocks(vec![support::mir::return_block()]);
    body.return_local = 0;

    let mut bodies = HashMap::new();
    bodies.insert(body_id, body);
    program.bodies = bodies;
    program.items.push(support::mir::function_item(body_id));

    let mut generator = LirGenerator::new();
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
        sig: FunctionSig {
            inputs: Vec::new(),
            output: return_ty.clone(),
        },
        body_id,
    };

    let program = mir::Program {
        items: vec![Item {
            mir_id: 0,
            kind: ItemKind::Function(function),
        }],
        bodies,
    };

    let mut generator = LirGenerator::new();
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

    let mut generator = LirGenerator::new();
    let lir_program = generator
        .transform(program)
        .expect("lowering should succeed");

    assert!(lir_program.functions.is_empty());
    assert_eq!(lir_program.globals.len(), 1);
    let global = &lir_program.globals[0];
    assert_eq!(global.ty, LirType::I32);
    assert!(global.is_constant);
    match &global.initializer {
        Some(LirConstant::Int(value, lir_ty)) => {
            assert_eq!(*value, 7);
            assert_eq!(*lir_ty, LirType::I32);
        }
        other => panic!("expected integer initializer, got {:?}", other),
    }
}
