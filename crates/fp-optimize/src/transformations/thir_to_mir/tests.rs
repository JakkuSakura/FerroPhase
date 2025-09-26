use super::*;
use fp_core::id::Ident;

#[test]
fn test_mir_generator_creation() {
    let generator = MirGenerator::new();
    assert_eq!(generator.next_mir_id, 0);
    assert_eq!(generator.next_local_id, 0);
    assert_eq!(generator.next_bb_id, 0);
}

#[test]
fn test_local_creation() {
    let mut generator = MirGenerator::new();
    let ty = Ty {
        kind: TyKind::Tuple(Vec::new()),
    };

    let local1 = generator.create_local(ty.clone());
    let local2 = generator.create_local(ty.clone());

    assert_eq!(local1, 0);
    assert_eq!(local2, 1);
    assert_eq!(generator.next_local_id, 2);
}

#[test]
fn test_basic_block_creation() {
    let mut generator = MirGenerator::new();

    let bb1 = generator.create_basic_block();
    let bb2 = generator.create_basic_block();

    assert_eq!(bb1, 0);
    assert_eq!(bb2, 1);
    assert_eq!(generator.current_blocks.len(), 2);
}

#[test]
fn test_binary_op_transformation() {
    let generator = MirGenerator::new();

    assert_eq!(
        generator.transform_binary_op(BinOpKind::Add).unwrap(),
        mir::BinOp::Add
    );
    assert_eq!(
        generator.transform_binary_op(BinOpKind::Eq).unwrap(),
        mir::BinOp::Eq
    );
}

#[test]
fn test_empty_thir_program_transformation() {
    let mut generator = MirGenerator::new();
    let thir_program = thir::Program::new();

    let result = generator.transform(thir_program);
    assert!(result.is_ok());

    let mir_program = result.unwrap();
    assert_eq!(mir_program.items.len(), 0);
}
