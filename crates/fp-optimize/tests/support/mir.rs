use fp_core::hir::ty::Ty;
use fp_core::mir::{
    self, BasicBlockData, Body, BodyId, Function, FunctionSig, Item, ItemKind, Program, Statement,
    StatementKind, Terminator, TerminatorKind,
};
use fp_core::span::Span;
use fp_core::hir::ty::IntTy;

fn int_ty() -> Ty {
    Ty::int(IntTy::I32)
}

pub fn empty_program() -> Program {
    Program::new()
}

pub fn return_block() -> BasicBlockData {
    let terminator = Terminator {
        source_info: Span::new(0, 0, 0),
        kind: TerminatorKind::Return,
    };

    let mut block = BasicBlockData::new(Some(terminator));
    block.statements.push(Statement {
        source_info: Span::new(0, 0, 0),
        kind: StatementKind::Nop,
    });
    block
}

pub fn body_with_blocks(blocks: Vec<BasicBlockData>) -> (BodyId, Body) {
    let body = Body::new(blocks, Vec::new(), 0, Span::new(0, 0, 0));
    (BodyId(0), body)
}

pub fn function_item(body_id: BodyId) -> Item {
    let sig = FunctionSig {
        inputs: Vec::new(),
        output: int_ty(),
    };

    let function = Function { sig, body_id };

    Item {
        mir_id: 0,
        kind: ItemKind::Function(function),
    }
}
