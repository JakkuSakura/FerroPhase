use fp_core::mir::{
    self, BasicBlockData, BodyId, MirBody, MirFunction, MirFunctionSig, MirItem, MirItemKind,
    MirProgram, Statement, StatementKind, Terminator, TerminatorKind,
};
use fp_core::span::Span;
use fp_core::types::{IntTy, Ty, TyKind, TypeFlags};

fn int_ty() -> Ty {
    Ty {
        kind: TyKind::Int(IntTy::I32),
        flags: TypeFlags::empty(),
    }
}

pub fn empty_program() -> MirProgram {
    MirProgram::new()
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

pub fn body_with_blocks(blocks: Vec<BasicBlockData>) -> (BodyId, MirBody) {
    let body = MirBody::new(blocks, Vec::new(), 0, Span::new(0, 0, 0));
    (BodyId(0), body)
}

pub fn function_item(body_id: BodyId) -> MirItem {
    let sig = MirFunctionSig {
        inputs: Vec::new(),
        output: int_ty(),
    };

    let function = MirFunction { sig, body_id };

    MirItem {
        mir_id: 0,
        kind: MirItemKind::Function(function),
    }
}
