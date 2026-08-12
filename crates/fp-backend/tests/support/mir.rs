#![allow(dead_code)]
use fp_core::mir::ty::{IntTy, Ty};
use fp_core::mir::{
    self, BasicBlockData, Body, BodyId, Function, FunctionSig, Item, ItemKind, LocalDecl,
    LocalInfo, Mutability, Program, Statement, StatementKind, Terminator, TerminatorKind,
};
use fp_core::span::Span;

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
    // Local 0 is always the return-value slot; a body with no locals at all
    // leaves it undeclared, so lowering has no storage/register to read the
    // return value back from.
    let return_local = LocalDecl {
        mutability: Mutability::Not,
        local_info: LocalInfo::Other,
        internal: false,
        is_block_tail: None,
        ty: int_ty(),
        user_ty: None,
        source_info: Span::new(0, 0, 0),
    };
    let body = Body::new(blocks, vec![return_local], 0, Span::new(0, 0, 0));
    (BodyId(0), body)
}

pub fn function_item(body_id: BodyId) -> Item {
    let sig = FunctionSig {
        inputs: Vec::new(),
        output: int_ty(),
    };

    let symbol = mir::Symbol::new("test_fn");

    let function = Function {
        name: symbol.clone(),
        path: vec![symbol],
        def_id: None,
        substs: Vec::new(),
        sig,
        body_id,
        abi: mir::ty::Abi::Rust,
        is_extern: false,
        attrs: Vec::new(),
    };

    Item {
        mir_id: 0,
        kind: ItemKind::Function(function),
    }
}
