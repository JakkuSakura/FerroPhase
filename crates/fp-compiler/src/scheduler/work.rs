use serde::{Deserialize, Serialize};

use super::identity::{
    AstId, BytecodeId, ConstValueId, FullyQualifiedPath, HirId, JitObjectId, LirId, MirId,
    NativeObjectId, RawAstId, RuntimeValueId, SavedOutputId, SourceId,
    TypedAstId,
};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum CompilerWork {
    ParseSource {
        source: SourceId,
    },
    NormalizeAst {
        raw_ast: RawAstId,
    },
    CompileUnitCompileNative {
        ast: AstId,
        path: FullyQualifiedPath,
    },
    CompileUnitAnswerComptime {
        typed_ast: TypedAstId,
        path: FullyQualifiedPath,
    },
    EnqueueGeneric {
        typed_ast: TypedAstId,
        path: FullyQualifiedPath,
        generic: GenericWorkRequest,
    },
    Revalidate {
        invalidated: Vec<InvalidatedObjectId>,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct GenericWorkRequest {
    pub path: FullyQualifiedPath,
    pub generic_params: Vec<String>,
    pub concrete_types: Vec<fp_core::ast::Ty>,
}

impl GenericWorkRequest {
    pub fn new(path: FullyQualifiedPath, generic_params: Vec<String>, concrete_types: Vec<fp_core::ast::Ty>) -> Self {
        Self { path, generic_params, concrete_types }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum InvalidatedObjectId {
    RawAst(RawAstId),
    Ast(AstId),
    TypedAst(TypedAstId),
    Hir(HirId),
    Mir(MirId),
    Lir(LirId),
    ConstValue(ConstValueId),
    RuntimeValue(RuntimeValueId),
    Bytecode(BytecodeId),
    Native(NativeObjectId),
    Jit(JitObjectId),
    SavedOutput(SavedOutputId),
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum CompilerAnswer {
    RawAst {
        raw_ast: RawAstId,
    },
    Ast {
        ast: AstId,
        path: FullyQualifiedPath,
    },
    CompileUnitCompileNative,
    CompileUnitAnswerComptime {
        value: ConstValueId,
    },
    GenericQueued {
        generic: GenericWorkRequest,
    },
    AstUpdated {
        ast: AstId,
    },
    Revalidated,
}
