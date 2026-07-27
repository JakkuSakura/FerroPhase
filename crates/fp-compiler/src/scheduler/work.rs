use serde::{Deserialize, Serialize};

use super::identity::{
    AstId, BytecodeId, ConstValueId, FullyQualifiedPath, HirId, JitObjectId, LirId, MirId,
    NativeObjectId, RuntimeValueId, SavedOutputId,
    TypedAstId,
};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum CompilerWork {
    CompileUnitCompileNative {
        ast: AstId,
        path: FullyQualifiedPath,
    },
    CompileUnitAnswerComptime {
        ast: AstId,
        path: FullyQualifiedPath,
    },
    EnqueueGeneric {
        typed_ast: TypedAstId,
        path: FullyQualifiedPath,
        generic: GenericWorkRequest,
    },
    CompileUnitCompileBytecode {
        ast: AstId,
        path: FullyQualifiedPath,
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
    CompileUnitCompileNative,
    CompileUnitAnswerComptime {
        value: ConstValueId,
    },
    CompileUnitCompileBytecode,
    GenericQueued {
        generic: GenericWorkRequest,
    },
    AstUpdated {
        ast: AstId,
    },
    Revalidated,
}
