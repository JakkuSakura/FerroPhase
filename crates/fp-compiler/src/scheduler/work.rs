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
    CompileUnitCompileBytecode {
        ast: AstId,
        path: FullyQualifiedPath,
    },
    Revalidate {
        invalidated: Vec<InvalidatedObjectId>,
    },
    /// Load a registered package (e.g. `std`, or any other package a
    /// `PackageProvider` is registered for) on demand — submitted when a
    /// compile unit's typing pass reports a `Package` pending request; the
    /// scheduler's usual dependency/retry mechanism blocks that compile
    /// unit until this completes, then retries it.
    LoadPackage {
        name: String,
    },
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
    AstUpdated {
        ast: AstId,
    },
    Revalidated,
    PackageLoaded {
        name: String,
    },
}
