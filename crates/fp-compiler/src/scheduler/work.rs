use serde::{Deserialize, Serialize};

use super::identity::{
    AstId, BytecodeId, ConstValueId, FullyQualifiedPath, HirId, JitObjectId, LirId, MirId,
    NativeObjectId, RawAstId, RequestId, RuntimeValueId, SavedOutputId, ScopeId, SourceId,
    TypedAstId,
};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum CompilerWork {
    ParseSource {
        source: SourceId,
    },
    NormalizeAst {
        raw_ast: RawAstId,
    },
    TypeAst {
        ast: AstId,
        scope: ScopeId,
        path: FullyQualifiedPath,
        consumers: Vec<LirConsumer>,
    },
    EnqueueGeneric {
        typed_ast: TypedAstId,
        path: FullyQualifiedPath,
        generic: GenericWorkRequest,
        consumers: Vec<LirConsumer>,
    },
    LowerToHir {
        typed_ast: TypedAstId,
        scope: ScopeId,
        path: FullyQualifiedPath,
        consumers: Vec<LirConsumer>,
    },
    LowerToMir {
        hir: HirId,
        path: FullyQualifiedPath,
        consumers: Vec<LirConsumer>,
    },
    LowerToLir {
        mir: MirId,
        path: FullyQualifiedPath,
        consumers: Vec<LirConsumer>,
    },
    Execute {
        lir: LirId,
        path: FullyQualifiedPath,
        mode: ExecutionMode,
        answer_to: Option<RequestId>,
    },
    SerializeBytecode {
        lir: LirId,
        path: FullyQualifiedPath,
    },
    EmitNative {
        lir: LirId,
        path: FullyQualifiedPath,
        jit: bool,
    },
    JitNative {
        path: FullyQualifiedPath,
        native_object: NativeObjectId,
    },
    SaveOutput {
        output: OutputObjectId,
        destination: OutputDestination,
    },
    Revalidate {
        invalidated: Vec<InvalidatedObjectId>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompileTimeNeed {
    pub description: String,
}

impl CompileTimeNeed {
    pub fn new(description: impl Into<String>) -> Self {
        Self {
            description: description.into(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeNeed {
    pub description: String,
}

impl TypeNeed {
    pub fn new(description: impl Into<String>) -> Self {
        Self {
            description: description.into(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct GenericWorkRequest {
    pub path: FullyQualifiedPath,
    pub description: String,
}

impl GenericWorkRequest {
    pub fn new(path: FullyQualifiedPath, description: impl Into<String>) -> Self {
        Self {
            path,
            description: description.into(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypingRequest {
    UnknownType(TypeNeed),
    Generic(GenericWorkRequest),
    Comptime(CompileTimeNeed),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExecutionMode {
    Comptime,
    Runtime,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum LirConsumer {
    ExecuteComptime,
    AnswerComptime(RequestId),
    ExecuteRuntime,
    Bytecode,
    Native,
    NativeJit,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct OutputDestination {
    pub key: String,
}

impl OutputDestination {
    pub fn new(key: impl Into<String>) -> Self {
        Self { key: key.into() }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum OutputObjectId {
    Bytecode(BytecodeId),
    Native(NativeObjectId),
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

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum CompilerAnswer {
    RawAst {
        raw_ast: RawAstId,
    },
    Ast {
        ast: AstId,
        scope: ScopeId,
        path: FullyQualifiedPath,
        consumers: Vec<LirConsumer>,
    },
    TypedAst {
        typed_ast: TypedAstId,
        requests: Vec<TypingRequest>,
    },
    GenericQueued {
        generic: GenericWorkRequest,
    },
    CompileTimeValue {
        value: ConstValueId,
    },
    BlockedOnRequest {
        request: RequestId,
    },
    AstUpdated {
        ast: AstId,
    },
    Hir {
        hir: HirId,
    },
    Mir {
        mir: MirId,
    },
    Lir {
        lir: LirId,
    },
    Bytecode {
        bytecode: BytecodeId,
    },
    NativeArtifact {
        native_object: NativeObjectId,
    },
    JitReady {
        jit_object: JitObjectId,
    },
    SavedOutput {
        saved_output: SavedOutputId,
    },
    RuntimeOutput {
        value: RuntimeValueId,
    },
    Revalidated,
}
