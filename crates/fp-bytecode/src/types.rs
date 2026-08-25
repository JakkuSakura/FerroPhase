pub use fp_core::intrinsics::IntrinsicKind;
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BytecodeFile {
    pub version: u32,
    pub program: BytecodeProgram,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BytecodeProgram {
    pub const_pool: Vec<BytecodeConst>,
    pub functions: Vec<BytecodeFunction>,
    pub entry: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BytecodeFunction {
    pub name: String,
    pub param_types: Vec<fp_core::lir::LirType>,
    pub return_type: fp_core::lir::LirType,
    pub local_types: Vec<fp_core::lir::LirType>,
    pub blocks: Vec<BytecodeBlock>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BytecodeBlock {
    pub id: u32,
    pub code: Vec<BytecodeInstr>,
    pub terminator: BytecodeTerminator,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BytecodeInstr {
    LoadConst(u32),
    LoadLocal(u32),
    StoreLocal(u32),
    LoadPlace(BytecodePlace),
    StorePlace(BytecodePlace),
    BinaryOp(BytecodeBinOp),
    UnaryOp(BytecodeUnOp),
    IntrinsicCall {
        kind: IntrinsicKind,
        arg_count: u32,
        format: Option<String>,
        result_type: fp_core::lir::LirType,
    },
    MakeTuple(u32),
    MakeArray(u32),
    MakeList(u32),
    MakeMap(u32),
    ContainerGet,
    ContainerLen,
    Pop,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BytecodeTerminator {
    Return,
    Jump {
        target: u32,
    },
    JumpIfTrue {
        target: u32,
        otherwise: u32,
    },
    JumpIfFalse {
        target: u32,
        otherwise: u32,
    },
    SwitchInt {
        values: Vec<u128>,
        targets: Vec<u32>,
        otherwise: u32,
    },
    Call {
        callee: BytecodeCallee,
        arg_count: u32,
        destination: Option<BytecodePlace>,
        result_type: fp_core::lir::LirType,
        target: u32,
    },
    Abort,
    Unreachable,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BytecodeCallee {
    Function(String),
    Local(BytecodePlace),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BytecodeConst {
    Unit,
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
    Str(String),
    Function(String),
    Null,
    Undef,
    Tuple(Vec<BytecodeConst>),
    Array(Vec<BytecodeConst>),
    List(Vec<BytecodeConst>),
    Map(Vec<(BytecodeConst, BytecodeConst)>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BytecodePlace {
    pub local: u32,
    pub projection: Vec<BytecodePlaceElem>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BytecodePlaceElem {
    Field(u32),
    Index(u32),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BytecodeBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BytecodeUnOp {
    Not,
    Neg,
}

#[derive(Debug, Error)]
pub enum BytecodeError {
    #[error("bytecode lowering failed: {message}")]
    Lowering { message: String },
    #[error("bytecode encode failed: {0}")]
    Encode(#[from] bincode::Error),
    #[error("bytecode decode failed: {0}")]
    Decode(bincode::Error),
    #[error("bytecode format error: {message}")]
    Format { message: String },
}
