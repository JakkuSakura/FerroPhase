pub use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::diagnostics::{Diagnostic, diagnostic_manager};
use fp_core::mir;
use fp_core::winnow::ModalResult;
use fp_core::winnow::Parser;
use fp_core::winnow::error::{ContextError, ErrMode};
use fp_core::winnow::token::{literal, take_till, take_while};

pub const BYTECODE_MAGIC: [u8; 4] = *b"FPBC";
pub const BYTECODE_VERSION: u32 = 1;
const BYTECODE_LOWERING_CONTEXT: &str = "mir→bytecode";

#[derive(Debug, Clone)]
pub struct BytecodeFile {
    pub version: u32,
    pub program: BytecodeProgram,
}

#[derive(Debug, Clone)]
pub struct BytecodeProgram {
    pub const_pool: Vec<BytecodeConst>,
    pub functions: Vec<BytecodeFunction>,
    pub entry: Option<String>,
}

#[derive(Debug, Clone)]
pub struct BytecodeFunction {
    pub name: String,
    pub params: u32,
    pub locals: u32,
    pub blocks: Vec<BytecodeBlock>,
}

#[derive(Debug, Clone)]
pub struct BytecodeBlock {
    pub id: u32,
    pub code: Vec<BytecodeInstr>,
    pub terminator: BytecodeTerminator,
}

#[derive(Debug, Clone)]
pub enum BytecodeInstr {
    LoadConst(u32),
    LoadLocal(u32),
    StoreLocal(u32),
    LoadPlace(BytecodePlace),
    StorePlace(BytecodePlace),
    BinaryOp(BytecodeBinOp),
    UnaryOp(BytecodeUnOp),
    IntrinsicCall {
        kind: IntrinsicCallKind,
        arg_count: u32,
        format: Option<String>,
    },
    MakeTuple(u32),
    MakeArray(u32),
    MakeList(u32),
    MakeMap(u32),
    ContainerGet,
    ContainerLen,
    Pop,
}

#[derive(Debug, Clone)]
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
        target: u32,
    },
    Abort,
    Unreachable,
}

#[derive(Debug, Clone)]
pub enum BytecodeCallee {
    Function(String),
    Local(BytecodePlace),
}

#[derive(Debug, Clone)]
pub enum BytecodeConst {
    Unit,
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
    Str(String),
    Function(String),
    Null,
    Tuple(Vec<BytecodeConst>),
    Array(Vec<BytecodeConst>),
    List(Vec<BytecodeConst>),
    Map(Vec<(BytecodeConst, BytecodeConst)>),
}

#[derive(Debug, Clone)]
pub struct BytecodePlace {
    pub local: u32,
    pub projection: Vec<BytecodePlaceElem>,
}

#[derive(Debug, Clone)]
pub enum BytecodePlaceElem {
    Field(u32),
    Index(u32),
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum BytecodeUnOp {
    Not,
    Neg,
}

#[derive(Debug, Clone)]
pub enum BytecodeError {
    Lowering { message: String },
    Encode { message: String },
    Decode { message: String },
    Format { message: String },
}

impl std::fmt::Display for BytecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BytecodeError::Lowering { message } => {
                write!(f, "bytecode lowering failed: {}", message)
            }
            BytecodeError::Encode { message } => write!(f, "bytecode encode failed: {}", message),
            BytecodeError::Decode { message } => write!(f, "bytecode decode failed: {}", message),
            BytecodeError::Format { message } => write!(f, "bytecode format error: {}", message),
        }
    }
}

impl std::error::Error for BytecodeError {}

pub fn encode_file(program: &BytecodeProgram) -> Result<Vec<u8>, BytecodeError> {
    let file = BytecodeFile {
        version: BYTECODE_VERSION,
        program: program.clone(),
    };
    let mut encoded = Vec::new();
    encoded.extend_from_slice(&BYTECODE_MAGIC);
    encoded.extend_from_slice(&BYTECODE_VERSION.to_le_bytes());
    encode_program(&file.program, &mut encoded)?;
    Ok(encoded)
}

pub fn decode_file(bytes: &[u8]) -> Result<BytecodeFile, BytecodeError> {
    if bytes.len() < 8 {
        return Err(BytecodeError::Format {
            message: "bytecode header too short".to_string(),
        });
    }
    let magic = &bytes[..4];
    if magic != BYTECODE_MAGIC {
        return Err(BytecodeError::Format {
            message: "invalid bytecode magic".to_string(),
        });
    }
    let version = u32::from_le_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]);
    if version != BYTECODE_VERSION {
        return Err(BytecodeError::Format {
            message: format!(
                "unsupported bytecode version {}, expected {}",
                version, BYTECODE_VERSION
            ),
        });
    }
    let mut cursor = Cursor::new(&bytes[8..]);
    let program = decode_program(&mut cursor)?;
    validate_program(&program)?;
    Ok(BytecodeFile {
        version: BYTECODE_VERSION,
        program,
    })
}

fn encode_program(program: &BytecodeProgram, out: &mut Vec<u8>) -> Result<(), BytecodeError> {
    write_opt_string(out, program.entry.as_deref())?;
    write_vec(out, &program.const_pool, encode_const)?;
    write_vec(out, &program.functions, encode_function)?;
    Ok(())
}

fn decode_program(cursor: &mut Cursor<'_>) -> Result<BytecodeProgram, BytecodeError> {
    let entry = read_opt_string(cursor)?;
    let const_pool = read_vec(cursor, decode_const)?;
    let functions = read_vec(cursor, decode_function)?;
    Ok(BytecodeProgram {
        const_pool,
        functions,
        entry,
    })
}

fn encode_function(function: &BytecodeFunction, out: &mut Vec<u8>) -> Result<(), BytecodeError> {
    write_string(out, &function.name)?;
    write_u32(out, function.params)?;
    write_u32(out, function.locals)?;
    write_vec(out, &function.blocks, encode_block)?;
    Ok(())
}

fn decode_function(cursor: &mut Cursor<'_>) -> Result<BytecodeFunction, BytecodeError> {
    let name = read_string(cursor)?;
    let params = read_u32(cursor)?;
    let locals = read_u32(cursor)?;
    let blocks = read_vec(cursor, decode_block)?;
    Ok(BytecodeFunction {
        name,
        params,
        locals,
        blocks,
    })
}

fn encode_block(block: &BytecodeBlock, out: &mut Vec<u8>) -> Result<(), BytecodeError> {
    write_u32(out, block.id)?;
    write_vec(out, &block.code, encode_instr)?;
    encode_terminator(&block.terminator, out)?;
    Ok(())
}

fn decode_block(cursor: &mut Cursor<'_>) -> Result<BytecodeBlock, BytecodeError> {
    let id = read_u32(cursor)?;
    let code = read_vec(cursor, decode_instr)?;
    let terminator = decode_terminator(cursor)?;
    Ok(BytecodeBlock { id, code, terminator })
}

fn encode_instr(instr: &BytecodeInstr, out: &mut Vec<u8>) -> Result<(), BytecodeError> {
    match instr {
        BytecodeInstr::LoadConst(idx) => {
            write_u8(out, 0);
            write_u32(out, *idx)?;
        }
        BytecodeInstr::LoadLocal(idx) => {
            write_u8(out, 1);
            write_u32(out, *idx)?;
        }
        BytecodeInstr::StoreLocal(idx) => {
            write_u8(out, 2);
            write_u32(out, *idx)?;
        }
        BytecodeInstr::LoadPlace(place) => {
            write_u8(out, 3);
            encode_place(place, out)?;
        }
        BytecodeInstr::StorePlace(place) => {
            write_u8(out, 4);
            encode_place(place, out)?;
        }
        BytecodeInstr::BinaryOp(op) => {
            write_u8(out, 5);
            write_u8(out, encode_bin_op(op.clone()));
        }
        BytecodeInstr::UnaryOp(op) => {
            write_u8(out, 6);
            write_u8(out, encode_un_op(op.clone()));
        }
        BytecodeInstr::IntrinsicCall { kind, arg_count, format } => {
            write_u8(out, 7);
            write_u32(out, encode_intrinsic(*kind))?;
            write_u32(out, *arg_count)?;
            write_opt_string(out, format.as_deref())?;
        }
        BytecodeInstr::MakeTuple(count) => {
            write_u8(out, 8);
            write_u32(out, *count)?;
        }
        BytecodeInstr::MakeArray(count) => {
            write_u8(out, 9);
            write_u32(out, *count)?;
        }
        BytecodeInstr::MakeList(count) => {
            write_u8(out, 10);
            write_u32(out, *count)?;
        }
        BytecodeInstr::MakeMap(count) => {
            write_u8(out, 11);
            write_u32(out, *count)?;
        }
        BytecodeInstr::ContainerGet => write_u8(out, 12),
        BytecodeInstr::ContainerLen => write_u8(out, 13),
        BytecodeInstr::Pop => write_u8(out, 14),
    }
    Ok(())
}

fn decode_instr(cursor: &mut Cursor<'_>) -> Result<BytecodeInstr, BytecodeError> {
    let tag = read_u8(cursor)?;
    let instr = match tag {
        0 => BytecodeInstr::LoadConst(read_u32(cursor)?),
        1 => BytecodeInstr::LoadLocal(read_u32(cursor)?),
        2 => BytecodeInstr::StoreLocal(read_u32(cursor)?),
        3 => BytecodeInstr::LoadPlace(decode_place(cursor)?),
        4 => BytecodeInstr::StorePlace(decode_place(cursor)?),
        5 => BytecodeInstr::BinaryOp(decode_bin_op(read_u8(cursor)?)?),
        6 => BytecodeInstr::UnaryOp(decode_un_op(read_u8(cursor)?)?),
        7 => {
            let kind = decode_intrinsic(read_u32(cursor)?)?;
            let arg_count = read_u32(cursor)?;
            let format = read_opt_string(cursor)?;
            BytecodeInstr::IntrinsicCall {
                kind,
                arg_count,
                format,
            }
        }
        8 => BytecodeInstr::MakeTuple(read_u32(cursor)?),
        9 => BytecodeInstr::MakeArray(read_u32(cursor)?),
        10 => BytecodeInstr::MakeList(read_u32(cursor)?),
        11 => BytecodeInstr::MakeMap(read_u32(cursor)?),
        12 => BytecodeInstr::ContainerGet,
        13 => BytecodeInstr::ContainerLen,
        14 => BytecodeInstr::Pop,
        _ => {
            return Err(BytecodeError::Decode {
                message: format!("unknown instruction tag {}", tag),
            })
        }
    };
    Ok(instr)
}

fn encode_terminator(term: &BytecodeTerminator, out: &mut Vec<u8>) -> Result<(), BytecodeError> {
    match term {
        BytecodeTerminator::Return => write_u8(out, 0),
        BytecodeTerminator::Jump { target } => {
            write_u8(out, 1);
            write_u32(out, *target)?;
        }
        BytecodeTerminator::JumpIfTrue { target, otherwise } => {
            write_u8(out, 2);
            write_u32(out, *target)?;
            write_u32(out, *otherwise)?;
        }
        BytecodeTerminator::JumpIfFalse { target, otherwise } => {
            write_u8(out, 3);
            write_u32(out, *target)?;
            write_u32(out, *otherwise)?;
        }
        BytecodeTerminator::SwitchInt { values, targets, otherwise } => {
            write_u8(out, 4);
            write_vec(out, values, |value, out| { write_u128(out, *value); Ok(()) })?;
            write_vec(out, targets, |value, out| { write_u32(out, *value) })?;
            write_u32(out, *otherwise)?;
        }
        BytecodeTerminator::Call { callee, arg_count, destination, target } => {
            write_u8(out, 5);
            encode_callee(callee, out)?;
            write_u32(out, *arg_count)?;
            write_opt_place(out, destination.as_ref())?;
            write_u32(out, *target)?;
        }
        BytecodeTerminator::Abort => write_u8(out, 6),
        BytecodeTerminator::Unreachable => write_u8(out, 7),
    }
    Ok(())
}

fn decode_terminator(cursor: &mut Cursor<'_>) -> Result<BytecodeTerminator, BytecodeError> {
    let tag = read_u8(cursor)?;
    let term = match tag {
        0 => BytecodeTerminator::Return,
        1 => BytecodeTerminator::Jump { target: read_u32(cursor)? },
        2 => BytecodeTerminator::JumpIfTrue { target: read_u32(cursor)?, otherwise: read_u32(cursor)? },
        3 => BytecodeTerminator::JumpIfFalse { target: read_u32(cursor)?, otherwise: read_u32(cursor)? },
        4 => {
            let values = read_vec(cursor, |cursor| read_u128(cursor))?;
            let targets = read_vec(cursor, |cursor| read_u32(cursor))?;
            let otherwise = read_u32(cursor)?;
            BytecodeTerminator::SwitchInt { values, targets, otherwise }
        }
        5 => {
            let callee = decode_callee(cursor)?;
            let arg_count = read_u32(cursor)?;
            let destination = read_opt_place(cursor)?;
            let target = read_u32(cursor)?;
            BytecodeTerminator::Call { callee, arg_count, destination, target }
        }
        6 => BytecodeTerminator::Abort,
        7 => BytecodeTerminator::Unreachable,
        _ => {
            return Err(BytecodeError::Decode {
                message: format!("unknown terminator tag {}", tag),
            })
        }
    };
    Ok(term)
}

fn encode_place(place: &BytecodePlace, out: &mut Vec<u8>) -> Result<(), BytecodeError> {
    write_u32(out, place.local)?;
    write_vec(out, &place.projection, encode_place_elem)?;
    Ok(())
}

fn decode_place(cursor: &mut Cursor<'_>) -> Result<BytecodePlace, BytecodeError> {
    let local = read_u32(cursor)?;
    let projection = read_vec(cursor, decode_place_elem)?;
    Ok(BytecodePlace { local, projection })
}

fn encode_place_elem(elem: &BytecodePlaceElem, out: &mut Vec<u8>) -> Result<(), BytecodeError> {
    match elem {
        BytecodePlaceElem::Field(id) => {
            write_u8(out, 0);
            write_u32(out, *id)?;
        }
        BytecodePlaceElem::Index(id) => {
            write_u8(out, 1);
            write_u32(out, *id)?;
        }
    }
    Ok(())
}

fn decode_place_elem(cursor: &mut Cursor<'_>) -> Result<BytecodePlaceElem, BytecodeError> {
    let tag = read_u8(cursor)?;
    match tag {
        0 => Ok(BytecodePlaceElem::Field(read_u32(cursor)?)),
        1 => Ok(BytecodePlaceElem::Index(read_u32(cursor)?)),
        _ => Err(BytecodeError::Decode {
            message: format!("unknown place elem tag {}", tag),
        }),
    }
}

fn encode_callee(callee: &BytecodeCallee, out: &mut Vec<u8>) -> Result<(), BytecodeError> {
    match callee {
        BytecodeCallee::Function(name) => {
            write_u8(out, 0);
            write_string(out, name)?;
        }
        BytecodeCallee::Local(place) => {
            write_u8(out, 1);
            encode_place(place, out)?;
        }
    }
    Ok(())
}

fn decode_callee(cursor: &mut Cursor<'_>) -> Result<BytecodeCallee, BytecodeError> {
    let tag = read_u8(cursor)?;
    match tag {
        0 => Ok(BytecodeCallee::Function(read_string(cursor)?)),
        1 => Ok(BytecodeCallee::Local(decode_place(cursor)?)),
        _ => Err(BytecodeError::Decode {
            message: format!("unknown callee tag {}", tag),
        }),
    }
}

fn encode_const(value: &BytecodeConst, out: &mut Vec<u8>) -> Result<(), BytecodeError> {
    match value {
        BytecodeConst::Unit => write_u8(out, 0),
        BytecodeConst::Bool(val) => {
            write_u8(out, 1);
            write_u8(out, if *val { 1 } else { 0 });
        }
        BytecodeConst::Int(val) => {
            write_u8(out, 2);
            write_i64(out, *val);
        }
        BytecodeConst::UInt(val) => {
            write_u8(out, 3);
            write_u64(out, *val);
        }
        BytecodeConst::Float(val) => {
            write_u8(out, 4);
            write_f64(out, *val);
        }
        BytecodeConst::Str(val) => {
            write_u8(out, 5);
            write_string(out, val)?;
        }
        BytecodeConst::Function(name) => {
            write_u8(out, 6);
            write_string(out, name)?;
        }
        BytecodeConst::Null => write_u8(out, 7),
        BytecodeConst::Tuple(values) => {
            write_u8(out, 8);
            write_vec(out, values, encode_const)?;
        }
        BytecodeConst::Array(values) => {
            write_u8(out, 9);
            write_vec(out, values, encode_const)?;
        }
        BytecodeConst::List(values) => {
            write_u8(out, 10);
            write_vec(out, values, encode_const)?;
        }
        BytecodeConst::Map(values) => {
            write_u8(out, 11);
            write_u32(out, values.len() as u32)?;
            for (k, v) in values {
                encode_const(k, out)?;
                encode_const(v, out)?;
            }
        }
    }
    Ok(())
}

fn decode_const(cursor: &mut Cursor<'_>) -> Result<BytecodeConst, BytecodeError> {
    let tag = read_u8(cursor)?;
    match tag {
        0 => Ok(BytecodeConst::Unit),
        1 => Ok(BytecodeConst::Bool(read_u8(cursor)? != 0)),
        2 => Ok(BytecodeConst::Int(read_i64(cursor)?)),
        3 => Ok(BytecodeConst::UInt(read_u64(cursor)?)),
        4 => Ok(BytecodeConst::Float(read_f64(cursor)?)),
        5 => Ok(BytecodeConst::Str(read_string(cursor)?)),
        6 => Ok(BytecodeConst::Function(read_string(cursor)?)),
        7 => Ok(BytecodeConst::Null),
        8 => Ok(BytecodeConst::Tuple(read_vec(cursor, decode_const)?)),
        9 => Ok(BytecodeConst::Array(read_vec(cursor, decode_const)?)),
        10 => Ok(BytecodeConst::List(read_vec(cursor, decode_const)?)),
        11 => {
            let len = read_u32(cursor)? as usize;
            let mut values = Vec::with_capacity(len);
            for _ in 0..len {
                let key = decode_const(cursor)?;
                let value = decode_const(cursor)?;
                values.push((key, value));
            }
            Ok(BytecodeConst::Map(values))
        }
        _ => Err(BytecodeError::Decode {
            message: format!("unknown const tag {}", tag),
        }),
    }
}

fn encode_bin_op(op: BytecodeBinOp) -> u8 {
    match op {
        BytecodeBinOp::Add => 0,
        BytecodeBinOp::Sub => 1,
        BytecodeBinOp::Mul => 2,
        BytecodeBinOp::Div => 3,
        BytecodeBinOp::Rem => 4,
        BytecodeBinOp::And => 5,
        BytecodeBinOp::Or => 6,
        BytecodeBinOp::BitXor => 7,
        BytecodeBinOp::BitAnd => 8,
        BytecodeBinOp::BitOr => 9,
        BytecodeBinOp::Shl => 10,
        BytecodeBinOp::Shr => 11,
        BytecodeBinOp::Eq => 12,
        BytecodeBinOp::Lt => 13,
        BytecodeBinOp::Le => 14,
        BytecodeBinOp::Ne => 15,
        BytecodeBinOp::Ge => 16,
        BytecodeBinOp::Gt => 17,
    }
}

fn decode_bin_op(tag: u8) -> Result<BytecodeBinOp, BytecodeError> {
    Ok(match tag {
        0 => BytecodeBinOp::Add,
        1 => BytecodeBinOp::Sub,
        2 => BytecodeBinOp::Mul,
        3 => BytecodeBinOp::Div,
        4 => BytecodeBinOp::Rem,
        5 => BytecodeBinOp::And,
        6 => BytecodeBinOp::Or,
        7 => BytecodeBinOp::BitXor,
        8 => BytecodeBinOp::BitAnd,
        9 => BytecodeBinOp::BitOr,
        10 => BytecodeBinOp::Shl,
        11 => BytecodeBinOp::Shr,
        12 => BytecodeBinOp::Eq,
        13 => BytecodeBinOp::Lt,
        14 => BytecodeBinOp::Le,
        15 => BytecodeBinOp::Ne,
        16 => BytecodeBinOp::Ge,
        17 => BytecodeBinOp::Gt,
        _ => {
            return Err(BytecodeError::Decode {
                message: format!("unknown binop tag {}", tag),
            })
        }
    })
}

fn encode_un_op(op: BytecodeUnOp) -> u8 {
    match op {
        BytecodeUnOp::Not => 0,
        BytecodeUnOp::Neg => 1,
    }
}

fn decode_un_op(tag: u8) -> Result<BytecodeUnOp, BytecodeError> {
    Ok(match tag {
        0 => BytecodeUnOp::Not,
        1 => BytecodeUnOp::Neg,
        _ => {
            return Err(BytecodeError::Decode {
                message: format!("unknown unop tag {}", tag),
            })
        }
    })
}

fn encode_intrinsic(kind: IntrinsicCallKind) -> u32 {
    use IntrinsicCallKind::*;
    match kind {
        Println => 0,
        Print => 1,
        Format => 2,
        Len => 3,
        Slice => 4,
        DebugAssertions => 5,
        Input => 6,
        Panic => 7,
        CatchUnwind => 8,
        TimeNow => 9,
        Sleep => 10,
        Spawn => 11,
        Join => 12,
        Select => 13,
        SizeOf => 14,
        ReflectFields => 15,
        HasMethod => 16,
        TypeName => 17,
        TypeOf => 18,
        CreateStruct => 19,
        CloneStruct => 20,
        AddField => 21,
        HasField => 22,
        FieldCount => 23,
        MethodCount => 24,
        FieldType => 25,
        VecType => 26,
        FieldNameAt => 27,
        StructSize => 28,
        GenerateMethod => 29,
        CompileError => 30,
        CompileWarning => 31,
        ProcMacroTokenStreamFromStr => 32,
        ProcMacroTokenStreamToString => 33,
    }
}

fn decode_intrinsic(tag: u32) -> Result<IntrinsicCallKind, BytecodeError> {
    use IntrinsicCallKind::*;
    Ok(match tag {
        0 => Println,
        1 => Print,
        2 => Format,
        3 => Len,
        4 => Slice,
        5 => DebugAssertions,
        6 => Input,
        7 => Panic,
        8 => CatchUnwind,
        9 => TimeNow,
        10 => Sleep,
        11 => Spawn,
        12 => Join,
        13 => Select,
        14 => SizeOf,
        15 => ReflectFields,
        16 => HasMethod,
        17 => TypeName,
        18 => TypeOf,
        19 => CreateStruct,
        20 => CloneStruct,
        21 => AddField,
        22 => HasField,
        23 => FieldCount,
        24 => MethodCount,
        25 => FieldType,
        26 => VecType,
        27 => FieldNameAt,
        28 => StructSize,
        29 => GenerateMethod,
        30 => CompileError,
        31 => CompileWarning,
        32 => ProcMacroTokenStreamFromStr,
        33 => ProcMacroTokenStreamToString,
        _ => {
            return Err(BytecodeError::Decode {
                message: format!("unknown intrinsic tag {}", tag),
            })
        }
    })
}

fn write_u8(out: &mut Vec<u8>, value: u8) {
    out.push(value);
}

fn write_u32(out: &mut Vec<u8>, value: u32) -> Result<(), BytecodeError> {
    out.extend_from_slice(&value.to_le_bytes());
    Ok(())
}

fn write_u64(out: &mut Vec<u8>, value: u64) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn write_u128(out: &mut Vec<u8>, value: u128) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn write_i64(out: &mut Vec<u8>, value: i64) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn write_f64(out: &mut Vec<u8>, value: f64) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn write_string(out: &mut Vec<u8>, value: &str) -> Result<(), BytecodeError> {
    let bytes = value.as_bytes();
    let len = u32::try_from(bytes.len()).map_err(|_| BytecodeError::Encode {
        message: "string too long".to_string(),
    })?;
    write_u32(out, len)?;
    out.extend_from_slice(bytes);
    Ok(())
}

fn write_opt_string(out: &mut Vec<u8>, value: Option<&str>) -> Result<(), BytecodeError> {
    match value {
        Some(value) => {
            write_u8(out, 1);
            write_string(out, value)?;
        }
        None => write_u8(out, 0),
    }
    Ok(())
}

fn write_vec<T, F>(out: &mut Vec<u8>, values: &[T], mut encode: F) -> Result<(), BytecodeError>
where
    F: FnMut(&T, &mut Vec<u8>) -> Result<(), BytecodeError>,
{
    let len = u32::try_from(values.len()).map_err(|_| BytecodeError::Encode {
        message: "vector too long".to_string(),
    })?;
    write_u32(out, len)?;
    for value in values {
        encode(value, out)?;
    }
    Ok(())
}

fn write_opt_place(out: &mut Vec<u8>, place: Option<&BytecodePlace>) -> Result<(), BytecodeError> {
    match place {
        Some(place) => {
            write_u8(out, 1);
            encode_place(place, out)?;
        }
        None => write_u8(out, 0),
    }
    Ok(())
}

struct Cursor<'a> {
    bytes: &'a [u8],
    offset: usize,
}

impl<'a> Cursor<'a> {
    fn new(bytes: &'a [u8]) -> Self {
        Self { bytes, offset: 0 }
    }

    fn read_exact(&mut self, len: usize) -> Result<&'a [u8], BytecodeError> {
        if self.offset + len > self.bytes.len() {
            return Err(BytecodeError::Decode {
                message: "unexpected end of bytecode".to_string(),
            });
        }
        let slice = &self.bytes[self.offset..self.offset + len];
        self.offset += len;
        Ok(slice)
    }
}

fn read_u8(cursor: &mut Cursor<'_>) -> Result<u8, BytecodeError> {
    Ok(cursor.read_exact(1)?[0])
}

fn read_u32(cursor: &mut Cursor<'_>) -> Result<u32, BytecodeError> {
    let bytes = cursor.read_exact(4)?;
    Ok(u32::from_le_bytes(bytes.try_into().unwrap()))
}

fn read_u64(cursor: &mut Cursor<'_>) -> Result<u64, BytecodeError> {
    let bytes = cursor.read_exact(8)?;
    Ok(u64::from_le_bytes(bytes.try_into().unwrap()))
}

fn read_u128(cursor: &mut Cursor<'_>) -> Result<u128, BytecodeError> {
    let bytes = cursor.read_exact(16)?;
    Ok(u128::from_le_bytes(bytes.try_into().unwrap()))
}

fn read_i64(cursor: &mut Cursor<'_>) -> Result<i64, BytecodeError> {
    let bytes = cursor.read_exact(8)?;
    Ok(i64::from_le_bytes(bytes.try_into().unwrap()))
}

fn read_f64(cursor: &mut Cursor<'_>) -> Result<f64, BytecodeError> {
    let bytes = cursor.read_exact(8)?;
    Ok(f64::from_le_bytes(bytes.try_into().unwrap()))
}

fn read_string(cursor: &mut Cursor<'_>) -> Result<String, BytecodeError> {
    let len = read_u32(cursor)? as usize;
    let bytes = cursor.read_exact(len)?;
    String::from_utf8(bytes.to_vec()).map_err(|err| BytecodeError::Decode {
        message: format!("invalid utf8 string: {}", err),
    })
}

fn read_opt_string(cursor: &mut Cursor<'_>) -> Result<Option<String>, BytecodeError> {
    let flag = read_u8(cursor)?;
    if flag == 0 {
        Ok(None)
    } else {
        read_string(cursor).map(Some)
    }
}

fn read_vec<T, F>(cursor: &mut Cursor<'_>, mut decode: F) -> Result<Vec<T>, BytecodeError>
where
    F: FnMut(&mut Cursor<'_>) -> Result<T, BytecodeError>,
{
    let len = read_u32(cursor)? as usize;
    let mut values = Vec::with_capacity(len);
    for _ in 0..len {
        values.push(decode(cursor)?);
    }
    Ok(values)
}

fn read_opt_place(cursor: &mut Cursor<'_>) -> Result<Option<BytecodePlace>, BytecodeError> {
    let flag = read_u8(cursor)?;
    if flag == 0 {
        Ok(None)
    } else {
        decode_place(cursor).map(Some)
    }
}

fn validate_program(program: &BytecodeProgram) -> Result<(), BytecodeError> {
    if let Some(entry) = &program.entry {
        if !program.functions.iter().any(|func| &func.name == entry) {
            return Err(BytecodeError::Format {
                message: format!("entry function {} not found", entry),
            });
        }
    }
    for function in &program.functions {
        validate_function(function, program.const_pool.len())?;
    }
    Ok(())
}

fn validate_function(
    function: &BytecodeFunction,
    const_pool_len: usize,
) -> Result<(), BytecodeError> {
    if function.params > function.locals {
        return Err(BytecodeError::Format {
            message: format!(
                "function {} has {} params but only {} locals",
                function.name, function.params, function.locals
            ),
        });
    }
    if function.blocks.is_empty() {
        return Err(BytecodeError::Format {
            message: format!("function {} has no blocks", function.name),
        });
    }

    let mut ids: Vec<u32> = function.blocks.iter().map(|block| block.id).collect();
    ids.sort_unstable();
    ids.dedup();
    if ids.len() != function.blocks.len() {
        return Err(BytecodeError::Format {
            message: format!("function {} has duplicate block ids", function.name),
        });
    }
    for (expected, actual) in ids.iter().enumerate() {
        if *actual != expected as u32 {
            return Err(BytecodeError::Format {
                message: format!(
                    "function {} has non-contiguous block id {}",
                    function.name, actual
                ),
            });
        }
    }

    for block in &function.blocks {
        validate_block(block, function.locals as usize, const_pool_len, &ids)?;
    }
    Ok(())
}

fn validate_block(
    block: &BytecodeBlock,
    locals_len: usize,
    const_pool_len: usize,
    block_ids: &[u32],
) -> Result<(), BytecodeError> {
    for instr in &block.code {
        validate_instr(instr, locals_len, const_pool_len)?;
    }
    validate_terminator(&block.terminator, block_ids)
}

fn validate_instr(
    instr: &BytecodeInstr,
    locals_len: usize,
    const_pool_len: usize,
) -> Result<(), BytecodeError> {
    match instr {
        BytecodeInstr::LoadConst(id) => {
            if (*id as usize) >= const_pool_len {
                return Err(BytecodeError::Format {
                    message: format!("const id {} out of bounds", id),
                });
            }
        }
        BytecodeInstr::LoadLocal(local) | BytecodeInstr::StoreLocal(local) => {
            if (*local as usize) >= locals_len {
                return Err(BytecodeError::Format {
                    message: format!("local {} out of bounds", local),
                });
            }
        }
        BytecodeInstr::LoadPlace(place) | BytecodeInstr::StorePlace(place) => {
            validate_place(place, locals_len)?;
        }
        _ => {}
    }
    Ok(())
}

fn validate_place(place: &BytecodePlace, locals_len: usize) -> Result<(), BytecodeError> {
    if (place.local as usize) >= locals_len {
        return Err(BytecodeError::Format {
            message: format!("place local {} out of bounds", place.local),
        });
    }
    for elem in &place.projection {
        if let BytecodePlaceElem::Index(local) = elem {
            if (*local as usize) >= locals_len {
                return Err(BytecodeError::Format {
                    message: format!("place index local {} out of bounds", local),
                });
            }
        }
    }
    Ok(())
}

fn validate_terminator(
    terminator: &BytecodeTerminator,
    block_ids: &[u32],
) -> Result<(), BytecodeError> {
    let contains = |target: u32| block_ids.binary_search(&target).is_ok();
    match terminator {
        BytecodeTerminator::Return
        | BytecodeTerminator::Abort
        | BytecodeTerminator::Unreachable => Ok(()),
        BytecodeTerminator::Jump { target } => {
            if contains(*target) {
                Ok(())
            } else {
                Err(BytecodeError::Format {
                    message: format!("terminator target {} missing", target),
                })
            }
        }
        BytecodeTerminator::JumpIfTrue { target, otherwise }
        | BytecodeTerminator::JumpIfFalse { target, otherwise } => {
            if !contains(*target) {
                return Err(BytecodeError::Format {
                    message: format!("terminator target {} missing", target),
                });
            }
            if !contains(*otherwise) {
                return Err(BytecodeError::Format {
                    message: format!("terminator target {} missing", otherwise),
                });
            }
            Ok(())
        }
        BytecodeTerminator::SwitchInt {
            values,
            targets,
            otherwise,
        } => {
            if values.len() != targets.len() {
                return Err(BytecodeError::Format {
                    message: "switch targets length mismatch".to_string(),
                });
            }
            for target in targets.iter().chain(std::iter::once(otherwise)) {
                if !contains(*target) {
                    return Err(BytecodeError::Format {
                        message: format!("switch target {} missing", target),
                    });
                }
            }
            Ok(())
        }
        BytecodeTerminator::Call { target, .. } => {
            if contains(*target) {
                Ok(())
            } else {
                Err(BytecodeError::Format {
                    message: format!("call target {} missing", target),
                })
            }
        }
    }
}

pub fn format_program(program: &BytecodeProgram) -> String {
    let mut output = String::new();
    output.push_str("fp-bytecode {\n");
    output.push_str("  const_pool:\n");
    for (index, constant) in program.const_pool.iter().enumerate() {
        output.push_str(&format!("    [{}] {}\n", index, format_const(constant)));
    }
    output.push_str("  functions:\n");
    for function in &program.functions {
        output.push_str(&format!(
            "    fn {}(params: {}, locals: {})\n",
            function.name, function.params, function.locals
        ));
        for block in &function.blocks {
            output.push_str(&format!("      bb{}:\n", block.id));
            for instr in &block.code {
                output.push_str(&format!("        {}\n", format_instr(instr)));
            }
            output.push_str(&format!(
                "        terminator {}\n",
                format_terminator(&block.terminator)
            ));
        }
    }
    if let Some(entry) = &program.entry {
        output.push_str(&format!("  entry: {}\n", entry));
    }
    output.push_str("}\n");
    output
}

pub fn parse_program(text: &str) -> Result<BytecodeProgram, BytecodeError> {
    let mut input = text;
    let program = match parse_program_winnow.parse_next(&mut input) {
        Ok(program) => program,
        Err(err) => {
            return Err(BytecodeError::Format {
                message: format!("failed to parse text bytecode: {}", err),
            });
        }
    };
    let _ = ws0.parse_next(&mut input);
    if !input.trim().is_empty() {
        return Err(BytecodeError::Format {
            message: "trailing data after bytecode program".to_string(),
        });
    }
    validate_program(&program)?;
    Ok(program)
}

pub fn lower_program(program: &mir::Program) -> Result<BytecodeProgram, BytecodeError> {
    let mut const_pool = Vec::new();
    let mut functions = Vec::new();

    for item in &program.items {
        let function = match &item.kind {
            mir::ItemKind::Function(func) => func,
            mir::ItemKind::Static(_) => continue,
        };
        let body =
            program
                .bodies
                .get(&function.body_id)
                .ok_or_else(|| BytecodeError::Lowering {
                    message: format!("missing body for function {}", function.name.as_str()),
                })?;
        let lowered = lower_function(function, body, &mut const_pool)?;
        functions.push(lowered);
    }

    let entry = functions
        .iter()
        .find(|f| f.name == "main")
        .map(|f| f.name.clone());

    Ok(BytecodeProgram {
        const_pool,
        functions,
        entry,
    })
}

fn emit_lowering_warning(message: impl Into<String>) {
    diagnostic_manager().add_diagnostic(
        Diagnostic::warning(message.into()).with_source_context(BYTECODE_LOWERING_CONTEXT),
    );
}

fn push_dummy_unit(code: &mut Vec<BytecodeInstr>, const_pool: &mut Vec<BytecodeConst>) {
    let id = push_const(const_pool, BytecodeConst::Unit);
    code.push(BytecodeInstr::LoadConst(id));
}

fn parse_program_winnow(input: &mut &str) -> ModalResult<BytecodeProgram> {
    ws0.parse_next(input)?;
    literal("fp-bytecode").parse_next(input)?;
    ws0.parse_next(input)?;
    literal("{").parse_next(input)?;
    ws0.parse_next(input)?;
    literal("const_pool:").parse_next(input)?;
    consume_line_end(input);

    let mut const_pool = Vec::new();
    let mut functions = Vec::new();
    let mut entry = None;

    loop {
        ws0.parse_next(input)?;
        if input.is_empty() {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        if input.trim_start().starts_with("functions:") {
            literal("functions:").parse_next(input)?;
            consume_line_end(input);
            break;
        }
        let line = next_non_empty_line(input)?.ok_or(ErrMode::Cut(ContextError::new()))?;
        let (index, value) =
            parse_const_pool_entry_line(line).map_err(|_| ErrMode::Cut(ContextError::new()))?;
        if index != const_pool.len() as u32 {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        const_pool.push(value);
    }

    loop {
        ws0.parse_next(input)?;
        let Some(line) = next_non_empty_line(input)? else {
            return Err(ErrMode::Cut(ContextError::new()));
        };
        if line == "}" {
            break;
        }
        if let Some(rest) = line.strip_prefix("entry:") {
            let name = rest.trim();
            if name.is_empty() {
                return Err(ErrMode::Cut(ContextError::new()));
            }
            entry = Some(name.to_string());
            continue;
        }
        if line.starts_with("fn ") {
            let (name, params, locals) =
                parse_function_header_line(line).map_err(|_| ErrMode::Cut(ContextError::new()))?;
            let mut blocks = Vec::new();
            loop {
                ws0.parse_next(input)?;
                let Some(peek) = peek_next_non_empty_line(input) else {
                    return Err(ErrMode::Cut(ContextError::new()));
                };
                if peek.starts_with("fn ") || peek.starts_with("entry:") || peek == "}" {
                    break;
                }
                let block_line =
                    next_non_empty_line(input)?.ok_or(ErrMode::Cut(ContextError::new()))?;
                let block_id = parse_block_header_line(block_line)
                    .map_err(|_| ErrMode::Cut(ContextError::new()))?;
                let block = parse_block_winnow(input, block_id)
                    .map_err(|_| ErrMode::Cut(ContextError::new()))?;
                blocks.push(block);
            }

            functions.push(BytecodeFunction {
                name,
                params,
                locals,
                blocks,
            });
            continue;
        }
        return Err(ErrMode::Cut(ContextError::new()));
    }

    Ok(BytecodeProgram {
        const_pool,
        functions,
        entry,
    })
}

fn parse_const_pool_entry_line(line: &str) -> Result<(u32, BytecodeConst), BytecodeError> {
    let trimmed = line.trim();
    let Some(rest) = trimmed.strip_prefix('[') else {
        return Err(BytecodeError::Format {
            message: format!("invalid const pool entry: {}", line),
        });
    };
    let (index_part, value_part) = rest.split_once(']').ok_or_else(|| BytecodeError::Format {
        message: format!("invalid const pool entry: {}", line),
    })?;
    let index = index_part
        .trim()
        .parse::<u32>()
        .map_err(|_| BytecodeError::Format {
            message: format!("invalid const pool index: {}", line),
        })?;
    let value = parse_const_value(value_part.trim())?;
    Ok((index, value))
}

fn parse_function_header_line(line: &str) -> Result<(String, u32, u32), BytecodeError> {
    let trimmed = line.trim();
    let Some(rest) = trimmed.strip_prefix("fn ") else {
        return Err(BytecodeError::Format {
            message: format!("invalid function header: {}", line),
        });
    };
    let (name_part, tail) = rest.split_once('(').ok_or_else(|| BytecodeError::Format {
        message: format!("invalid function header: {}", line),
    })?;
    let name = name_part.trim();
    if name.is_empty() {
        return Err(BytecodeError::Format {
            message: format!("invalid function name: {}", line),
        });
    }
    let tail = tail.trim();
    let (tail, after) = tail.rsplit_once(')').ok_or_else(|| BytecodeError::Format {
        message: format!("invalid function header: {}", line),
    })?;
    if !after.trim().is_empty() {
        return Err(BytecodeError::Format {
            message: format!("invalid function header: {}", line),
        });
    }
    let tail = tail.trim();
    let mut params = None;
    let mut locals = None;
    for part in tail.split(',') {
        let part = part.trim();
        if let Some(value) = part.strip_prefix("params:") {
            params = Some(
                value
                    .trim()
                    .parse::<u32>()
                    .map_err(|_| BytecodeError::Format {
                        message: format!("invalid params count: {}", line),
                    })?,
            );
        } else if let Some(value) = part.strip_prefix("locals:") {
            locals = Some(
                value
                    .trim()
                    .parse::<u32>()
                    .map_err(|_| BytecodeError::Format {
                        message: format!("invalid locals count: {}", line),
                    })?,
            );
        }
    }
    let params = params.ok_or_else(|| BytecodeError::Format {
        message: format!("invalid function header: {}", line),
    })?;
    let locals = locals.ok_or_else(|| BytecodeError::Format {
        message: format!("invalid function header: {}", line),
    })?;
    Ok((name.to_string(), params, locals))
}

fn parse_block_header_line(line: &str) -> Result<u32, BytecodeError> {
    let trimmed = line.trim();
    let Some(rest) = trimmed.strip_prefix("bb") else {
        return Err(BytecodeError::Format {
            message: format!("invalid block header: {}", line),
        });
    };
    let rest = rest.trim_end_matches(':');
    let id = rest
        .trim()
        .parse::<u32>()
        .map_err(|_| BytecodeError::Format {
            message: format!("invalid block header: {}", line),
        })?;
    Ok(id)
}

fn parse_block_winnow(input: &mut &str, block_id: u32) -> Result<BytecodeBlock, BytecodeError> {
    let mut code = Vec::new();
    let terminator = loop {
        let line = next_line(input).map_err(|_| BytecodeError::Format {
            message: "unexpected end while parsing block".to_string(),
        })?;
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("terminator ") {
            break parse_terminator(rest.trim())?;
        }
        if trimmed.starts_with("bb")
            || trimmed.starts_with("fn ")
            || trimmed.starts_with("entry:")
            || trimmed == "}"
        {
            return Err(BytecodeError::Format {
                message: "block terminator missing".to_string(),
            });
        }
        code.push(parse_instr(trimmed)?);
    };

    Ok(BytecodeBlock {
        id: block_id,
        code,
        terminator,
    })
}

fn ws0(input: &mut &str) -> ModalResult<()> {
    take_while(0.., char::is_whitespace)
        .map(|_| ())
        .parse_next(input)
}

fn next_line<'a>(input: &mut &'a str) -> ModalResult<&'a str> {
    let line = take_till(0.., |ch: char| ch == '\n' || ch == '\r').parse_next(input)?;
    consume_line_end(input);
    Ok(line)
}

fn next_non_empty_line<'a>(input: &mut &'a str) -> ModalResult<Option<&'a str>> {
    loop {
        if input.is_empty() {
            return Ok(None);
        }
        let line = next_line(input)?;
        let trimmed = line.trim();
        if !trimmed.is_empty() {
            return Ok(Some(trimmed));
        }
    }
}

fn peek_next_non_empty_line(input: &str) -> Option<&str> {
    input
        .lines()
        .map(|line| line.trim())
        .find(|line| !line.is_empty())
}

fn consume_line_end(input: &mut &str) {
    if input.starts_with("\r\n") {
        *input = &input[2..];
    } else if input.starts_with('\n') || input.starts_with('\r') {
        *input = &input[1..];
    }
}

fn parse_instr(line: &str) -> Result<BytecodeInstr, BytecodeError> {
    if let Some(rest) = line.strip_prefix("load.const ") {
        return Ok(BytecodeInstr::LoadConst(parse_u32(rest)?));
    }
    if let Some(rest) = line.strip_prefix("load.local ") {
        return Ok(BytecodeInstr::LoadLocal(parse_u32(rest)?));
    }
    if let Some(rest) = line.strip_prefix("store.local ") {
        return Ok(BytecodeInstr::StoreLocal(parse_u32(rest)?));
    }
    if let Some(rest) = line.strip_prefix("load.place ") {
        return Ok(BytecodeInstr::LoadPlace(parse_place(rest)?));
    }
    if let Some(rest) = line.strip_prefix("store.place ") {
        return Ok(BytecodeInstr::StorePlace(parse_place(rest)?));
    }
    if let Some(rest) = line.strip_prefix("binop ") {
        return Ok(BytecodeInstr::BinaryOp(parse_binop(rest)?));
    }
    if let Some(rest) = line.strip_prefix("unop ") {
        return Ok(BytecodeInstr::UnaryOp(parse_unop(rest)?));
    }
    if let Some(rest) = line.strip_prefix("intrinsic ") {
        return parse_intrinsic(rest);
    }
    if let Some(rest) = line.strip_prefix("make.tuple ") {
        return Ok(BytecodeInstr::MakeTuple(parse_u32(rest)?));
    }
    if let Some(rest) = line.strip_prefix("make.array ") {
        return Ok(BytecodeInstr::MakeArray(parse_u32(rest)?));
    }
    if let Some(rest) = line.strip_prefix("make.list ") {
        return Ok(BytecodeInstr::MakeList(parse_u32(rest)?));
    }
    if let Some(rest) = line.strip_prefix("make.map ") {
        return Ok(BytecodeInstr::MakeMap(parse_u32(rest)?));
    }
    if line == "container.get" {
        return Ok(BytecodeInstr::ContainerGet);
    }
    if line == "container.len" {
        return Ok(BytecodeInstr::ContainerLen);
    }
    if line == "pop" {
        return Ok(BytecodeInstr::Pop);
    }

    Err(BytecodeError::Format {
        message: format!("unknown instruction: {}", line),
    })
}

fn parse_terminator(line: &str) -> Result<BytecodeTerminator, BytecodeError> {
    if line == "return" {
        return Ok(BytecodeTerminator::Return);
    }
    if let Some(rest) = line.strip_prefix("jump bb") {
        return Ok(BytecodeTerminator::Jump {
            target: parse_u32(rest)?,
        });
    }
    if let Some(rest) = line.strip_prefix("jump_if_true bb") {
        let (target, otherwise) = parse_jump_pair(rest)?;
        return Ok(BytecodeTerminator::JumpIfTrue { target, otherwise });
    }
    if let Some(rest) = line.strip_prefix("jump_if_false bb") {
        let (target, otherwise) = parse_jump_pair(rest)?;
        return Ok(BytecodeTerminator::JumpIfFalse { target, otherwise });
    }
    if let Some(rest) = line.strip_prefix("switch ") {
        return parse_switch(rest);
    }
    if let Some(rest) = line.strip_prefix("call ") {
        return parse_call(rest);
    }
    if line == "abort" {
        return Ok(BytecodeTerminator::Abort);
    }
    if line == "unreachable" {
        return Ok(BytecodeTerminator::Unreachable);
    }

    Err(BytecodeError::Format {
        message: format!("unknown terminator: {}", line),
    })
}

fn parse_jump_pair(rest: &str) -> Result<(u32, u32), BytecodeError> {
    let (target_part, otherwise_part) =
        rest.split_once(" else bb")
            .ok_or_else(|| BytecodeError::Format {
                message: format!("invalid jump format: {}", rest),
            })?;
    let target = parse_u32(target_part)?;
    let otherwise = parse_u32(otherwise_part)?;
    Ok((target, otherwise))
}

fn parse_switch(rest: &str) -> Result<BytecodeTerminator, BytecodeError> {
    let (list_part, otherwise_part) = rest
        .strip_prefix('[')
        .and_then(|s| s.split_once("] otherwise bb"))
        .ok_or_else(|| BytecodeError::Format {
            message: format!("invalid switch format: {}", rest),
        })?;
    let mut values = Vec::new();
    let mut targets = Vec::new();
    for entry in split_top_level(list_part) {
        let entry = entry.trim();
        if entry.is_empty() {
            continue;
        }
        let (value, target) = entry
            .split_once(":bb")
            .ok_or_else(|| BytecodeError::Format {
                message: format!("invalid switch entry: {}", entry),
            })?;
        let value = value
            .trim()
            .parse::<u128>()
            .map_err(|_| BytecodeError::Format {
                message: format!("invalid switch value: {}", value),
            })?;
        let target = parse_u32(target)?;
        values.push(value);
        targets.push(target);
    }
    let otherwise = parse_u32(otherwise_part)?;
    Ok(BytecodeTerminator::SwitchInt {
        values,
        targets,
        otherwise,
    })
}

fn parse_call(rest: &str) -> Result<BytecodeTerminator, BytecodeError> {
    let (before_arrow, after_arrow) =
        rest.split_once(" -> ")
            .ok_or_else(|| BytecodeError::Format {
                message: format!("invalid call format: {}", rest),
            })?;
    let (dest_part, target_part) =
        after_arrow
            .split_once(" then bb")
            .ok_or_else(|| BytecodeError::Format {
                message: format!("invalid call format: {}", rest),
            })?;
    let (callee_part, arg_count_part) =
        before_arrow
            .rsplit_once(' ')
            .ok_or_else(|| BytecodeError::Format {
                message: format!("invalid call format: {}", rest),
            })?;
    let callee = parse_callee(callee_part.trim())?;
    let arg_count = parse_u32(arg_count_part.trim())?;
    let destination = if dest_part.trim() == "_" {
        None
    } else {
        Some(parse_place(dest_part.trim())?)
    };
    let target = parse_u32(target_part.trim())?;
    Ok(BytecodeTerminator::Call {
        callee,
        arg_count,
        destination,
        target,
    })
}

fn parse_intrinsic(rest: &str) -> Result<BytecodeInstr, BytecodeError> {
    let mut parts = rest.splitn(3, ' ');
    let kind_part = parts.next().ok_or_else(|| BytecodeError::Format {
        message: format!("invalid intrinsic: {}", rest),
    })?;
    let count_part = parts.next().ok_or_else(|| BytecodeError::Format {
        message: format!("invalid intrinsic: {}", rest),
    })?;
    let format_part = parts.next().map(str::trim);

    let kind = parse_intrinsic_kind(kind_part)?;
    let arg_count = parse_u32(count_part)?;
    let format = match format_part {
        Some(raw) if !raw.is_empty() => {
            let (value, rest) = parse_debug_string(raw)?;
            if !rest.trim().is_empty() {
                return Err(BytecodeError::Format {
                    message: format!("invalid intrinsic format: {}", rest),
                });
            }
            Some(value)
        }
        _ => None,
    };

    Ok(BytecodeInstr::IntrinsicCall {
        kind,
        arg_count,
        format,
    })
}

fn parse_intrinsic_kind(raw: &str) -> Result<IntrinsicCallKind, BytecodeError> {
    match raw {
        "Println" => Ok(IntrinsicCallKind::Println),
        "Print" => Ok(IntrinsicCallKind::Print),
        "Format" => Ok(IntrinsicCallKind::Format),
        "Len" => Ok(IntrinsicCallKind::Len),
        "DebugAssertions" => Ok(IntrinsicCallKind::DebugAssertions),
        "Input" => Ok(IntrinsicCallKind::Input),
        "Panic" => Ok(IntrinsicCallKind::Panic),
        "CatchUnwind" => Ok(IntrinsicCallKind::CatchUnwind),
        "SizeOf" => Ok(IntrinsicCallKind::SizeOf),
        "ReflectFields" => Ok(IntrinsicCallKind::ReflectFields),
        "HasMethod" => Ok(IntrinsicCallKind::HasMethod),
        "TypeName" => Ok(IntrinsicCallKind::TypeName),
        "TypeOf" => Ok(IntrinsicCallKind::TypeOf),
        "CreateStruct" => Ok(IntrinsicCallKind::CreateStruct),
        "CloneStruct" => Ok(IntrinsicCallKind::CloneStruct),
        "AddField" => Ok(IntrinsicCallKind::AddField),
        "HasField" => Ok(IntrinsicCallKind::HasField),
        "FieldCount" => Ok(IntrinsicCallKind::FieldCount),
        "MethodCount" => Ok(IntrinsicCallKind::MethodCount),
        "FieldType" => Ok(IntrinsicCallKind::FieldType),
        "StructSize" => Ok(IntrinsicCallKind::StructSize),
        "GenerateMethod" => Ok(IntrinsicCallKind::GenerateMethod),
        "CompileError" => Ok(IntrinsicCallKind::CompileError),
        "CompileWarning" => Ok(IntrinsicCallKind::CompileWarning),
        _ => Err(BytecodeError::Format {
            message: format!("unknown intrinsic kind: {}", raw),
        }),
    }
}

fn parse_binop(raw: &str) -> Result<BytecodeBinOp, BytecodeError> {
    match raw {
        "Add" => Ok(BytecodeBinOp::Add),
        "Sub" => Ok(BytecodeBinOp::Sub),
        "Mul" => Ok(BytecodeBinOp::Mul),
        "Div" => Ok(BytecodeBinOp::Div),
        "Rem" => Ok(BytecodeBinOp::Rem),
        "And" => Ok(BytecodeBinOp::And),
        "Or" => Ok(BytecodeBinOp::Or),
        "BitXor" => Ok(BytecodeBinOp::BitXor),
        "BitAnd" => Ok(BytecodeBinOp::BitAnd),
        "BitOr" => Ok(BytecodeBinOp::BitOr),
        "Shl" => Ok(BytecodeBinOp::Shl),
        "Shr" => Ok(BytecodeBinOp::Shr),
        "Eq" => Ok(BytecodeBinOp::Eq),
        "Lt" => Ok(BytecodeBinOp::Lt),
        "Le" => Ok(BytecodeBinOp::Le),
        "Ne" => Ok(BytecodeBinOp::Ne),
        "Ge" => Ok(BytecodeBinOp::Ge),
        "Gt" => Ok(BytecodeBinOp::Gt),
        _ => Err(BytecodeError::Format {
            message: format!("unknown binop: {}", raw),
        }),
    }
}

fn parse_unop(raw: &str) -> Result<BytecodeUnOp, BytecodeError> {
    match raw {
        "Not" => Ok(BytecodeUnOp::Not),
        "Neg" => Ok(BytecodeUnOp::Neg),
        _ => Err(BytecodeError::Format {
            message: format!("unknown unop: {}", raw),
        }),
    }
}

fn parse_place(raw: &str) -> Result<BytecodePlace, BytecodeError> {
    let mut chars = raw.trim().chars().peekable();
    if chars.next() != Some('_') {
        return Err(BytecodeError::Format {
            message: format!("invalid place: {}", raw),
        });
    }
    let local = parse_number_token(&mut chars)?;
    let mut projection = Vec::new();
    while let Some(ch) = chars.peek().copied() {
        match ch {
            '.' => {
                chars.next();
                let field = parse_number_token(&mut chars)?;
                projection.push(BytecodePlaceElem::Field(field));
            }
            '[' => {
                chars.next();
                if chars.next() != Some('_') {
                    return Err(BytecodeError::Format {
                        message: format!("invalid index projection: {}", raw),
                    });
                }
                let index = parse_number_token(&mut chars)?;
                if chars.next() != Some(']') {
                    return Err(BytecodeError::Format {
                        message: format!("unterminated index projection: {}", raw),
                    });
                }
                projection.push(BytecodePlaceElem::Index(index));
            }
            _ => {
                return Err(BytecodeError::Format {
                    message: format!("invalid place projection: {}", raw),
                });
            }
        }
    }
    Ok(BytecodePlace { local, projection })
}

fn parse_callee(raw: &str) -> Result<BytecodeCallee, BytecodeError> {
    let raw = raw.trim();
    if let Some(rest) = raw.strip_prefix("fn ") {
        let name = rest.trim();
        if name.is_empty() {
            return Err(BytecodeError::Format {
                message: format!("invalid function callee: {}", raw),
            });
        }
        return Ok(BytecodeCallee::Function(name.to_string()));
    }
    if let Some(rest) = raw.strip_prefix("local ") {
        let place = parse_place(rest.trim())?;
        return Ok(BytecodeCallee::Local(place));
    }
    parse_callee_debug(raw)
}

fn parse_callee_debug(raw: &str) -> Result<BytecodeCallee, BytecodeError> {
    if let Some(inner) = raw
        .strip_prefix("Function(")
        .and_then(|s| s.strip_suffix(')'))
    {
        let (value, rest) = parse_debug_string(inner.trim())?;
        if !rest.trim().is_empty() {
            return Err(BytecodeError::Format {
                message: format!("invalid function callee: {}", raw),
            });
        }
        return Ok(BytecodeCallee::Function(value));
    }
    if let Some(inner) = raw.strip_prefix("Local(").and_then(|s| s.strip_suffix(')')) {
        let inner = inner.trim();
        let local_prefix = "BytecodePlace { local: ";
        let projection_prefix = ", projection: ";
        let local_start =
            inner
                .strip_prefix(local_prefix)
                .ok_or_else(|| BytecodeError::Format {
                    message: format!("invalid local callee: {}", raw),
                })?;
        let (local_part, rest) =
            local_start
                .split_once(projection_prefix)
                .ok_or_else(|| BytecodeError::Format {
                    message: format!("invalid local callee: {}", raw),
                })?;
        let local = local_part
            .trim()
            .parse::<u32>()
            .map_err(|_| BytecodeError::Format {
                message: format!("invalid local index: {}", local_part),
            })?;
        let rest = rest.trim();
        let projections = rest
            .strip_prefix('[')
            .and_then(|s| s.strip_suffix("] }"))
            .ok_or_else(|| BytecodeError::Format {
                message: format!("invalid local projection: {}", raw),
            })?;
        let mut projection = Vec::new();
        for part in split_top_level(projections) {
            let part = part.trim();
            if part.is_empty() {
                continue;
            }
            if let Some(inner) = part
                .strip_prefix("Field(")
                .and_then(|s| s.strip_suffix(')'))
            {
                let index = inner
                    .trim()
                    .parse::<u32>()
                    .map_err(|_| BytecodeError::Format {
                        message: format!("invalid field index: {}", part),
                    })?;
                projection.push(BytecodePlaceElem::Field(index));
            } else if let Some(inner) = part
                .strip_prefix("Index(")
                .and_then(|s| s.strip_suffix(')'))
            {
                let index = inner
                    .trim()
                    .parse::<u32>()
                    .map_err(|_| BytecodeError::Format {
                        message: format!("invalid index projection: {}", part),
                    })?;
                projection.push(BytecodePlaceElem::Index(index));
            } else {
                return Err(BytecodeError::Format {
                    message: format!("invalid projection element: {}", part),
                });
            }
        }
        return Ok(BytecodeCallee::Local(BytecodePlace { local, projection }));
    }

    Err(BytecodeError::Format {
        message: format!("unknown callee: {}", raw),
    })
}

fn parse_const_value(raw: &str) -> Result<BytecodeConst, BytecodeError> {
    let raw = raw.trim();
    if let Some(rest) = raw.strip_prefix("u64 ") {
        let value = rest
            .trim()
            .parse::<u64>()
            .map_err(|_| BytecodeError::Format {
                message: format!("invalid u64 constant: {}", raw),
            })?;
        return Ok(BytecodeConst::UInt(value));
    }
    if let Some(rest) = raw.strip_prefix("i64 ") {
        let value = rest
            .trim()
            .parse::<i64>()
            .map_err(|_| BytecodeError::Format {
                message: format!("invalid i64 constant: {}", raw),
            })?;
        return Ok(BytecodeConst::Int(value));
    }
    if let Some(rest) = raw.strip_prefix("f64 ") {
        let value = rest
            .trim()
            .parse::<f64>()
            .map_err(|_| BytecodeError::Format {
                message: format!("invalid f64 constant: {}", raw),
            })?;
        return Ok(BytecodeConst::Float(value));
    }
    if raw == "()" {
        return Ok(BytecodeConst::Unit);
    }
    if raw == "true" {
        return Ok(BytecodeConst::Bool(true));
    }
    if raw == "false" {
        return Ok(BytecodeConst::Bool(false));
    }
    if raw == "null" {
        return Ok(BytecodeConst::Null);
    }
    if let Some(rest) = raw.strip_prefix("fn ") {
        return Ok(BytecodeConst::Function(rest.trim().to_string()));
    }
    if raw.starts_with('"') {
        let (value, rest) = parse_debug_string(raw)?;
        if !rest.trim().is_empty() {
            return Err(BytecodeError::Format {
                message: format!("invalid string const: {}", raw),
            });
        }
        return Ok(BytecodeConst::Str(value));
    }
    if let Some(rest) = raw.strip_prefix("tuple") {
        let rest = rest.trim_start();
        let items = parse_const_list(rest)?;
        return Ok(BytecodeConst::Tuple(items));
    }
    if let Some(rest) = raw.strip_prefix("array") {
        let rest = rest.trim_start();
        let items = parse_const_list(rest)?;
        return Ok(BytecodeConst::Array(items));
    }
    if let Some(rest) = raw.strip_prefix("list") {
        let rest = rest.trim_start();
        let items = parse_const_list(rest)?;
        return Ok(BytecodeConst::List(items));
    }
    if let Some(rest) = raw.strip_prefix("map") {
        let rest = rest.trim_start();
        let entries = parse_map_entries(rest)?;
        return Ok(BytecodeConst::Map(entries));
    }
    if let Ok(value) = raw.parse::<i64>() {
        return Ok(BytecodeConst::Int(value));
    }
    if let Ok(value) = raw.parse::<u64>() {
        if value > i64::MAX as u64 {
            return Ok(BytecodeConst::UInt(value));
        }
        return Ok(BytecodeConst::Int(value as i64));
    }
    if let Ok(value) = raw.parse::<f64>() {
        return Ok(BytecodeConst::Float(value));
    }
    Err(BytecodeError::Format {
        message: format!("invalid constant: {}", raw),
    })
}

fn parse_const_list(raw: &str) -> Result<Vec<BytecodeConst>, BytecodeError> {
    let content = raw
        .trim()
        .strip_prefix('[')
        .and_then(|s| s.strip_suffix(']'))
        .ok_or_else(|| BytecodeError::Format {
            message: format!("invalid list constant: {}", raw),
        })?;
    let mut items = Vec::new();
    for entry in split_top_level(content) {
        let entry = entry.trim();
        if entry.is_empty() {
            continue;
        }
        items.push(parse_const_value(entry)?);
    }
    Ok(items)
}

fn parse_map_entries(raw: &str) -> Result<Vec<(BytecodeConst, BytecodeConst)>, BytecodeError> {
    let content = raw
        .trim()
        .strip_prefix('[')
        .and_then(|s| s.strip_suffix(']'))
        .ok_or_else(|| BytecodeError::Format {
            message: format!("invalid map constant: {}", raw),
        })?;
    let mut entries = Vec::new();
    for entry in split_top_level(content) {
        let entry = entry.trim();
        if entry.is_empty() {
            continue;
        }
        let (key, value) =
            split_once_top_level(entry, "=>").ok_or_else(|| BytecodeError::Format {
                message: format!("invalid map entry: {}", entry),
            })?;
        entries.push((
            parse_const_value(key.trim())?,
            parse_const_value(value.trim())?,
        ));
    }
    Ok(entries)
}

fn split_top_level(input: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut start = 0;
    let mut depth = 0usize;
    let mut in_string = false;
    let mut escape = false;
    for (idx, ch) in input.char_indices() {
        if in_string {
            if escape {
                escape = false;
            } else if ch == '\\' {
                escape = true;
            } else if ch == '"' {
                in_string = false;
            }
            continue;
        }
        match ch {
            '"' => in_string = true,
            '[' => depth += 1,
            ']' => depth = depth.saturating_sub(1),
            ',' if depth == 0 => {
                parts.push(input[start..idx].trim());
                start = idx + 1;
            }
            _ => {}
        }
    }
    if start <= input.len() {
        parts.push(input[start..].trim());
    }
    parts
}

fn split_once_top_level<'a>(input: &'a str, needle: &str) -> Option<(&'a str, &'a str)> {
    let mut depth = 0usize;
    let mut in_string = false;
    let mut escape = false;
    let bytes = input.as_bytes();
    let needle_bytes = needle.as_bytes();
    let mut i = 0;
    while i + needle_bytes.len() <= bytes.len() {
        let ch = bytes[i] as char;
        if in_string {
            if escape {
                escape = false;
            } else if ch == '\\' {
                escape = true;
            } else if ch == '"' {
                in_string = false;
            }
            i += 1;
            continue;
        }
        match ch {
            '"' => in_string = true,
            '[' => depth += 1,
            ']' => depth = depth.saturating_sub(1),
            _ => {}
        }
        if depth == 0 && bytes[i..].starts_with(needle_bytes) {
            return Some((&input[..i], &input[i + needle_bytes.len()..]));
        }
        i += 1;
    }
    None
}

fn parse_u32(raw: &str) -> Result<u32, BytecodeError> {
    raw.trim()
        .parse::<u32>()
        .map_err(|_| BytecodeError::Format {
            message: format!("invalid number: {}", raw),
        })
}

fn parse_number_token(
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
) -> Result<u32, BytecodeError> {
    let mut digits = String::new();
    while let Some(ch) = chars.peek().copied() {
        if ch.is_ascii_digit() {
            digits.push(ch);
            chars.next();
        } else {
            break;
        }
    }
    if digits.is_empty() {
        return Err(BytecodeError::Format {
            message: "missing number".to_string(),
        });
    }
    digits.parse::<u32>().map_err(|_| BytecodeError::Format {
        message: format!("invalid number: {}", digits),
    })
}

fn parse_debug_string(raw: &str) -> Result<(String, &str), BytecodeError> {
    let mut chars = raw.char_indices().peekable();
    match chars.next() {
        Some((_, '"')) => {}
        _ => {
            return Err(BytecodeError::Format {
                message: format!("expected string literal: {}", raw),
            });
        }
    }
    let mut output = String::new();
    while let Some((idx, ch)) = chars.next() {
        match ch {
            '"' => {
                let rest = &raw[idx + 1..];
                return Ok((output, rest));
            }
            '\\' => {
                let Some((_, escaped)) = chars.next() else {
                    return Err(BytecodeError::Format {
                        message: "unterminated escape sequence".to_string(),
                    });
                };
                match escaped {
                    '\\' => output.push('\\'),
                    '"' => output.push('"'),
                    'n' => output.push('\n'),
                    'r' => output.push('\r'),
                    't' => output.push('\t'),
                    '0' => output.push('\0'),
                    'u' => {
                        let Some((_, '{')) = chars.next() else {
                            return Err(BytecodeError::Format {
                                message: "invalid unicode escape".to_string(),
                            });
                        };
                        let mut hex = String::new();
                        while let Some((_, ch)) = chars.next() {
                            if ch == '}' {
                                break;
                            }
                            hex.push(ch);
                        }
                        let value =
                            u32::from_str_radix(&hex, 16).map_err(|_| BytecodeError::Format {
                                message: format!("invalid unicode escape: {}", hex),
                            })?;
                        if let Some(ch) = char::from_u32(value) {
                            output.push(ch);
                        } else {
                            return Err(BytecodeError::Format {
                                message: format!("invalid unicode scalar: {}", hex),
                            });
                        }
                    }
                    other => {
                        return Err(BytecodeError::Format {
                            message: format!("unsupported escape: \\{}", other),
                        });
                    }
                }
            }
            other => output.push(other),
        }
    }
    Err(BytecodeError::Format {
        message: "unterminated string literal".to_string(),
    })
}

fn lower_function(
    func: &mir::Function,
    body: &mir::Body,
    const_pool: &mut Vec<BytecodeConst>,
) -> Result<BytecodeFunction, BytecodeError> {
    let mut blocks = Vec::new();
    for (block_id, block) in body.basic_blocks.iter().enumerate() {
        let mut code = Vec::new();
        for stmt in &block.statements {
            lower_statement(stmt, &mut code, const_pool)?;
        }
        let lowered_term = match block.terminator.as_ref() {
            Some(terminator) => lower_terminator(terminator, &mut code, const_pool)?,
            None => {
                // Treat missing terminators as implicit returns to avoid executing
                // incomplete control flow graphs.
                BytecodeTerminator::Return
            }
        };
        blocks.push(BytecodeBlock {
            id: block_id as u32,
            code,
            terminator: lowered_term,
        });
    }

    Ok(BytecodeFunction {
        name: func.name.as_str().to_string(),
        params: func.sig.inputs.len() as u32,
        locals: body.locals.len() as u32,
        blocks,
    })
}

fn lower_statement(
    stmt: &mir::Statement,
    code: &mut Vec<BytecodeInstr>,
    const_pool: &mut Vec<BytecodeConst>,
) -> Result<(), BytecodeError> {
    match &stmt.kind {
        mir::StatementKind::Assign(place, rvalue) => {
            lower_rvalue(rvalue, code, const_pool)?;
            code.push(BytecodeInstr::StorePlace(lower_place(place)?));
            Ok(())
        }
        mir::StatementKind::IntrinsicCall { kind, format, args } => {
            for arg in args {
                lower_operand(arg, code, const_pool)?;
            }
            code.push(BytecodeInstr::IntrinsicCall {
                kind: *kind,
                arg_count: args.len() as u32,
                format: if format.is_empty() {
                    None
                } else {
                    Some(format.clone())
                },
            });
            Ok(())
        }
        mir::StatementKind::StorageLive(_)
        | mir::StatementKind::StorageDead(_)
        | mir::StatementKind::Retag(_, _)
        | mir::StatementKind::AscribeUserType(_, _, _)
        | mir::StatementKind::Nop
        | mir::StatementKind::SetDiscriminant { .. } => Ok(()),
    }
}

fn lower_terminator(
    term: &mir::Terminator,
    code: &mut Vec<BytecodeInstr>,
    const_pool: &mut Vec<BytecodeConst>,
) -> Result<BytecodeTerminator, BytecodeError> {
    match &term.kind {
        mir::TerminatorKind::Return => Ok(BytecodeTerminator::Return),
        mir::TerminatorKind::Goto { target } => Ok(BytecodeTerminator::Jump { target: *target }),
        mir::TerminatorKind::Assert {
            cond,
            expected,
            target,
            ..
        } => {
            lower_operand(cond, code, const_pool)?;
            let otherwise = terminator_otherwise(term)?;
            let terminator = if *expected {
                BytecodeTerminator::JumpIfTrue {
                    target: *target,
                    otherwise,
                }
            } else {
                BytecodeTerminator::JumpIfFalse {
                    target: *target,
                    otherwise,
                }
            };
            Ok(terminator)
        }
        mir::TerminatorKind::SwitchInt { discr, targets, .. } => {
            lower_operand(discr, code, const_pool)?;
            Ok(BytecodeTerminator::SwitchInt {
                values: targets.values.clone(),
                targets: targets.targets.clone(),
                otherwise: targets.otherwise,
            })
        }
        mir::TerminatorKind::Call {
            func,
            args,
            destination,
            ..
        } => {
            for arg in args {
                lower_operand(arg, code, const_pool)?;
            }
            let callee = lower_callee(func)?;
            let dest = destination
                .as_ref()
                .map(|(place, _)| lower_place(place))
                .transpose()?;
            let target =
                destination
                    .as_ref()
                    .map(|(_, bb)| *bb)
                    .ok_or_else(|| BytecodeError::Lowering {
                        message: "call terminator missing destination".to_string(),
                    })?;
            Ok(BytecodeTerminator::Call {
                callee,
                arg_count: args.len() as u32,
                destination: dest,
                target,
            })
        }
        mir::TerminatorKind::FalseEdge {
            real_target,
            imaginary_target,
        } => Ok(BytecodeTerminator::JumpIfTrue {
            target: *real_target,
            otherwise: *imaginary_target,
        }),
        mir::TerminatorKind::FalseUnwind { real_target, .. } => {
            Ok(BytecodeTerminator::JumpIfTrue {
                target: *real_target,
                otherwise: *real_target,
            })
        }
        mir::TerminatorKind::Abort => Ok(BytecodeTerminator::Abort),
        mir::TerminatorKind::Unreachable => Ok(BytecodeTerminator::Unreachable),
        _ => Err(BytecodeError::Lowering {
            message: format!("unsupported terminator: {:?}", term.kind),
        }),
    }
}

fn terminator_otherwise(term: &mir::Terminator) -> Result<u32, BytecodeError> {
    match &term.kind {
        mir::TerminatorKind::Assert {
            cleanup, target, ..
        } => match cleanup {
            Some(otherwise) => Ok(*otherwise),
            None => Err(BytecodeError::Lowering {
                message: format!("missing cleanup target for assert to bb{}", target),
            }),
        },
        _ => Err(BytecodeError::Lowering {
            message: "terminator_otherwise expects assert terminator".to_string(),
        }),
    }
}

fn lower_rvalue(
    rvalue: &mir::Rvalue,
    code: &mut Vec<BytecodeInstr>,
    const_pool: &mut Vec<BytecodeConst>,
) -> Result<(), BytecodeError> {
    match rvalue {
        mir::Rvalue::Use(op) => lower_operand(op, code, const_pool),
        mir::Rvalue::Ref(_, _, place) => {
            lower_operand(&mir::Operand::Copy(place.clone()), code, const_pool)
        }
        mir::Rvalue::BinaryOp(op, lhs, rhs) => {
            lower_operand(lhs, code, const_pool)?;
            lower_operand(rhs, code, const_pool)?;
            code.push(BytecodeInstr::BinaryOp(lower_binop(op)?));
            Ok(())
        }
        mir::Rvalue::UnaryOp(op, value) => {
            lower_operand(value, code, const_pool)?;
            code.push(BytecodeInstr::UnaryOp(lower_unop(op)?));
            Ok(())
        }
        mir::Rvalue::Cast(_, operand, _) => lower_operand(operand, code, const_pool),
        mir::Rvalue::IntrinsicCall { kind, format, args } => {
            for arg in args {
                lower_operand(arg, code, const_pool)?;
            }
            code.push(BytecodeInstr::IntrinsicCall {
                kind: *kind,
                arg_count: args.len() as u32,
                format: if format.is_empty() {
                    None
                } else {
                    Some(format.clone())
                },
            });
            Ok(())
        }
        mir::Rvalue::Repeat(operand, len) => {
            if *len > u32::MAX as u64 {
                return Err(BytecodeError::Lowering {
                    message: format!("repeat length {} exceeds bytecode limits", len),
                });
            }
            for _ in 0..*len {
                lower_operand(operand, code, const_pool)?;
            }
            code.push(BytecodeInstr::MakeArray(*len as u32));
            Ok(())
        }
        mir::Rvalue::Aggregate(kind, operands) => {
            for op in operands {
                lower_operand(op, code, const_pool)?;
            }
            match kind {
                mir::AggregateKind::Tuple => {
                    code.push(BytecodeInstr::MakeTuple(operands.len() as u32));
                    Ok(())
                }
                mir::AggregateKind::Array(_) => {
                    code.push(BytecodeInstr::MakeArray(operands.len() as u32));
                    Ok(())
                }
                _ => Err(BytecodeError::Lowering {
                    message: format!("unsupported aggregate: {:?}", kind),
                }),
            }
        }
        mir::Rvalue::ContainerLiteral { kind, elements } => {
            for op in elements {
                lower_operand(op, code, const_pool)?;
            }
            match kind {
                mir::ContainerKind::List { .. } => {
                    code.push(BytecodeInstr::MakeList(elements.len() as u32));
                    Ok(())
                }
                _ => Err(BytecodeError::Lowering {
                    message: format!("unsupported container literal: {:?}", kind),
                }),
            }
        }
        mir::Rvalue::ContainerMapLiteral { kind, entries } => {
            for (key, value) in entries {
                lower_operand(key, code, const_pool)?;
                lower_operand(value, code, const_pool)?;
            }
            match kind {
                mir::ContainerKind::Map { .. } => {
                    code.push(BytecodeInstr::MakeMap(entries.len() as u32));
                    Ok(())
                }
                _ => Err(BytecodeError::Lowering {
                    message: format!("unsupported container map literal: {:?}", kind),
                }),
            }
        }
        mir::Rvalue::ContainerLen { container, .. } => {
            lower_operand(container, code, const_pool)?;
            code.push(BytecodeInstr::ContainerLen);
            Ok(())
        }
        mir::Rvalue::ContainerGet { container, key, .. } => {
            lower_operand(container, code, const_pool)?;
            lower_operand(key, code, const_pool)?;
            code.push(BytecodeInstr::ContainerGet);
            Ok(())
        }
        _ => Err(BytecodeError::Lowering {
            message: format!("unsupported rvalue: {:?}", rvalue),
        }),
    }
}

fn lower_operand(
    operand: &mir::Operand,
    code: &mut Vec<BytecodeInstr>,
    const_pool: &mut Vec<BytecodeConst>,
) -> Result<(), BytecodeError> {
    match operand {
        mir::Operand::Copy(place) | mir::Operand::Move(place) => {
            code.push(BytecodeInstr::LoadPlace(lower_place(place)?));
            Ok(())
        }
        mir::Operand::Constant(constant) => {
            let value = lower_constant(constant)?;
            let id = push_const(const_pool, value);
            code.push(BytecodeInstr::LoadConst(id));
            Ok(())
        }
    }
}

fn lower_constant(constant: &mir::Constant) -> Result<BytecodeConst, BytecodeError> {
    match &constant.literal {
        mir::ConstantKind::Null => Ok(BytecodeConst::Null),
        mir::ConstantKind::Int(value) => Ok(BytecodeConst::Int(*value)),
        mir::ConstantKind::UInt(value) => Ok(BytecodeConst::UInt(*value)),
        mir::ConstantKind::Float(value) => Ok(BytecodeConst::Float(*value)),
        mir::ConstantKind::Bool(value) => Ok(BytecodeConst::Bool(*value)),
        mir::ConstantKind::Str(value) => Ok(BytecodeConst::Str(value.clone())),
        mir::ConstantKind::Fn(symbol, _) => {
            Ok(BytecodeConst::Function(symbol.as_str().to_string()))
        }
        mir::ConstantKind::Global(symbol, _) => {
            Ok(BytecodeConst::Function(symbol.as_str().to_string()))
        }
        mir::ConstantKind::Val(value, _) => lower_const_value(value),
        mir::ConstantKind::Ty(_) => Err(BytecodeError::Lowering {
            message: format!("unsupported constant: {:?}", constant.literal),
        }),
    }
}

fn lower_const_value(value: &mir::ConstValue) -> Result<BytecodeConst, BytecodeError> {
    match value {
        mir::ConstValue::Unit => Ok(BytecodeConst::Unit),
        mir::ConstValue::Bool(value) => Ok(BytecodeConst::Bool(*value)),
        mir::ConstValue::Int(value) => Ok(BytecodeConst::Int(*value)),
        mir::ConstValue::UInt(value) => Ok(BytecodeConst::UInt(*value)),
        mir::ConstValue::Float(value) => Ok(BytecodeConst::Float(*value)),
        mir::ConstValue::Str(value) => Ok(BytecodeConst::Str(value.clone())),
        mir::ConstValue::Null => Ok(BytecodeConst::Null),
        mir::ConstValue::Tuple(items) => items
            .iter()
            .map(lower_const_value)
            .collect::<Result<Vec<_>, _>>()
            .map(BytecodeConst::Tuple),
        mir::ConstValue::Array(items) => items
            .iter()
            .map(lower_const_value)
            .collect::<Result<Vec<_>, _>>()
            .map(BytecodeConst::Array),
        mir::ConstValue::List { elements, .. } => elements
            .iter()
            .map(lower_const_value)
            .collect::<Result<Vec<_>, _>>()
            .map(BytecodeConst::List),
        mir::ConstValue::Map { entries, .. } => {
            let mut lowered = Vec::with_capacity(entries.len());
            for (key, value) in entries {
                lowered.push((lower_const_value(key)?, lower_const_value(value)?));
            }
            Ok(BytecodeConst::Map(lowered))
        }
        _ => Err(BytecodeError::Lowering {
            message: format!("unsupported const value: {:?}", value),
        }),
    }
}

fn lower_place(place: &mir::Place) -> Result<BytecodePlace, BytecodeError> {
    let mut projection = Vec::new();
    for elem in &place.projection {
        match elem {
            mir::PlaceElem::Field(index, _) => {
                projection.push(BytecodePlaceElem::Field(*index as u32));
            }
            mir::PlaceElem::Index(local) => {
                projection.push(BytecodePlaceElem::Index(*local));
            }
            mir::PlaceElem::Deref => {}
            _ => {
                return Err(BytecodeError::Lowering {
                    message: format!("unsupported place projection: {:?}", elem),
                });
            }
        }
    }

    Ok(BytecodePlace {
        local: place.local,
        projection,
    })
}

fn lower_callee(operand: &mir::Operand) -> Result<BytecodeCallee, BytecodeError> {
    match operand {
        mir::Operand::Constant(constant) => match &constant.literal {
            mir::ConstantKind::Fn(symbol, _) => {
                Ok(BytecodeCallee::Function(symbol.as_str().to_string()))
            }
            mir::ConstantKind::Global(symbol, _) => {
                Ok(BytecodeCallee::Function(symbol.as_str().to_string()))
            }
            _ => Err(BytecodeError::Lowering {
                message: format!("unsupported call operand: {:?}", constant.literal),
            }),
        },
        mir::Operand::Copy(place) | mir::Operand::Move(place) => {
            Ok(BytecodeCallee::Local(lower_place(place)?))
        }
    }
}

fn push_const(pool: &mut Vec<BytecodeConst>, value: BytecodeConst) -> u32 {
    pool.push(value);
    (pool.len() - 1) as u32
}

fn lower_binop(op: &mir::BinOp) -> Result<BytecodeBinOp, BytecodeError> {
    let lowered = match op {
        mir::BinOp::Add => BytecodeBinOp::Add,
        mir::BinOp::Sub => BytecodeBinOp::Sub,
        mir::BinOp::Mul => BytecodeBinOp::Mul,
        mir::BinOp::Div => BytecodeBinOp::Div,
        mir::BinOp::Rem => BytecodeBinOp::Rem,
        mir::BinOp::And => BytecodeBinOp::And,
        mir::BinOp::Or => BytecodeBinOp::Or,
        mir::BinOp::BitXor => BytecodeBinOp::BitXor,
        mir::BinOp::BitAnd => BytecodeBinOp::BitAnd,
        mir::BinOp::BitOr => BytecodeBinOp::BitOr,
        mir::BinOp::Shl => BytecodeBinOp::Shl,
        mir::BinOp::Shr => BytecodeBinOp::Shr,
        mir::BinOp::Eq => BytecodeBinOp::Eq,
        mir::BinOp::Lt => BytecodeBinOp::Lt,
        mir::BinOp::Le => BytecodeBinOp::Le,
        mir::BinOp::Ne => BytecodeBinOp::Ne,
        mir::BinOp::Ge => BytecodeBinOp::Ge,
        mir::BinOp::Gt => BytecodeBinOp::Gt,
        _ => {
            return Err(BytecodeError::Lowering {
                message: format!("unsupported binary op: {:?}", op),
            });
        }
    };
    Ok(lowered)
}

fn lower_unop(op: &mir::UnOp) -> Result<BytecodeUnOp, BytecodeError> {
    let lowered = match op {
        mir::UnOp::Not => BytecodeUnOp::Not,
        mir::UnOp::Neg => BytecodeUnOp::Neg,
    };
    Ok(lowered)
}

fn format_const(value: &BytecodeConst) -> String {
    match value {
        BytecodeConst::Unit => "()".to_string(),
        BytecodeConst::Bool(value) => value.to_string(),
        BytecodeConst::Int(value) => value.to_string(),
        BytecodeConst::UInt(value) => format!("u64 {}", value),
        BytecodeConst::Float(value) => format!("f64 {}", value),
        BytecodeConst::Str(value) => format!("{:?}", value),
        BytecodeConst::Function(name) => format!("fn {}", name),
        BytecodeConst::Null => "null".to_string(),
        BytecodeConst::Tuple(items) => format_list("tuple", items),
        BytecodeConst::Array(items) => format_list("array", items),
        BytecodeConst::List(items) => format_list("list", items),
        BytecodeConst::Map(items) => {
            let mut rendered = Vec::with_capacity(items.len());
            for (key, value) in items {
                rendered.push(format!("{} => {}", format_const(key), format_const(value)));
            }
            format!("map [{}]", rendered.join(", "))
        }
    }
}

fn format_list(label: &str, items: &[BytecodeConst]) -> String {
    let rendered = items
        .iter()
        .map(format_const)
        .collect::<Vec<_>>()
        .join(", ");
    format!("{} [{}]", label, rendered)
}

fn format_instr(instr: &BytecodeInstr) -> String {
    match instr {
        BytecodeInstr::LoadConst(id) => format!("load.const {}", id),
        BytecodeInstr::LoadLocal(id) => format!("load.local {}", id),
        BytecodeInstr::StoreLocal(id) => format!("store.local {}", id),
        BytecodeInstr::LoadPlace(place) => format!("load.place {}", format_place(place)),
        BytecodeInstr::StorePlace(place) => format!("store.place {}", format_place(place)),
        BytecodeInstr::BinaryOp(op) => format!("binop {:?}", op),
        BytecodeInstr::UnaryOp(op) => format!("unop {:?}", op),
        BytecodeInstr::IntrinsicCall {
            kind,
            arg_count,
            format,
        } => {
            let format_label = format.as_deref().unwrap_or("");
            if format_label.is_empty() {
                format!("intrinsic {:?} {}", kind, arg_count)
            } else {
                format!("intrinsic {:?} {} {:?}", kind, arg_count, format_label)
            }
        }
        BytecodeInstr::MakeTuple(count) => format!("make.tuple {}", count),
        BytecodeInstr::MakeArray(count) => format!("make.array {}", count),
        BytecodeInstr::MakeList(count) => format!("make.list {}", count),
        BytecodeInstr::MakeMap(count) => format!("make.map {}", count),
        BytecodeInstr::ContainerGet => "container.get".to_string(),
        BytecodeInstr::ContainerLen => "container.len".to_string(),
        BytecodeInstr::Pop => "pop".to_string(),
    }
}

fn format_terminator(term: &BytecodeTerminator) -> String {
    match term {
        BytecodeTerminator::Return => "return".to_string(),
        BytecodeTerminator::Jump { target } => format!("jump bb{}", target),
        BytecodeTerminator::JumpIfTrue { target, otherwise } => {
            format!("jump_if_true bb{} else bb{}", target, otherwise)
        }
        BytecodeTerminator::JumpIfFalse { target, otherwise } => {
            format!("jump_if_false bb{} else bb{}", target, otherwise)
        }
        BytecodeTerminator::SwitchInt {
            values,
            targets,
            otherwise,
        } => {
            let mut pairs = Vec::with_capacity(values.len());
            for (value, target) in values.iter().zip(targets) {
                pairs.push(format!("{}:bb{}", value, target));
            }
            format!("switch [{}] otherwise bb{}", pairs.join(", "), otherwise)
        }
        BytecodeTerminator::Call {
            callee,
            arg_count,
            destination,
            target,
        } => {
            let dest = destination
                .as_ref()
                .map(format_place)
                .unwrap_or_else(|| "_".to_string());
            format!(
                "call {} {} -> {} then bb{}",
                format_callee(callee),
                arg_count,
                dest,
                target
            )
        }
        BytecodeTerminator::Abort => "abort".to_string(),
        BytecodeTerminator::Unreachable => "unreachable".to_string(),
    }
}

fn format_callee(callee: &BytecodeCallee) -> String {
    match callee {
        BytecodeCallee::Function(name) => format!("fn {}", name),
        BytecodeCallee::Local(place) => format!("local {}", format_place(place)),
    }
}

fn format_place(place: &BytecodePlace) -> String {
    if place.projection.is_empty() {
        return format!("_{}", place.local);
    }
    let mut rendered = format!("_{}", place.local);
    for elem in &place.projection {
        match elem {
            BytecodePlaceElem::Field(index) => {
                rendered.push_str(&format!(".{}", index));
            }
            BytecodePlaceElem::Index(local) => {
                rendered.push_str(&format!("[_{}]", local));
            }
        }
    }
    rendered
}
