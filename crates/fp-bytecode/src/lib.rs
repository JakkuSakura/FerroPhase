pub use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::mir;
use serde::{Deserialize, Serialize};
use thiserror::Error;

pub const BYTECODE_MAGIC: [u8; 4] = *b"FPBC";
pub const BYTECODE_VERSION: u32 = 1;

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
    pub params: u32,
    pub locals: u32,
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

pub fn encode_file(program: &BytecodeProgram) -> Result<Vec<u8>, BytecodeError> {
    let file = BytecodeFile {
        version: BYTECODE_VERSION,
        program: program.clone(),
    };
    let mut encoded = Vec::new();
    encoded.extend_from_slice(&BYTECODE_MAGIC);
    encoded.extend_from_slice(&BYTECODE_VERSION.to_le_bytes());
    encoded.extend_from_slice(&bincode::serialize(&file)?);
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
    let file: BytecodeFile = bincode::deserialize(&bytes[8..]).map_err(BytecodeError::Decode)?;
    if file.version != BYTECODE_VERSION {
        return Err(BytecodeError::Format {
            message: format!(
                "bytecode payload version {} does not match expected {}",
                file.version, BYTECODE_VERSION
            ),
        });
    }
    validate_program(&file.program)?;
    Ok(file)
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
        BytecodeConst::UInt(value) => value.to_string(),
        BytecodeConst::Float(value) => value.to_string(),
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
                "call {:?} {} -> {} then bb{}",
                callee, arg_count, dest, target
            )
        }
        BytecodeTerminator::Abort => "abort".to_string(),
        BytecodeTerminator::Unreachable => "unreachable".to_string(),
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
