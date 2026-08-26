use fp_core::diagnostics::DiagnosticManager;
pub use fp_core::intrinsics::IntrinsicKind;
use fp_core::mir;
use thiserror::Error;
use winnow::ModalResult;
use winnow::Parser;
use winnow::error::{ContextError, ErrMode};
use winnow::token::{literal, take_till, take_while};

mod types;
pub use types::*;
mod parser;
use parser::*;
mod lowering;
use lowering::*;

pub const BYTECODE_MAGIC: [u8; 4] = *b"FPBC";
pub const BYTECODE_VERSION: u32 = 2;
const BYTECODE_LOWERING_CONTEXT: &str = "mir→bytecode";

#[derive(Debug, Error)]
enum LoweringFallbackError {
    #[error("missing cleanup target for assert to bb{0}")]
    MissingAssertCleanup(u32),
    #[error("terminator_otherwise expects assert terminator")]
    InvalidOtherwiseTerminator,
    #[error("unsupported binary op: {0:?}")]
    UnsupportedBinaryOp(mir::BinOp),
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
    if function.param_types.len() > function.local_types.len() {
        return Err(BytecodeError::Format {
            message: format!(
                "function {} has {} params but only {} locals",
                function.name,
                function.param_types.len(),
                function.local_types.len()
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
        validate_block(block, function.local_types.len(), const_pool_len, &ids)?;
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
            "    fn {}(params: [{}], return: {}, locals: [{}])\n",
            function.name,
            function
                .param_types
                .iter()
                .map(format_lir_type)
                .collect::<Vec<_>>()
                .join(", "),
            format_lir_type(&function.return_type),
            function
                .local_types
                .iter()
                .map(format_lir_type)
                .collect::<Vec<_>>()
                .join(", ")
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

pub fn lower_program(program: &mir::MirCodeUnit) -> Result<BytecodeProgram, BytecodeError> {
    let mut const_pool = Vec::new();
    let mut functions = Vec::new();
    let function_names = program
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            mir::ItemKind::Function(function) => function
                .def_id
                .clone()
                .map(|def_id| (def_id, function.name.as_str().to_string())),
            mir::ItemKind::ExecutableConst(entry) => Some((
                entry.def_id.clone(),
                entry.function_name.as_str().to_string(),
            )),
            _ => None,
        })
        .collect::<std::collections::HashMap<_, _>>();

    for item in &program.items {
        let synthetic_function;
        let function = match &item.kind {
            mir::ItemKind::Function(func) => func,
            mir::ItemKind::ExecutableConst(entry) => {
                synthetic_function = mir::Function {
                    name: entry.function_name.clone(),
                    def_id: Some(entry.def_id.clone()),
                    substs: Vec::new(),
                    sig: mir::FunctionSig {
                        inputs: Vec::new(),
                        output: entry.ty.clone(),
                    },
                    body_id: entry.body_id,
                    abi: mir::ty::Abi::Rust,
                    is_extern: false,
                    attrs: Vec::new(),
                };
                &synthetic_function
            }
            mir::ItemKind::Static(_) | mir::ItemKind::Query(_) => continue,
        };
        let body =
            program
                .bodies
                .get(&function.body_id)
                .ok_or_else(|| BytecodeError::Lowering {
                    message: format!("missing body for function {}", function.name.as_str()),
                })?;
        let lowered = lower_function(function, body, &mut const_pool, &function_names)?;
        let executable_alias = match &item.kind {
            mir::ItemKind::ExecutableConst(entry) => Some(entry.def_id.comptime_const_symbol()),
            _ => None,
        };
        functions.push(lowered.clone());
        if let Some(alias) = executable_alias {
            let mut alias_function = lowered;
            alias_function.name = alias;
            functions.push(alias_function);
        }
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

fn format_const(value: &BytecodeConst) -> String {
    match value {
        BytecodeConst::Unit => "()".to_string(),
        BytecodeConst::Bool(value) => value.to_string(),
        BytecodeConst::Int(value) => value.to_string(),
        BytecodeConst::UInt(value) => format!("u64 {}", value),
        BytecodeConst::Float(value) => format!("f64 {}", value),
        BytecodeConst::Str(value) => format!("{:?}", value),
        BytecodeConst::Function(name) => format!("fn {}", name),
        BytecodeConst::Global(name) => format!("global {}", name),
        BytecodeConst::Null => "null".to_string(),
        BytecodeConst::Undef => "undef".to_string(),
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
            result_type,
        } => {
            let format_label = format.as_deref().unwrap_or("");
            if format_label.is_empty() {
                format!(
                    "intrinsic {:?} {} : {}",
                    kind,
                    arg_count,
                    format_lir_type(result_type)
                )
            } else {
                format!(
                    "intrinsic {:?} {} : {} {:?}",
                    kind,
                    arg_count,
                    format_lir_type(result_type),
                    format_label
                )
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
            result_type,
            target,
        } => {
            let dest = destination
                .as_ref()
                .map(format_place)
                .unwrap_or_else(|| "_".to_string());
            format!(
                "call {} {} : {} -> {} then bb{}",
                format_callee(callee),
                arg_count,
                format_lir_type(result_type),
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
