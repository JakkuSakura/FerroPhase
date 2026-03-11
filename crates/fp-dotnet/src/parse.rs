use fp_core::error::{Error, Result};
use fp_core::lir::{
    BasicBlockId, LirBasicBlock, LirConstant, LirFunction, LirFunctionSignature, LirInstruction,
    LirInstructionKind, LirProgram, LirTerminator, LirType, LirValue, Name,
};
use std::collections::HashMap;

/// Parse a narrow subset of textual CIL into `LirProgram`.
///
/// Scope:
/// - Intended for CIL emitted by FerroPhase itself.
/// - Supports `ldc.i4`, `ldloc`, `stloc`, `add/sub/mul/div`, `ret`, `br`, `brtrue`, labels.
pub fn parse_cil_program(text: &str) -> Result<LirProgram> {
    let mut program = LirProgram::new();
    for method in parse_methods(text)? {
        program.add_function(lower_method(method)?);
    }
    Ok(program)
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ParsedMethod {
    name: Name,
    locals: u32,
    instructions: Vec<ParsedLine>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ParsedLine {
    Label(String),
    Instr(String),
}

fn parse_methods(text: &str) -> Result<Vec<ParsedMethod>> {
    let mut methods = Vec::new();
    let mut lines = text.lines().peekable();

    while let Some(raw) = lines.next() {
        let line = raw.trim();
        if !line.starts_with(".method") {
            continue;
        }

        let name = parse_method_name(line)?;
        let mut locals = 0u32;
        let mut body = Vec::new();

        while let Some(raw) = lines.next() {
            let line = raw.trim();
            if line == "}" {
                break;
            }
            if line.starts_with(".maxstack") || line.starts_with(".entrypoint") {
                continue;
            }
            if let Some(rest) = line.strip_prefix(".locals") {
                locals = parse_locals_count(rest)?;
                continue;
            }
            if let Some(label) = line.strip_suffix(':') {
                body.push(ParsedLine::Label(label.to_string()));
                continue;
            }
            if line.starts_with("//") || line.is_empty() {
                continue;
            }
            body.push(ParsedLine::Instr(line.to_string()));
        }

        methods.push(ParsedMethod {
            name,
            locals,
            instructions: body,
        });
    }

    Ok(methods)
}

fn parse_method_name(line: &str) -> Result<Name> {
    // We only need a best-effort parse for FerroPhase-emitted methods:
    // ".method public hidebysig static void Main() cil managed"
    // ".method public hidebysig static int32 main() cil managed"
    let open = line
        .find('(')
        .ok_or_else(|| Error::from("cil parse: malformed .method signature"))?;
    let before = line[..open].trim();
    let token = before
        .split_whitespace()
        .last()
        .ok_or_else(|| Error::from("cil parse: missing method name"))?;
    Ok(Name::new(token.trim_matches('\'')))
}

fn parse_locals_count(rest: &str) -> Result<u32> {
    // Accept either "init (...)" or a single local.
    let rest = rest.trim();
    if let Some(open) = rest.find('(') {
        let close = rest
            .rfind(')')
            .ok_or_else(|| Error::from("cil parse: malformed .locals"))?;
        let inside = &rest[open + 1..close];
        let count = inside
            .split(',')
            .map(|tok| tok.trim())
            .filter(|tok| !tok.is_empty())
            .count();
        return Ok(count as u32);
    }
    Ok(0)
}

fn lower_method(method: ParsedMethod) -> Result<LirFunction> {
    let label_to_block = collect_block_labels(&method.instructions);
    let mut blocks: Vec<LirBasicBlock> = Vec::new();

    // Always produce at least one block.
    let mut current_block_id: BasicBlockId = 0;
    let mut current_instructions = Vec::new();
    let mut current_label: Option<Name> = None;

    let mut stack: Vec<LirValue> = Vec::new();
    let mut next_vreg: u32 = 0;

    let mut iter = method.instructions.into_iter().peekable();
    while let Some(line) = iter.next() {
        match line {
            ParsedLine::Label(label) => {
                // Flush current block.
                if !current_instructions.is_empty() || current_label.is_some() {
                    blocks.push(LirBasicBlock {
                        id: current_block_id,
                        label: current_label.take(),
                        instructions: std::mem::take(&mut current_instructions),
                        terminator: LirTerminator::Br(
                            label_to_block
                                .get(&label)
                                .copied()
                                .unwrap_or(current_block_id),
                        ),
                        predecessors: Vec::new(),
                        successors: Vec::new(),
                    });
                    current_block_id += 1;
                    stack.clear();
                }
                current_label = Some(Name::new(label));
            }
            ParsedLine::Instr(text) => {
                if let Some(term) = try_parse_terminator(&text, &mut stack, &label_to_block)? {
                    blocks.push(LirBasicBlock {
                        id: current_block_id,
                        label: current_label.take(),
                        instructions: std::mem::take(&mut current_instructions),
                        terminator: term,
                        predecessors: Vec::new(),
                        successors: Vec::new(),
                    });
                    current_block_id += 1;
                    stack.clear();
                    continue;
                }
                if let Some(inst) = parse_stack_instruction(&text, &mut stack, &mut next_vreg)? {
                    current_instructions.push(inst);
                }
            }
        }
    }

    if blocks.is_empty() {
        blocks.push(LirBasicBlock {
            id: current_block_id,
            label: current_label.take(),
            instructions: current_instructions,
            terminator: LirTerminator::Return(stack.pop()),
            predecessors: Vec::new(),
            successors: Vec::new(),
        });
    } else if !current_instructions.is_empty() || current_label.is_some() {
        blocks.push(LirBasicBlock {
            id: current_block_id,
            label: current_label.take(),
            instructions: current_instructions,
            terminator: LirTerminator::Return(stack.pop()),
            predecessors: Vec::new(),
            successors: Vec::new(),
        });
    }

    Ok(LirFunction {
        name: method.name,
        signature: LirFunctionSignature {
            params: Vec::new(),
            return_type: LirType::I64,
            is_variadic: false,
        },
        basic_blocks: blocks,
        locals: (0..method.locals)
            .map(|id| fp_core::lir::LirLocal {
                id,
                ty: LirType::I64,
                name: Some(format!("loc{id}")),
                is_argument: false,
            })
            .collect(),
        stack_slots: Vec::new(),
        calling_convention: fp_core::lir::CallingConvention::C,
        linkage: fp_core::lir::Linkage::External,
        is_declaration: false,
    })
}

fn collect_block_labels(lines: &[ParsedLine]) -> HashMap<String, BasicBlockId> {
    let mut map = HashMap::new();
    let mut next = 0u32;
    for line in lines {
        if let ParsedLine::Label(label) = line {
            map.entry(label.clone()).or_insert_with(|| {
                let id = next;
                next += 1;
                id
            });
        }
    }
    if map.is_empty() {
        map.insert("entry".to_string(), 0);
    }
    map
}

fn try_parse_terminator(
    line: &str,
    stack: &mut Vec<LirValue>,
    labels: &HashMap<String, BasicBlockId>,
) -> Result<Option<LirTerminator>> {
    let line = line.trim();
    if line == "ret" {
        return Ok(Some(LirTerminator::Return(stack.pop())));
    }
    if let Some(rest) = line.strip_prefix("br ") {
        let target = rest.trim();
        let bb = labels
            .get(target)
            .copied()
            .ok_or_else(|| Error::from(format!("cil parse: unknown label `{target}`")))?;
        return Ok(Some(LirTerminator::Br(bb)));
    }
    if let Some(rest) = line.strip_prefix("brtrue ") {
        let target = rest.trim();
        let bb = labels
            .get(target)
            .copied()
            .ok_or_else(|| Error::from(format!("cil parse: unknown label `{target}`")))?;
        let cond = stack
            .pop()
            .ok_or_else(|| Error::from("cil parse: brtrue missing condition"))?;
        return Ok(Some(LirTerminator::CondBr {
            condition: cond,
            if_true: bb,
            if_false: bb,
        }));
    }
    Ok(None)
}

fn parse_stack_instruction(
    line: &str,
    stack: &mut Vec<LirValue>,
    next_vreg: &mut u32,
) -> Result<Option<LirInstruction>> {
    let line = line.trim();

    if let Some(rest) = line.strip_prefix("ldc.i4 ") {
        let value = rest
            .trim()
            .parse::<i64>()
            .map_err(|_| Error::from("cil parse: invalid ldc.i4"))?;
        stack.push(LirValue::Constant(LirConstant::Int(value, LirType::I64)));
        return Ok(None);
    }
    if let Some(rest) = line.strip_prefix("ldloc.") {
        let id = rest
            .trim()
            .parse::<u32>()
            .map_err(|_| Error::from("cil parse: invalid ldloc"))?;
        stack.push(LirValue::Local(id));
        return Ok(None);
    }
    if let Some(rest) = line.strip_prefix("stloc.") {
        let _id = rest
            .trim()
            .parse::<u32>()
            .map_err(|_| Error::from("cil parse: invalid stloc"))?;
        let value = stack
            .pop()
            .ok_or_else(|| Error::from("cil parse: stloc missing value"))?;
        let id_inst = *next_vreg;
        *next_vreg += 1;
        return Ok(Some(LirInstruction {
            id: id_inst,
            kind: LirInstructionKind::Freeze(value),
            type_hint: Some(LirType::I64),
            debug_info: None,
        }));
    }

    let binop = match line {
        "add" => Some(LirInstructionKind::Add as fn(_, _) -> _),
        "sub" => Some(LirInstructionKind::Sub as fn(_, _) -> _),
        "mul" => Some(LirInstructionKind::Mul as fn(_, _) -> _),
        "div" => Some(LirInstructionKind::Div as fn(_, _) -> _),
        _ => None,
    };
    if let Some(constructor) = binop {
        let rhs = stack
            .pop()
            .ok_or_else(|| Error::from("cil parse: missing rhs"))?;
        let lhs = stack
            .pop()
            .ok_or_else(|| Error::from("cil parse: missing lhs"))?;
        let id = *next_vreg;
        *next_vreg += 1;
        let kind = constructor(lhs.clone(), rhs.clone());
        stack.push(LirValue::Register(id));
        return Ok(Some(LirInstruction {
            id,
            kind,
            type_hint: Some(LirType::I64),
            debug_info: None,
        }));
    }

    Ok(None)
}
