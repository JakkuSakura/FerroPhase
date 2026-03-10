use fp_core::error::{Error, Result};
use fp_core::lir::{
    BasicBlockId, LirBasicBlock, LirConstant, LirFunction, LirFunctionSignature, LirInstruction,
    LirInstructionKind, LirProgram, LirTerminator, LirType, LirValue, Name,
};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
struct ParsedFunction {
    name: Name,
    blocks: Vec<ParsedBlock>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ParsedBlock {
    label: String,
    lines: Vec<String>,
}

/// Parse URCL text emitted by `fp-urcl` back into a `LirProgram`.
///
/// Scope: this is intentionally conservative and primarily targets the
/// SSA-ish URCL subset emitted by FerroPhase itself.
pub fn parse_program(text: &str) -> Result<LirProgram> {
    let mut program = LirProgram::new();
    for function in parse_functions(text)? {
        program.add_function(lower_function(function)?);
    }
    Ok(program)
}

fn parse_functions(text: &str) -> Result<Vec<ParsedFunction>> {
    let mut functions = Vec::new();

    let mut current: Option<ParsedFunction> = None;
    let mut current_block: Option<ParsedBlock> = None;

    for raw_line in text.lines() {
        let line = strip_comment(raw_line).trim();
        if line.is_empty() {
            continue;
        }

        if line.starts_with("BITS")
            || line.starts_with("MINREG")
            || line.starts_with(".data")
            || line.starts_with("DW")
        {
            continue;
        }

        if let Some(name) = line.strip_prefix(".function") {
            flush_block(&mut current, &mut current_block);
            if let Some(func) = current.take() {
                functions.push(func);
            }
            let name = name.trim();
            if name.is_empty() {
                return Err(Error::from("urcl parse: missing function name"));
            }
            current = Some(ParsedFunction {
                name: Name::new(name),
                blocks: Vec::new(),
            });
            continue;
        }

        if let Some(label) = line.strip_suffix(':') {
            // Local labels are emitted for lowered select/compare. We keep them
            // inline so the peephole parser can recover high-level ops.
            if label.starts_with(".L") {
                if let Some(block) = current_block.as_mut() {
                    block.lines.push(line.to_string());
                    continue;
                }
            }

            flush_block(&mut current, &mut current_block);
            current_block = Some(ParsedBlock {
                label: label.to_string(),
                lines: Vec::new(),
            });
            continue;
        }

        let Some(block) = current_block.as_mut() else {
            return Err(Error::from("urcl parse: instruction outside block"));
        };
        block.lines.push(line.to_string());
    }

    flush_block(&mut current, &mut current_block);
    if let Some(func) = current.take() {
        functions.push(func);
    }

    Ok(functions)
}

fn flush_block(current: &mut Option<ParsedFunction>, current_block: &mut Option<ParsedBlock>) {
    let Some(block) = current_block.take() else {
        return;
    };
    if let Some(func) = current.as_mut() {
        func.blocks.push(block);
    }
}

fn lower_function(function: ParsedFunction) -> Result<LirFunction> {
    if function.blocks.is_empty() {
        return Err(Error::from("urcl parse: function has no basic blocks"));
    }

    let label_to_block: HashMap<String, BasicBlockId> = function
        .blocks
        .iter()
        .enumerate()
        .map(|(index, block)| (block.label.clone(), index as BasicBlockId))
        .collect();

    let mut basic_blocks = Vec::new();
    for (index, block) in function.blocks.iter().enumerate() {
        basic_blocks.push(lower_block(
            &function.name,
            index as BasicBlockId,
            block,
            &label_to_block,
        )?);
    }

    Ok(LirFunction {
        name: function.name,
        signature: LirFunctionSignature {
            params: Vec::new(),
            return_type: LirType::I64,
            is_variadic: false,
        },
        basic_blocks,
        locals: Vec::new(),
        stack_slots: Vec::new(),
        calling_convention: fp_core::lir::CallingConvention::C,
        linkage: fp_core::lir::Linkage::External,
        is_declaration: false,
    })
}

fn lower_block(
    function_name: &Name,
    id: BasicBlockId,
    block: &ParsedBlock,
    label_to_block: &HashMap<String, BasicBlockId>,
) -> Result<LirBasicBlock> {
    let label = block_label_suffix(function_name, &block.label)
        .map(Name::new)
        .filter(|suffix| !suffix.as_str().is_empty());

    let mut instructions = Vec::new();
    let mut cursor = 0usize;
    while cursor < block.lines.len() {
        if let Some((inst, consumed)) = parse_select_pattern(&block.lines[cursor..])? {
            instructions.push(inst);
            cursor += consumed;
            continue;
        }
        if let Some((inst, consumed)) = parse_compare_pattern(&block.lines[cursor..])? {
            instructions.push(inst);
            cursor += consumed;
            continue;
        }
        if is_local_label(&block.lines[cursor]) {
            cursor += 1;
            continue;
        }
        if cursor + 1 < block.lines.len() {
            if parse_return_mov(&block.lines[cursor])?.is_some()
                && block.lines[cursor + 1].starts_with("RET")
            {
                break;
            }
        }
        if is_terminator_start(&block.lines[cursor]) {
            break;
        }
        instructions.push(parse_instruction(&block.lines[cursor])?);
        cursor += 1;
    }

    let terminator = parse_terminator(&block.lines[cursor..], label_to_block)?;

    Ok(LirBasicBlock {
        id,
        label,
        instructions,
        terminator,
        predecessors: Vec::new(),
        successors: Vec::new(),
    })
}

fn parse_instruction(line: &str) -> Result<LirInstruction> {
    let (mnemonic, rest) = split_mnemonic_operands(line)?;
    let operands = split_operands(rest);

    let dst_reg = operands
        .first()
        .and_then(|op| parse_register(op))
        .ok_or_else(|| Error::from(format!("urcl parse: missing dst register in `{line}`")))?;
    let id = dst_reg
        .checked_sub(2)
        .ok_or_else(|| Error::from("urcl parse: invalid destination register"))? as u32;

    let kind = match mnemonic {
        "ADD" => binary_kind(LirInstructionKind::Add, &operands, line)?,
        "SUB" => binary_kind(LirInstructionKind::Sub, &operands, line)?,
        "MLT" => binary_kind(LirInstructionKind::Mul, &operands, line)?,
        "DIV" => binary_kind(LirInstructionKind::Div, &operands, line)?,
        "MOD" => binary_kind(LirInstructionKind::Rem, &operands, line)?,
        "AND" => binary_kind(LirInstructionKind::And, &operands, line)?,
        "OR" => binary_kind(LirInstructionKind::Or, &operands, line)?,
        "XOR" => binary_kind(LirInstructionKind::Xor, &operands, line)?,
        "LSH" => binary_kind(LirInstructionKind::Shl, &operands, line)?,
        "RSH" => binary_kind(LirInstructionKind::Shr, &operands, line)?,
        "NOT" => {
            let value = parse_value(operands.get(1).map(|value| value.as_str()), line)?;
            LirInstructionKind::Not(value)
        }
        "LOD" => {
            let address = parse_value(operands.get(1).map(|value| value.as_str()), line)?;
            LirInstructionKind::Load {
                address,
                alignment: None,
                volatile: false,
            }
        }
        "MOV" => {
            let value = parse_value(operands.get(1).map(|value| value.as_str()), line)?;
            LirInstructionKind::Freeze(value)
        }
        "PSH" => {
            let size = parse_value(operands.get(1).map(|value| value.as_str()), line)?;
            LirInstructionKind::Alloca {
                size,
                alignment: 16,
            }
        }
        other => {
            return Err(Error::from(format!(
                "urcl parse: unsupported instruction `{other}`"
            )))
        }
    };

    let ty = match &kind {
        LirInstructionKind::Eq(..)
        | LirInstructionKind::Ne(..)
        | LirInstructionKind::Lt(..)
        | LirInstructionKind::Le(..)
        | LirInstructionKind::Gt(..)
        | LirInstructionKind::Ge(..) => LirType::I1,
        LirInstructionKind::Load { .. } => LirType::I64,
        LirInstructionKind::Alloca { .. } => LirType::Ptr(Box::new(LirType::I8)),
        _ => LirType::I64,
    };

    Ok(LirInstruction {
        id,
        kind,
        type_hint: Some(ty),
        debug_info: None,
    })
}

fn parse_terminator(
    lines: &[String],
    label_to_block: &HashMap<String, BasicBlockId>,
) -> Result<LirTerminator> {
    if lines.len() >= 2 {
        if let Some(value) = parse_return_mov(&lines[0])? {
            if lines[1].starts_with("RET") {
                return Ok(LirTerminator::Return(Some(value)));
            }
        }
    }
    if lines.first().is_some_and(|line| line.starts_with("RET")) {
        return Ok(LirTerminator::Return(None));
    }
    if lines.len() >= 2 {
        if let Some((cond, true_label)) = parse_bnz(&lines[0])? {
            if let Some(false_label) = parse_jmp(&lines[1])? {
                let if_true = resolve_block(label_to_block, &true_label)?;
                let if_false = resolve_block(label_to_block, &false_label)?;
                return Ok(LirTerminator::CondBr {
                    condition: cond,
                    if_true,
                    if_false,
                });
            }
        }
    }
    if let Some(target) = lines
        .first()
        .and_then(|line| parse_jmp(line).transpose())
        .transpose()?
    {
        return Ok(LirTerminator::Br(resolve_block(label_to_block, &target)?));
    }

    match lines.first() {
        Some(line) => Err(Error::from(format!(
            "urcl parse: unsupported terminator `{line}`"
        ))),
        None => Ok(LirTerminator::Return(None)),
    }
}

fn parse_return_mov(line: &str) -> Result<Option<LirValue>> {
    let (mnemonic, rest) = split_mnemonic_operands(line)?;
    if mnemonic != "MOV" {
        return Ok(None);
    }
    let operands = split_operands(rest);
    let Some(dst) = operands.first().and_then(|op| parse_register(op)) else {
        return Ok(None);
    };
    if dst != 1 {
        return Ok(None);
    }
    Ok(Some(parse_value(
        operands.get(1).map(|value| value.as_str()),
        line,
    )?))
}

fn parse_bnz(line: &str) -> Result<Option<(LirValue, String)>> {
    let (mnemonic, rest) = split_mnemonic_operands(line)?;
    if mnemonic != "BNZ" {
        return Ok(None);
    }
    let operands = split_operands(rest);
    let cond = parse_value(operands.get(0).map(|value| value.as_str()), line)?;
    let label = operands
        .get(1)
        .ok_or_else(|| Error::from("urcl parse: missing BNZ target"))?;
    Ok(Some((cond, label.to_string())))
}

fn parse_jmp(line: &str) -> Result<Option<String>> {
    let (mnemonic, rest) = split_mnemonic_operands(line)?;
    if mnemonic != "JMP" {
        return Ok(None);
    }
    let operands = split_operands(rest);
    let label = operands
        .first()
        .ok_or_else(|| Error::from("urcl parse: missing JMP target"))?;
    Ok(Some(label.to_string()))
}

fn resolve_block(map: &HashMap<String, BasicBlockId>, label: &str) -> Result<BasicBlockId> {
    map.get(label)
        .copied()
        .ok_or_else(|| Error::from(format!("urcl parse: unknown block label `{label}`")))
}

fn parse_select_pattern(lines: &[String]) -> Result<Option<(LirInstruction, usize)>> {
    if lines.len() < 6 {
        return Ok(None);
    }
    let Some((dst, if_false)) = parse_mov_assign(&lines[0])? else {
        return Ok(None);
    };
    let Some((cond, true_label)) = parse_bnz(&lines[1])? else {
        return Ok(None);
    };
    let Some(done_label) = parse_jmp(&lines[2])? else {
        return Ok(None);
    };
    if !is_local_label(&lines[3]) || !is_local_label(&lines[5]) {
        return Ok(None);
    }
    if lines[3].trim_end_matches(':') != true_label {
        return Ok(None);
    }
    if lines[5].trim_end_matches(':') != done_label {
        return Ok(None);
    }
    let Some((dst2, if_true)) = parse_mov_assign(&lines[4])? else {
        return Ok(None);
    };
    if dst2 != dst {
        return Ok(None);
    }

    let id = dst
        .checked_sub(2)
        .ok_or_else(|| Error::from("urcl parse: invalid select dst"))? as u32;
    Ok(Some((
        LirInstruction {
            id,
            kind: LirInstructionKind::Select {
                condition: cond,
                if_true,
                if_false,
            },
            type_hint: Some(LirType::I64),
            debug_info: None,
        },
        6,
    )))
}

fn parse_compare_pattern(lines: &[String]) -> Result<Option<(LirInstruction, usize)>> {
    if lines.len() < 6 {
        return Ok(None);
    }
    let Some((dst, zero)) = parse_mov_assign(&lines[0])? else {
        return Ok(None);
    };
    if !matches!(
        zero,
        LirValue::Constant(LirConstant::Int(0, _)) | LirValue::Constant(LirConstant::UInt(0, _))
    ) {
        return Ok(None);
    }
    let (branch_mnemonic, branch_rest) = split_mnemonic_operands(&lines[1])?;
    let constructor: fn(LirValue, LirValue) -> LirInstructionKind = match branch_mnemonic {
        "BRE" => LirInstructionKind::Eq,
        "BNE" => LirInstructionKind::Ne,
        "BRL" => LirInstructionKind::Lt,
        "BLE" => LirInstructionKind::Le,
        "BRG" => LirInstructionKind::Gt,
        "BGE" => LirInstructionKind::Ge,
        _ => return Ok(None),
    };
    let operands = split_operands(branch_rest);
    if operands.len() != 3 {
        return Ok(None);
    }
    let lhs = parse_value(Some(operands[0].as_str()), &lines[1])?;
    let rhs = parse_value(Some(operands[1].as_str()), &lines[1])?;
    let true_label = operands[2].clone();
    let Some(done_label) = parse_jmp(&lines[2])? else {
        return Ok(None);
    };
    if lines[3].trim_end_matches(':') != true_label {
        return Ok(None);
    }
    let Some((dst2, one)) = parse_mov_assign(&lines[4])? else {
        return Ok(None);
    };
    if dst2 != dst {
        return Ok(None);
    }
    if !matches!(
        one,
        LirValue::Constant(LirConstant::Int(1, _)) | LirValue::Constant(LirConstant::UInt(1, _))
    ) {
        return Ok(None);
    }
    if lines[5].trim_end_matches(':') != done_label {
        return Ok(None);
    }

    let id = dst
        .checked_sub(2)
        .ok_or_else(|| Error::from("urcl parse: invalid compare dst"))? as u32;
    Ok(Some((
        LirInstruction {
            id,
            kind: constructor(lhs, rhs),
            type_hint: Some(LirType::I1),
            debug_info: None,
        },
        6,
    )))
}

fn parse_mov_assign(line: &str) -> Result<Option<(u32, LirValue)>> {
    let (mnemonic, rest) = split_mnemonic_operands(line)?;
    if mnemonic != "MOV" {
        return Ok(None);
    }
    let operands = split_operands(rest);
    let Some(dst) = operands.first().and_then(|op| parse_register(op)) else {
        return Ok(None);
    };
    let value = parse_value(operands.get(1).map(|value| value.as_str()), line)?;
    Ok(Some((dst, value)))
}

fn binary_kind(
    constructor: fn(LirValue, LirValue) -> LirInstructionKind,
    operands: &[String],
    line: &str,
) -> Result<LirInstructionKind> {
    let lhs = parse_value(operands.get(1).map(|value| value.as_str()), line)?;
    let rhs = parse_value(operands.get(2).map(|value| value.as_str()), line)?;
    Ok(constructor(lhs, rhs))
}

fn parse_value(token: Option<&str>, line: &str) -> Result<LirValue> {
    let token = token.ok_or_else(|| Error::from(format!("urcl parse: missing operand in `{line}`")))?;
    if let Some(reg) = parse_register(token) {
        if reg == 1 {
            return Ok(LirValue::Register(0));
        }
        let id = reg
            .checked_sub(2)
            .ok_or_else(|| Error::from("urcl parse: invalid register operand"))?;
        return Ok(LirValue::Register(id));
    }
    if let Some(global) = token.strip_prefix('@') {
        return Ok(LirValue::Global(
            global.to_string(),
            LirType::Ptr(Box::new(LirType::I8)),
        ));
    }
    if token == "undef" {
        return Ok(LirValue::Undef(LirType::I64));
    }
    if token == "0" {
        return Ok(LirValue::Null(LirType::Ptr(Box::new(LirType::I8))));
    }
    if let Ok(value) = token.parse::<i64>() {
        return Ok(LirValue::Constant(LirConstant::Int(value, LirType::I64)));
    }
    Err(Error::from(format!("urcl parse: unsupported operand `{token}`")))
}

fn parse_register(token: &str) -> Option<u32> {
    let digits = token.trim().strip_prefix('R')?;
    digits.parse::<u32>().ok()
}

fn split_mnemonic_operands(line: &str) -> Result<(&str, &str)> {
    let line = line.trim();
    let mut parts = line.splitn(2, char::is_whitespace);
    let mnemonic = parts
        .next()
        .ok_or_else(|| Error::from("urcl parse: empty line"))?;
    let rest = parts.next().unwrap_or("").trim();
    Ok((mnemonic, rest))
}

fn split_operands(rest: &str) -> Vec<String> {
    if rest.is_empty() {
        return Vec::new();
    }
    rest.split(',').map(|part| part.trim().to_string()).collect()
}

fn strip_comment(line: &str) -> &str {
    line.split(';').next().unwrap_or(line)
}

fn is_local_label(line: &str) -> bool {
    line.starts_with(".L") && line.trim_end().ends_with(':')
}

fn is_terminator_start(line: &str) -> bool {
    line.starts_with("RET") || line.starts_with("JMP") || line.starts_with("BNZ")
}

fn block_label_suffix<'a>(function_name: &Name, label: &'a str) -> Option<&'a str> {
    let prefix = format!("{}_", function_name.as_str());
    label.strip_prefix(&prefix)
}

