use crate::config::GoAsmTarget;
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

/// Parse the Go assembler subset emitted by `fp-goasm` back into `LirProgram`.
///
/// This parser intentionally targets the canonical text emitted by FerroPhase
/// (`fp-goasm/src/emit.rs`). It is not a general Plan9 assembler parser.
pub fn parse_program(text: &str) -> Result<(LirProgram, GoAsmTarget)> {
    let target = detect_target(text);
    let functions = parse_functions(text)?;
    let mut program = LirProgram::new();
    for function in functions {
        program.add_function(lower_function(function, target)?);
    }
    Ok((program, target))
}

fn detect_target(text: &str) -> GoAsmTarget {
    for line in text.lines() {
        let line = line.trim();
        if let Some(suffix) = line.strip_prefix("// fp-goasm (") {
            if let Some(arch) = suffix.strip_suffix(')') {
                if let Some(target) = GoAsmTarget::parse(arch) {
                    return target;
                }
            }
        }
    }

    // Heuristic fallback.
    if text.contains("MOVQ") || text.contains("ADDQ") || text.contains("CMPQ") {
        GoAsmTarget::Amd64
    } else {
        GoAsmTarget::Arm64
    }
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
        if line.starts_with("#include") || line.starts_with("GLOBL") || line.starts_with("DATA") {
            continue;
        }

        if let Some(rest) = line.strip_prefix("TEXT") {
            flush_block(&mut current, &mut current_block);
            if let Some(func) = current.take() {
                functions.push(func);
            }
            let name = parse_text_symbol(rest.trim())?;
            current = Some(ParsedFunction {
                name,
                blocks: Vec::new(),
            });
            continue;
        }

        if let Some(label) = line.strip_suffix(':') {
            if label.starts_with('L') {
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
            return Err(Error::from("goasm parse: instruction outside block"));
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

fn parse_text_symbol(rest: &str) -> Result<Name> {
    // Example: "·main(SB), NOSPLIT, $0-0"
    let name = rest
        .split(',')
        .next()
        .ok_or_else(|| Error::from("goasm parse: malformed TEXT directive"))?
        .trim();
    let name = name
        .strip_suffix("(SB)")
        .ok_or_else(|| Error::from("goasm parse: missing (SB) in TEXT directive"))?;
    let name = name
        .strip_prefix('·')
        .ok_or_else(|| Error::from("goasm parse: expected · symbol prefix"))?;
    Ok(Name::new(name))
}

fn lower_function(function: ParsedFunction, target: GoAsmTarget) -> Result<LirFunction> {
    if function.blocks.is_empty() {
        return Err(Error::from("goasm parse: function has no basic blocks"));
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
            target,
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
    target: GoAsmTarget,
) -> Result<LirBasicBlock> {
    let label = block_label_suffix(function_name, &block.label)
        .map(Name::new)
        .filter(|suffix| !suffix.as_str().is_empty());

    let mut instructions = Vec::new();
    let mut cursor = 0usize;
    while cursor < block.lines.len() {
        if let Some((inst, consumed)) = parse_binop_pattern(&block.lines[cursor..], target)? {
            instructions.push(inst);
            cursor += consumed;
            continue;
        }
        if let Some((inst, consumed)) = parse_not_pattern(&block.lines[cursor..], target)? {
            instructions.push(inst);
            cursor += consumed;
            continue;
        }
        if let Some((inst, consumed)) = parse_compare_pattern(&block.lines[cursor..], target)? {
            instructions.push(inst);
            cursor += consumed;
            continue;
        }
        if cursor + 1 < block.lines.len() {
            if parse_return_mov(&block.lines[cursor], target)?.is_some()
                && block.lines[cursor + 1].starts_with("RET")
            {
                break;
            }
        }
        if is_local_label(&block.lines[cursor]) {
            cursor += 1;
            continue;
        }
        if is_terminator_start(&block.lines[cursor]) {
            break;
        }
        // Fallback: accept simple moves.
        if let Some(inst) = parse_freeze(&block.lines[cursor], target)? {
            instructions.push(inst);
            cursor += 1;
            continue;
        }
        return Err(Error::from(format!(
            "goasm parse: unsupported instruction sequence starting at `{}`",
            block.lines[cursor]
        )));
    }

    let terminator = parse_terminator(&block.lines[cursor..], label_to_block, target)?;

    Ok(LirBasicBlock {
        id,
        label,
        instructions,
        terminator,
        predecessors: Vec::new(),
        successors: Vec::new(),
    })
}

fn parse_freeze(line: &str, target: GoAsmTarget) -> Result<Option<LirInstruction>> {
    let line = line.trim_start_matches('\t').trim();
    let (mnemonic, rest) = split_mnemonic_operands(line)?;
    let expected_mov = match target {
        GoAsmTarget::Amd64 => "MOVQ",
        GoAsmTarget::Arm64 => "MOVD",
    };
    if mnemonic != expected_mov {
        return Ok(None);
    }
    let operands = split_operands(rest);
    if operands.len() != 2 {
        return Ok(None);
    }
    let Some(dst_reg) = parse_register(&operands[1], target) else {
        return Ok(None);
    };
    let id = dst_reg
        .checked_sub(10)
        .ok_or_else(|| Error::from("goasm parse: invalid destination register"))?;
    let value = parse_value(Some(operands[0].as_str()), target, line)?;
    Ok(Some(LirInstruction {
        id,
        kind: LirInstructionKind::Freeze(value),
        type_hint: Some(LirType::I64),
        debug_info: None,
    }))
}

fn parse_binop_pattern(
    lines: &[String],
    target: GoAsmTarget,
) -> Result<Option<(LirInstruction, usize)>> {
    if lines.len() < 2 {
        return Ok(None);
    }
    let Some(mov) = parse_move_line(&lines[0], target)? else {
        return Ok(None);
    };
    let Some((op, rhs, dst)) = parse_binop_line(&lines[1], target)? else {
        return Ok(None);
    };
    if dst != mov.dst {
        return Ok(None);
    }
    let id = dst
        .checked_sub(10)
        .ok_or_else(|| Error::from("goasm parse: invalid destination register"))?;
    let kind = match op {
        BinOp::Add => LirInstructionKind::Add(mov.src, rhs),
        BinOp::Sub => LirInstructionKind::Sub(mov.src, rhs),
        BinOp::Mul => LirInstructionKind::Mul(mov.src, rhs),
        BinOp::And => LirInstructionKind::And(mov.src, rhs),
        BinOp::Or => LirInstructionKind::Or(mov.src, rhs),
        BinOp::Xor => LirInstructionKind::Xor(mov.src, rhs),
        BinOp::Shl => LirInstructionKind::Shl(mov.src, rhs),
        BinOp::Shr => LirInstructionKind::Shr(mov.src, rhs),
    };
    Ok(Some((
        LirInstruction {
            id,
            kind,
            type_hint: Some(LirType::I64),
            debug_info: None,
        },
        2,
    )))
}

fn parse_not_pattern(
    lines: &[String],
    target: GoAsmTarget,
) -> Result<Option<(LirInstruction, usize)>> {
    if lines.len() < 2 {
        return Ok(None);
    }
    let Some(mov) = parse_move_line(&lines[0], target)? else {
        return Ok(None);
    };
    let Some(dst) = parse_not_line(&lines[1], target)? else {
        return Ok(None);
    };
    if dst != mov.dst {
        return Ok(None);
    }
    let id = dst
        .checked_sub(10)
        .ok_or_else(|| Error::from("goasm parse: invalid destination register"))?;
    Ok(Some((
        LirInstruction {
            id,
            kind: LirInstructionKind::Not(mov.src),
            type_hint: Some(LirType::I64),
            debug_info: None,
        },
        2,
    )))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BinOp {
    Add,
    Sub,
    Mul,
    And,
    Or,
    Xor,
    Shl,
    Shr,
}

struct ParsedMove {
    src: LirValue,
    dst: u32,
}

fn parse_move_line(line: &str, target: GoAsmTarget) -> Result<Option<ParsedMove>> {
    let line = line.trim_start_matches('\t').trim();
    let (mnemonic, rest) = split_mnemonic_operands(line)?;
    let expected_mov = match target {
        GoAsmTarget::Amd64 => "MOVQ",
        GoAsmTarget::Arm64 => "MOVD",
    };
    if mnemonic != expected_mov {
        return Ok(None);
    }
    let operands = split_operands(rest);
    if operands.len() != 2 {
        return Ok(None);
    }
    let Some(dst) = parse_register(&operands[1], target) else {
        return Ok(None);
    };
    let src = parse_value(Some(operands[0].as_str()), target, line)?;
    Ok(Some(ParsedMove { src, dst }))
}

fn parse_binop_line(line: &str, target: GoAsmTarget) -> Result<Option<(BinOp, LirValue, u32)>> {
    let line = line.trim_start_matches('\t').trim();
    let (mnemonic, rest) = split_mnemonic_operands(line)?;
    let op = match (mnemonic, target) {
        ("ADDQ", GoAsmTarget::Amd64) | ("ADD", GoAsmTarget::Arm64) => BinOp::Add,
        ("SUBQ", GoAsmTarget::Amd64) | ("SUB", GoAsmTarget::Arm64) => BinOp::Sub,
        ("IMULQ", GoAsmTarget::Amd64) | ("MUL", GoAsmTarget::Arm64) => BinOp::Mul,
        ("ANDQ", GoAsmTarget::Amd64) | ("AND", GoAsmTarget::Arm64) => BinOp::And,
        ("ORQ", GoAsmTarget::Amd64) | ("ORR", GoAsmTarget::Arm64) => BinOp::Or,
        ("XORQ", GoAsmTarget::Amd64) | ("EOR", GoAsmTarget::Arm64) => BinOp::Xor,
        ("SHLQ", GoAsmTarget::Amd64) | ("LSL", GoAsmTarget::Arm64) => BinOp::Shl,
        ("SHRQ", GoAsmTarget::Amd64) | ("LSR", GoAsmTarget::Arm64) => BinOp::Shr,
        _ => return Ok(None),
    };
    let operands = split_operands(rest);
    match target {
        GoAsmTarget::Amd64 => {
            if operands.len() != 2 {
                return Ok(None);
            }
            let rhs = parse_value(Some(operands[0].as_str()), target, line)?;
            let dst = parse_register(&operands[1], target)
                .ok_or_else(|| Error::from("goasm parse: missing destination register"))?;
            Ok(Some((op, rhs, dst)))
        }
        GoAsmTarget::Arm64 => {
            if operands.len() != 3 {
                return Ok(None);
            }
            let rhs = parse_value(Some(operands[0].as_str()), target, line)?;
            let dst = parse_register(&operands[1], target)
                .ok_or_else(|| Error::from("goasm parse: missing destination register"))?;
            let dst2 = parse_register(&operands[2], target)
                .ok_or_else(|| Error::from("goasm parse: missing destination register"))?;
            if dst != dst2 {
                return Ok(None);
            }
            Ok(Some((op, rhs, dst)))
        }
    }
}

fn parse_not_line(line: &str, target: GoAsmTarget) -> Result<Option<u32>> {
    let line = line.trim_start_matches('\t').trim();
    let (mnemonic, rest) = split_mnemonic_operands(line)?;
    match target {
        GoAsmTarget::Amd64 => {
            if mnemonic != "NOTQ" {
                return Ok(None);
            }
            let operands = split_operands(rest);
            if operands.len() != 1 {
                return Ok(None);
            }
            Ok(parse_register(&operands[0], target))
        }
        GoAsmTarget::Arm64 => {
            if mnemonic != "MVN" {
                return Ok(None);
            }
            let operands = split_operands(rest);
            if operands.len() != 2 {
                return Ok(None);
            }
            let dst = parse_register(&operands[0], target)
                .ok_or_else(|| Error::from("goasm parse: missing destination register"))?;
            let src = parse_register(&operands[1], target)
                .ok_or_else(|| Error::from("goasm parse: missing destination register"))?;
            if dst != src {
                return Ok(None);
            }
            Ok(Some(dst))
        }
    }
}

fn parse_terminator(
    lines: &[String],
    label_to_block: &HashMap<String, BasicBlockId>,
    target: GoAsmTarget,
) -> Result<LirTerminator> {
    if lines.len() >= 2 {
        if let Some(value) = parse_return_mov(&lines[0], target)? {
            if lines[1].trim() == "RET" {
                return Ok(LirTerminator::Return(Some(value)));
            }
        }
    }
    if lines
        .first()
        .is_some_and(|line| line.trim_start_matches('\t').trim() == "RET")
    {
        return Ok(LirTerminator::Return(None));
    }

    if let Some(target_label) = lines
        .first()
        .and_then(|line| parse_jmp(line).transpose())
        .transpose()?
    {
        return Ok(LirTerminator::Br(resolve_block(
            label_to_block,
            &target_label,
        )?));
    }

    if lines.len() >= 2 {
        if let Some((cond, true_label)) = parse_conditional_jump(&lines[0], target)? {
            if let Some(false_label) = parse_jmp(&lines[1])? {
                return Ok(LirTerminator::CondBr {
                    condition: cond,
                    if_true: resolve_block(label_to_block, &true_label)?,
                    if_false: resolve_block(label_to_block, &false_label)?,
                });
            }
        }
    }

    match lines.first() {
        Some(line) => Err(Error::from(format!(
            "goasm parse: unsupported terminator `{line}`"
        ))),
        None => Ok(LirTerminator::Return(None)),
    }
}

fn parse_return_mov(line: &str, target: GoAsmTarget) -> Result<Option<LirValue>> {
    let line = line.trim_start_matches('\t').trim();
    let (mnemonic, rest) = split_mnemonic_operands(line)?;
    let expected_mov = match target {
        GoAsmTarget::Amd64 => "MOVQ",
        GoAsmTarget::Arm64 => "MOVD",
    };
    if mnemonic != expected_mov {
        return Ok(None);
    }
    let operands = split_operands(rest);
    let Some(dst) = operands
        .last()
        .and_then(|op| parse_return_register(op, target))
    else {
        return Ok(None);
    };
    if !dst {
        return Ok(None);
    }
    Ok(Some(parse_value(
        operands.get(0).map(|value| value.as_str()),
        target,
        line,
    )?))
}

fn parse_conditional_jump(line: &str, target: GoAsmTarget) -> Result<Option<(LirValue, String)>> {
    let line = line.trim_start_matches('\t').trim();
    let (mnemonic, rest) = split_mnemonic_operands(line)?;
    let is_ne = match target {
        GoAsmTarget::Amd64 => mnemonic == "JNE",
        GoAsmTarget::Arm64 => mnemonic == "BNE",
    };
    let is_eq = match target {
        GoAsmTarget::Amd64 => mnemonic == "JEQ",
        GoAsmTarget::Arm64 => mnemonic == "BEQ",
    };
    if !is_ne && !is_eq {
        return Ok(None);
    }
    let operands = split_operands(rest);
    let Some(label) = operands.first() else {
        return Err(Error::from("goasm parse: missing conditional jump target"));
    };
    // Condition is always the last compare result register in the emitted form.
    // We recover it via the register that was written by the comparison pattern.
    // Here we just pass through a dummy; the compare peephole replaces it.
    Ok(Some((LirValue::Register(0), label.to_string())))
}

fn parse_jmp(line: &str) -> Result<Option<String>> {
    let line = line.trim_start_matches('\t').trim();
    let (mnemonic, rest) = split_mnemonic_operands(line)?;
    if mnemonic != "JMP" {
        return Ok(None);
    }
    let operands = split_operands(rest);
    let label = operands
        .first()
        .ok_or_else(|| Error::from("goasm parse: missing JMP target"))?;
    Ok(Some(label.to_string()))
}

fn parse_compare_pattern(
    lines: &[String],
    target: GoAsmTarget,
) -> Result<Option<(LirInstruction, usize)>> {
    if lines.len() < 6 {
        return Ok(None);
    }
    let expected_mov = match target {
        GoAsmTarget::Amd64 => "MOVQ",
        GoAsmTarget::Arm64 => "MOVD",
    };
    let expected_cmp = match target {
        GoAsmTarget::Amd64 => "CMPQ",
        GoAsmTarget::Arm64 => "CMP",
    };
    let expected_true_branch = match target {
        GoAsmTarget::Amd64 => ["JEQ", "JNE", "JLT", "JLE", "JGT", "JGE"],
        GoAsmTarget::Arm64 => ["BEQ", "BNE", "BLT", "BLE", "BGT", "BGE"],
    };

    let line0 = lines[0].trim_start_matches('\t').trim();
    let (mn0, rest0) = split_mnemonic_operands(line0)?;
    if mn0 != expected_mov {
        return Ok(None);
    }
    let ops0 = split_operands(rest0);
    if ops0.len() != 2 || ops0[0] != "$0" {
        return Ok(None);
    }
    let dst = parse_register(&ops0[1], target)
        .ok_or_else(|| Error::from("goasm parse: invalid compare dst"))?;
    let id = dst
        .checked_sub(10)
        .ok_or_else(|| Error::from("goasm parse: invalid compare dst"))?;

    let line1 = lines[1].trim_start_matches('\t').trim();
    let (mn1, rest1) = split_mnemonic_operands(line1)?;
    if mn1 != expected_cmp {
        return Ok(None);
    }
    let ops1 = split_operands(rest1);
    if ops1.len() != 2 {
        return Ok(None);
    }
    let (lhs_token, rhs_token) = match target {
        GoAsmTarget::Amd64 => (ops1[0].as_str(), ops1[1].as_str()),
        GoAsmTarget::Arm64 => (ops1[1].as_str(), ops1[0].as_str()),
    };
    let lhs = parse_value(Some(lhs_token), target, line1)?;
    let rhs = parse_value(Some(rhs_token), target, line1)?;

    let line2 = lines[2].trim_start_matches('\t').trim();
    let (mn2, rest2) = split_mnemonic_operands(line2)?;
    if !expected_true_branch.contains(&mn2) {
        return Ok(None);
    }
    let true_label = split_operands(rest2)
        .first()
        .ok_or_else(|| Error::from("goasm parse: missing compare true label"))?
        .to_string();

    let line3 = lines[3].trim_start_matches('\t').trim();
    let (mn3, rest3) = split_mnemonic_operands(line3)?;
    if mn3 != "JMP" {
        return Ok(None);
    }
    let done_label = split_operands(rest3)
        .first()
        .ok_or_else(|| Error::from("goasm parse: missing compare done label"))?
        .to_string();

    if lines[4].trim_end_matches(':') != true_label {
        return Ok(None);
    }

    let line5 = lines[5].trim_start_matches('\t').trim();
    let (mn5, rest5) = split_mnemonic_operands(line5)?;
    if mn5 != expected_mov {
        return Ok(None);
    }
    let ops5 = split_operands(rest5);
    if ops5.len() != 2 || ops5[0] != "$1" {
        return Ok(None);
    }
    let dst2 = parse_register(&ops5[1], target)
        .ok_or_else(|| Error::from("goasm parse: invalid compare dst"))?;
    if dst2 != dst {
        return Ok(None);
    }

    if lines
        .get(6)
        .is_some_and(|line| line.trim_end_matches(':') != done_label)
    {
        return Ok(None);
    }

    let kind = match mn2 {
        "JEQ" | "BEQ" => LirInstructionKind::Eq(lhs, rhs),
        "JNE" | "BNE" => LirInstructionKind::Ne(lhs, rhs),
        "JLT" | "BLT" => LirInstructionKind::Lt(lhs, rhs),
        "JLE" | "BLE" => LirInstructionKind::Le(lhs, rhs),
        "JGT" | "BGT" => LirInstructionKind::Gt(lhs, rhs),
        "JGE" | "BGE" => LirInstructionKind::Ge(lhs, rhs),
        _ => return Ok(None),
    };

    Ok(Some((
        LirInstruction {
            id,
            kind,
            type_hint: Some(LirType::I1),
            debug_info: None,
        },
        7,
    )))
}

fn parse_value(token: Option<&str>, target: GoAsmTarget, line: &str) -> Result<LirValue> {
    let token =
        token.ok_or_else(|| Error::from(format!("goasm parse: missing operand in `{line}`")))?;
    if token.starts_with('$') {
        let number = token
            .trim_start_matches('$')
            .parse::<i64>()
            .map_err(|_| Error::from(format!("goasm parse: invalid immediate `{token}`")))?;
        return Ok(LirValue::Constant(LirConstant::Int(number, LirType::I64)));
    }
    if let Some(reg) = parse_register(token, target) {
        let id = reg
            .checked_sub(10)
            .ok_or_else(|| Error::from("goasm parse: invalid vreg"))?;
        return Ok(LirValue::Register(id));
    }
    if parse_return_register(token, target).unwrap_or(false) {
        return Ok(LirValue::Register(0));
    }
    Err(Error::from(format!(
        "goasm parse: unsupported operand `{token}`"
    )))
}

fn parse_register(token: &str, target: GoAsmTarget) -> Option<u32> {
    let token = token.trim().trim_end_matches(',');
    if let Some(digits) = token.strip_prefix('R') {
        return digits.parse::<u32>().ok();
    }
    match target {
        GoAsmTarget::Amd64 => match token {
            "R8" => Some(8),
            "R9" => Some(9),
            _ => None,
        },
        GoAsmTarget::Arm64 => None,
    }
}

fn parse_return_register(token: &str, target: GoAsmTarget) -> Option<bool> {
    let token = token.trim().trim_end_matches(',');
    Some(match target {
        GoAsmTarget::Amd64 => token == "AX",
        GoAsmTarget::Arm64 => token == "R0",
    })
}

fn split_mnemonic_operands(line: &str) -> Result<(&str, &str)> {
    let line = line.trim();
    let mut parts = line.splitn(2, char::is_whitespace);
    let mnemonic = parts
        .next()
        .ok_or_else(|| Error::from("goasm parse: empty line"))?;
    let rest = parts.next().unwrap_or("").trim();
    Ok((mnemonic, rest))
}

fn split_operands(rest: &str) -> Vec<String> {
    if rest.is_empty() {
        return Vec::new();
    }
    rest.split(',')
        .map(|part| part.trim().to_string())
        .collect()
}

fn strip_comment(line: &str) -> &str {
    line.split("//").next().unwrap_or(line)
}

fn is_local_label(line: &str) -> bool {
    line.starts_with('L') && line.trim_end().ends_with(':')
}

fn is_terminator_start(line: &str) -> bool {
    let line = line.trim_start_matches('\t').trim();
    line == "RET" || line.starts_with("JMP")
}

fn resolve_block(map: &HashMap<String, BasicBlockId>, label: &str) -> Result<BasicBlockId> {
    map.get(label)
        .copied()
        .ok_or_else(|| Error::from(format!("goasm parse: unknown block label `{label}`")))
}

fn block_label_suffix<'a>(function_name: &Name, label: &'a str) -> Option<&'a str> {
    let prefix = format!("{}__", function_name.as_str());
    let prefix2 = format!("{}_", function_name.as_str());
    label
        .strip_prefix(&prefix)
        .or_else(|| label.strip_prefix(&prefix2))
}
