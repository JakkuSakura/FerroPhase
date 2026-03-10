use fp_core::asmir::{AsmBlockId, OperandAccess};
use fp_core::error::{Error, Result};
use fp_core::lir::Name;

use crate::asm::text::{
    parse_block_id, parse_i128, parse_u16, render_signed_offset, split_mnemonic_operands,
    split_operands, strip_comment,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AsmX86_64Program {
    pub functions: Vec<AsmX86_64Function>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AsmX86_64Function {
    pub name: Name,
    pub blocks: Vec<AsmX86_64Block>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AsmX86_64Block {
    pub id: AsmBlockId,
    pub instructions: Vec<X86InstructionDetail>,
    pub terminator: X86TerminatorDetail,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct X86InstructionDetail {
    pub opcode: X86Opcode,
    pub operands: Vec<X86Operand>,
    pub condition: Option<X86ConditionCode>,
    pub call_target: Option<X86CallTarget>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum X86Opcode {
    Add,
    Sub,
    IMul,
    IDiv,
    And,
    Or,
    Xor,
    Shl,
    Sar,
    Not,
    Cmp,
    Mov,
    Lea,
    LeaFrame,
    Cvtsi2sd,
    Cvttsd2si,
    Cvtss2sd,
    Cvtsd2ss,
    Mulss,
    Mulsd,
    Divss,
    Divsd,
    MovExtract,
    MovInsert,
    Call,
    PhiCopy,
    CMov,
    InlineAsm,
    LandingPad,
    Ud2,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum X86Operand {
    Register { reg: X86Register, access: OperandAccess },
    Immediate(i128),
    Memory(X86MemoryOperand),
    Block(AsmBlockId),
    Symbol(Name),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum X86Register {
    Physical { name: String, size_bits: u16 },
    Virtual { id: u32, size_bits: u16 },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct X86MemoryOperand {
    pub base: Option<X86Register>,
    pub index: Option<X86Register>,
    pub scale: u8,
    pub displacement: i64,
    pub size_bytes: Option<u16>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum X86ConditionCode {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    NonZero,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum X86CallTarget {
    Symbol(Name),
    Register(X86Register),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct X86TerminatorDetail {
    pub opcode: X86TerminatorOpcode,
    pub condition: Option<X86ConditionCode>,
    pub targets: Vec<AsmBlockId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum X86TerminatorOpcode {
    Ret,
    Jmp,
    Jcc,
    Switch,
    IndirectJmp,
    Invoke,
    Resume,
    CleanupRet,
    CatchRet,
    CatchSwitch,
    Ud2,
}

impl X86Opcode {
    pub fn mnemonic(&self) -> &str {
        match self {
            X86Opcode::Add => "add",
            X86Opcode::Sub => "sub",
            X86Opcode::IMul => "imul",
            X86Opcode::IDiv => "idiv",
            X86Opcode::And => "and",
            X86Opcode::Or => "or",
            X86Opcode::Xor => "xor",
            X86Opcode::Shl => "shl",
            X86Opcode::Sar => "sar",
            X86Opcode::Not => "not",
            X86Opcode::Cmp => "cmp",
            X86Opcode::Mov => "mov",
            X86Opcode::Lea => "lea",
            X86Opcode::LeaFrame => "lea.frame",
            X86Opcode::Cvtsi2sd => "cvtsi2sd",
            X86Opcode::Cvttsd2si => "cvttsd2si",
            X86Opcode::Cvtss2sd => "cvtss2sd",
            X86Opcode::Cvtsd2ss => "cvtsd2ss",
            X86Opcode::Mulss => "mulss",
            X86Opcode::Mulsd => "mulsd",
            X86Opcode::Divss => "divss",
            X86Opcode::Divsd => "divsd",
            X86Opcode::MovExtract => "mov.extract",
            X86Opcode::MovInsert => "mov.insert",
            X86Opcode::Call => "call",
            X86Opcode::PhiCopy => "phi.copy",
            X86Opcode::CMov => "cmov",
            X86Opcode::InlineAsm => "inlineasm",
            X86Opcode::LandingPad => "landingpad",
            X86Opcode::Ud2 => "ud2",
        }
    }
}

impl AsmX86_64Program {
    pub fn to_text(&self) -> String {
        let mut out = String::new();
        for (index, function) in self.functions.iter().enumerate() {
            if index > 0 {
                out.push('\n');
            }
            out.push_str(&format!(".globl {}\n", function.name));
            out.push_str(&format!("{}:\n", function.name));
            for block in &function.blocks {
                out.push_str(&format!("bb{}:\n", block.id));
                for instruction in &block.instructions {
                    out.push_str("    ");
                    out.push_str(&format_instruction(instruction));
                    out.push('\n');
                }
                out.push_str("    ");
                out.push_str(&format_terminator(&block.terminator));
                out.push('\n');
            }
        }
        out
    }

    pub fn parse_text(text: &str) -> Result<Self> {
        let mut parser = Parser::default();
        for raw_line in text.lines() {
            parser.push_line(raw_line)?;
        }
        parser.finish()
    }
}

fn format_instruction(inst: &X86InstructionDetail) -> String {
    let mnemonic = instruction_mnemonic(inst);
    if inst.operands.is_empty() {
        mnemonic
    } else {
        format!("{} {}", mnemonic, inst.operands.iter().map(format_operand).collect::<Vec<_>>().join(", "))
    }
}

fn instruction_mnemonic(inst: &X86InstructionDetail) -> String {
    match inst.condition.as_ref() {
        Some(condition) if matches!(inst.opcode, X86Opcode::Cmp | X86Opcode::CMov) => {
            format!("{}.{}", inst.opcode.mnemonic(), format_condition(condition))
        }
        _ => inst.opcode.mnemonic().to_string(),
    }
}

fn format_terminator(term: &X86TerminatorDetail) -> String {
    let mnemonic = match term.opcode {
        X86TerminatorOpcode::Jcc => format!(
            "jcc.{}",
            format_condition(term.condition.as_ref().unwrap_or(&X86ConditionCode::NonZero))
        ),
        X86TerminatorOpcode::Ret => "ret".to_string(),
        X86TerminatorOpcode::Jmp => "jmp".to_string(),
        X86TerminatorOpcode::Switch => "switch".to_string(),
        X86TerminatorOpcode::IndirectJmp => "indirectjmp".to_string(),
        X86TerminatorOpcode::Invoke => "invoke".to_string(),
        X86TerminatorOpcode::Resume => "resume".to_string(),
        X86TerminatorOpcode::CleanupRet => "cleanupret".to_string(),
        X86TerminatorOpcode::CatchRet => "catchret".to_string(),
        X86TerminatorOpcode::CatchSwitch => "catchswitch".to_string(),
        X86TerminatorOpcode::Ud2 => "ud2".to_string(),
    };
    if term.targets.is_empty() {
        mnemonic
    } else {
        format!("{} {}", mnemonic, term.targets.iter().map(|id| format!("bb{id}")).collect::<Vec<_>>().join(", "))
    }
}

fn format_operand(operand: &X86Operand) -> String {
    match operand {
        X86Operand::Register { reg, .. } => format_register(reg),
        X86Operand::Immediate(value) => value.to_string(),
        X86Operand::Memory(mem) => format_memory(mem),
        X86Operand::Block(id) => format!("bb{id}"),
        X86Operand::Symbol(name) => name.to_string(),
    }
}

fn format_register(reg: &X86Register) -> String {
    match reg {
        X86Register::Physical { name, .. } => name.clone(),
        X86Register::Virtual { id, size_bits } => format!("v{id}:{size_bits}"),
    }
}

fn format_memory(mem: &X86MemoryOperand) -> String {
    let mut inner = String::new();
    if let Some(base) = &mem.base {
        inner.push_str(&format_register(base));
    }
    if let Some(index) = &mem.index {
        if !inner.is_empty() {
            inner.push_str(" + ");
        }
        inner.push_str(&format_register(index));
        if mem.scale != 1 {
            inner.push('*');
            inner.push_str(&mem.scale.to_string());
        }
    }
    if inner.is_empty() {
        inner.push('0');
    }
    inner.push_str(&render_signed_offset(mem.displacement));
    match mem.size_bytes {
        Some(size_bytes) => format!("[{inner}]:{size_bytes}"),
        None => format!("[{inner}]"),
    }
}

fn format_condition(condition: &X86ConditionCode) -> &'static str {
    match condition {
        X86ConditionCode::Equal => "eq",
        X86ConditionCode::NotEqual => "ne",
        X86ConditionCode::Less => "lt",
        X86ConditionCode::LessEqual => "le",
        X86ConditionCode::Greater => "gt",
        X86ConditionCode::GreaterEqual => "ge",
        X86ConditionCode::NonZero => "nz",
    }
}

fn parse_condition(token: &str) -> Result<X86ConditionCode> {
    match token {
        "eq" => Ok(X86ConditionCode::Equal),
        "ne" => Ok(X86ConditionCode::NotEqual),
        "lt" => Ok(X86ConditionCode::Less),
        "le" => Ok(X86ConditionCode::LessEqual),
        "gt" => Ok(X86ConditionCode::Greater),
        "ge" => Ok(X86ConditionCode::GreaterEqual),
        "nz" => Ok(X86ConditionCode::NonZero),
        _ => Err(Error::from(format!("unknown x86 condition: {token}"))),
    }
}

#[derive(Default)]
struct Parser {
    functions: Vec<AsmX86_64Function>,
    current_function: Option<AsmX86_64Function>,
    current_block: Option<AsmX86_64Block>,
}

impl Parser {
    fn push_line(&mut self, raw_line: &str) -> Result<()> {
        let line = strip_comment(raw_line);
        if line.is_empty() {
            return Ok(());
        }
        if let Some(symbol) = line.strip_prefix(".globl ") {
            self.start_function(Name::new(symbol.trim()))?;
            return Ok(());
        }
        if let Some(label) = line.strip_suffix(':') {
            if let Some(id) = label.strip_prefix("bb") {
                let block_id = id
                    .parse::<u32>()
                    .map_err(|_| Error::from(format!("invalid x86 block label: {label}")))?;
                self.start_block(block_id)?;
            } else {
                self.start_function(Name::new(label.trim()))?;
            }
            return Ok(());
        }
        self.push_statement(line)
    }

    fn finish(mut self) -> Result<AsmX86_64Program> {
        self.finish_block()?;
        self.finish_function()?;
        Ok(AsmX86_64Program {
            functions: self.functions,
        })
    }

    fn start_function(&mut self, name: Name) -> Result<()> {
        self.finish_block()?;
        if matches!(self.current_function.as_ref(), Some(function) if function.name == name) {
            return Ok(());
        }
        self.finish_function()?;
        self.current_function = Some(AsmX86_64Function {
            name,
            blocks: Vec::new(),
        });
        Ok(())
    }

    fn start_block(&mut self, id: u32) -> Result<()> {
        self.finish_block()?;
        let function = self
            .current_function
            .as_ref()
            .ok_or_else(|| Error::from("x86 block seen before function"))?;
        let _ = function;
        self.current_block = Some(AsmX86_64Block {
            id,
            instructions: Vec::new(),
            terminator: X86TerminatorDetail {
                opcode: X86TerminatorOpcode::Ud2,
                condition: None,
                targets: Vec::new(),
            },
        });
        Ok(())
    }

    fn push_statement(&mut self, line: &str) -> Result<()> {
        let block = self
            .current_block
            .as_mut()
            .ok_or_else(|| Error::from("x86 instruction seen before block"))?;
        if is_terminator_line(line) {
            block.terminator = parse_terminator(line)?;
            return Ok(());
        }
        block.instructions.push(parse_instruction(line)?);
        Ok(())
    }

    fn finish_block(&mut self) -> Result<()> {
        if let Some(block) = self.current_block.take() {
            let function = self
                .current_function
                .as_mut()
                .ok_or_else(|| Error::from("x86 block without function"))?;
            function.blocks.push(block);
        }
        Ok(())
    }

    fn finish_function(&mut self) -> Result<()> {
        if let Some(function) = self.current_function.take() {
            self.functions.push(function);
        }
        Ok(())
    }
}

fn is_terminator_line(line: &str) -> bool {
    let (mnemonic, _) = split_mnemonic_operands(line);
    matches!(
        mnemonic,
        "ret" | "jmp" | "resume" | "ud2" | "switch" | "indirectjmp" | "invoke"
            | "cleanupret" | "catchret" | "catchswitch"
    ) || mnemonic.starts_with("jcc.")
}

fn parse_instruction(line: &str) -> Result<X86InstructionDetail> {
    let (mnemonic, operands_text) = split_mnemonic_operands(line);
    let operands = operands_text.map(split_operands).unwrap_or_default();
    let (base, suffix) = mnemonic.split_once('.').unwrap_or((mnemonic, ""));
    let opcode = parse_opcode(base)?;
    let condition = match opcode {
        X86Opcode::Cmp | X86Opcode::CMov if !suffix.is_empty() => Some(parse_condition(suffix)?),
        _ => None,
    };
    let mut parsed_operands = Vec::with_capacity(operands.len());
    for (index, operand) in operands.iter().enumerate() {
        parsed_operands.push(parse_operand(operand, operand_access(&opcode, index))?);
    }
    let call_target = if opcode == X86Opcode::Call {
        parsed_operands.first().map(call_target_from_operand).transpose()?
    } else {
        None
    };
    Ok(X86InstructionDetail {
        opcode,
        operands: parsed_operands,
        condition,
        call_target,
    })
}

fn parse_terminator(line: &str) -> Result<X86TerminatorDetail> {
    let (mnemonic, operands_text) = split_mnemonic_operands(line);
    let operands = operands_text.map(split_operands).unwrap_or_default();
    let (opcode, condition) = if let Some(cond) = mnemonic.strip_prefix("jcc.") {
        (X86TerminatorOpcode::Jcc, Some(parse_condition(cond)?))
    } else {
        (
            match mnemonic {
                "ret" => X86TerminatorOpcode::Ret,
                "jmp" => X86TerminatorOpcode::Jmp,
                "switch" => X86TerminatorOpcode::Switch,
                "indirectjmp" => X86TerminatorOpcode::IndirectJmp,
                "invoke" => X86TerminatorOpcode::Invoke,
                "resume" => X86TerminatorOpcode::Resume,
                "cleanupret" => X86TerminatorOpcode::CleanupRet,
                "catchret" => X86TerminatorOpcode::CatchRet,
                "catchswitch" => X86TerminatorOpcode::CatchSwitch,
                "ud2" => X86TerminatorOpcode::Ud2,
                _ => return Err(Error::from(format!("unknown x86 terminator: {mnemonic}"))),
            },
            None,
        )
    };
    let targets = operands
        .iter()
        .map(|operand| parse_block_id(operand))
        .collect::<Result<Vec<_>>>()?;
    Ok(X86TerminatorDetail {
        opcode,
        condition,
        targets,
    })
}

fn parse_opcode(token: &str) -> Result<X86Opcode> {
    match token {
        "add" => Ok(X86Opcode::Add),
        "sub" => Ok(X86Opcode::Sub),
        "imul" => Ok(X86Opcode::IMul),
        "idiv" => Ok(X86Opcode::IDiv),
        "and" => Ok(X86Opcode::And),
        "or" => Ok(X86Opcode::Or),
        "xor" => Ok(X86Opcode::Xor),
        "shl" => Ok(X86Opcode::Shl),
        "sar" => Ok(X86Opcode::Sar),
        "not" => Ok(X86Opcode::Not),
        "cmp" => Ok(X86Opcode::Cmp),
        "mov" => Ok(X86Opcode::Mov),
        "lea" => Ok(X86Opcode::Lea),
        "lea.frame" => Ok(X86Opcode::LeaFrame),
        "cvtsi2sd" => Ok(X86Opcode::Cvtsi2sd),
        "cvttsd2si" => Ok(X86Opcode::Cvttsd2si),
        "cvtss2sd" => Ok(X86Opcode::Cvtss2sd),
        "cvtsd2ss" => Ok(X86Opcode::Cvtsd2ss),
        "mulss" => Ok(X86Opcode::Mulss),
        "mulsd" => Ok(X86Opcode::Mulsd),
        "divss" => Ok(X86Opcode::Divss),
        "divsd" => Ok(X86Opcode::Divsd),
        "mov.extract" => Ok(X86Opcode::MovExtract),
        "mov.insert" => Ok(X86Opcode::MovInsert),
        "call" => Ok(X86Opcode::Call),
        "phi.copy" => Ok(X86Opcode::PhiCopy),
        "cmov" => Ok(X86Opcode::CMov),
        "inlineasm" => Ok(X86Opcode::InlineAsm),
        "landingpad" => Ok(X86Opcode::LandingPad),
        "ud2" => Ok(X86Opcode::Ud2),
        _ => Err(Error::from(format!("unknown x86 opcode: {token}"))),
    }
}

fn operand_access(opcode: &X86Opcode, index: usize) -> OperandAccess {
    if matches!(opcode, X86Opcode::Cmp | X86Opcode::Call | X86Opcode::InlineAsm | X86Opcode::LandingPad) {
        OperandAccess::Read
    } else if index == 0 {
        OperandAccess::Write
    } else {
        OperandAccess::Read
    }
}

fn parse_operand(token: &str, access: OperandAccess) -> Result<X86Operand> {
    let token = token.trim();
    if token.starts_with('[') {
        return Ok(X86Operand::Memory(parse_memory(token)?));
    }
    if token.starts_with("bb") {
        return Ok(X86Operand::Block(parse_block_id(token)?));
    }
    if token.starts_with('v') && token.contains(':') {
        return Ok(X86Operand::Register {
            reg: parse_register(token)?,
            access,
        });
    }
    if let Ok(value) = parse_i128(token) {
        return Ok(X86Operand::Immediate(value));
    }
    if is_register_name(token) {
        return Ok(X86Operand::Register {
            reg: parse_register(token)?,
            access,
        });
    }
    Ok(X86Operand::Symbol(Name::new(token.to_string())))
}

fn parse_register(token: &str) -> Result<X86Register> {
    if let Some((id, size_bits)) = token.strip_prefix('v').and_then(|rest| rest.split_once(':')) {
        return Ok(X86Register::Virtual {
            id: id
                .parse::<u32>()
                .map_err(|_| Error::from(format!("invalid x86 virtual register: {token}")))?,
            size_bits: parse_u16(size_bits, "x86 register size")?,
        });
    }
    Ok(X86Register::Physical {
        name: token.to_string(),
        size_bits: infer_physical_size(token),
    })
}

fn infer_physical_size(name: &str) -> u16 {
    if name.starts_with("xmm") {
        128
    } else if name.starts_with('e') {
        32
    } else if matches!(name, "ax" | "bx" | "cx" | "dx" | "si" | "di" | "sp" | "bp") {
        16
    } else if name.ends_with('l') || name.ends_with('h') {
        8
    } else {
        64
    }
}

fn is_register_name(token: &str) -> bool {
    token.starts_with('r') || token.starts_with('e') || token.starts_with("xmm") || matches!(token, "ax" | "bx" | "cx" | "dx" | "si" | "di" | "sp" | "bp" | "al" | "ah" | "bl" | "bh" | "cl" | "ch" | "dl" | "dh")
}

fn parse_memory(token: &str) -> Result<X86MemoryOperand> {
    let (inner, size_bytes) = if let Some(index) = token.rfind(":") {
        if token[..index].ends_with(']') {
            (
                &token[..index],
                Some(parse_u16(&token[index + 1..], "x86 memory size")?),
            )
        } else {
            (token, None)
        }
    } else {
        (token, None)
    };
    let inner = inner
        .strip_prefix('[')
        .and_then(|rest| rest.strip_suffix(']'))
        .ok_or_else(|| Error::from(format!("invalid x86 memory operand: {token}")))?
        .trim();

    let normalized = inner.replace('-', "+-");
    let mut base = None;
    let mut index = None;
    let mut scale = 1u8;
    let mut displacement = 0i64;
    for part in normalized.split('+').map(str::trim).filter(|part| !part.is_empty()) {
        if let Ok(value) = part.parse::<i64>() {
            displacement += value;
            continue;
        }
        if let Some((reg, factor)) = part.split_once('*') {
            index = Some(parse_register(reg.trim())?);
            scale = factor
                .trim()
                .parse::<u8>()
                .map_err(|_| Error::from(format!("invalid x86 scale: {token}")))?;
            continue;
        }
        let reg = parse_register(part)?;
        if base.is_none() {
            base = Some(reg);
        } else {
            index = Some(reg);
        }
    }
    Ok(X86MemoryOperand {
        base,
        index,
        scale,
        displacement,
        size_bytes,
    })
}

fn call_target_from_operand(operand: &X86Operand) -> Result<X86CallTarget> {
    match operand {
        X86Operand::Symbol(name) => Ok(X86CallTarget::Symbol(name.clone())),
        X86Operand::Register { reg, .. } => Ok(X86CallTarget::Register(reg.clone())),
        _ => Err(Error::from("x86 call target must be symbol or register")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn x86_text_roundtrip_preserves_structure() {
        let program = AsmX86_64Program {
            functions: vec![AsmX86_64Function {
                name: Name::new("main"),
                blocks: vec![AsmX86_64Block {
                    id: 0,
                    instructions: vec![
                        X86InstructionDetail {
                            opcode: X86Opcode::Add,
                            operands: vec![
                                X86Operand::Register {
                                    reg: X86Register::Virtual { id: 1, size_bits: 64 },
                                    access: OperandAccess::Write,
                                },
                                X86Operand::Register {
                                    reg: X86Register::Physical {
                                        name: "rax".to_string(),
                                        size_bits: 64,
                                    },
                                    access: OperandAccess::Read,
                                },
                                X86Operand::Immediate(4),
                            ],
                            condition: None,
                            call_target: None,
                        },
                        X86InstructionDetail {
                            opcode: X86Opcode::Call,
                            operands: vec![X86Operand::Symbol(Name::new("callee"))],
                            condition: None,
                            call_target: Some(X86CallTarget::Symbol(Name::new("callee"))),
                        },
                    ],
                    terminator: X86TerminatorDetail {
                        opcode: X86TerminatorOpcode::Jcc,
                        condition: Some(X86ConditionCode::Equal),
                        targets: vec![1, 2],
                    },
                }],
            }],
        };

        let text = program.to_text();
        let parsed = AsmX86_64Program::parse_text(&text).unwrap();
        assert_eq!(parsed, program);
    }
}
