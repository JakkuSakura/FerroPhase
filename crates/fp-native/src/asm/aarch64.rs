use fp_core::asmir::{AsmBlockId, OperandAccess};
use fp_core::error::{Error, Result};
use fp_core::lir::Name;

use crate::asm::text::{
    parse_block_id, parse_i128, parse_u16, split_mnemonic_operands, split_operands, strip_comment,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AsmAarch64Program {
    pub functions: Vec<AsmAarch64Function>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AsmAarch64Function {
    pub name: Name,
    pub blocks: Vec<AsmAarch64Block>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AsmAarch64Block {
    pub id: AsmBlockId,
    pub instructions: Vec<Aarch64InstructionDetail>,
    pub terminator: Aarch64TerminatorDetail,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Aarch64InstructionDetail {
    pub opcode: String,
    pub operands: Vec<Aarch64Operand>,
    pub condition: Option<Aarch64ConditionCode>,
    pub call_target: Option<Aarch64CallTarget>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Aarch64Operand {
    Register {
        reg: Aarch64Register,
        access: OperandAccess,
    },
    Immediate(i128),
    Memory(Aarch64MemoryOperand),
    Block(AsmBlockId),
    Symbol(Name),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Aarch64Register {
    Physical { name: String, size_bits: u16 },
    Virtual { id: u32, size_bits: u16 },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Aarch64MemoryOperand {
    pub base: Option<Aarch64Register>,
    pub index: Option<Aarch64Register>,
    pub scale: u8,
    pub displacement: i64,
    pub size_bytes: Option<u16>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Aarch64ConditionCode {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    NonZero,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Aarch64CallTarget {
    Symbol(Name),
    Register(Aarch64Register),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Aarch64TerminatorDetail {
    pub opcode: Aarch64TerminatorOpcode,
    pub condition: Option<Aarch64ConditionCode>,
    pub targets: Vec<AsmBlockId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Aarch64TerminatorOpcode {
    Ret,
    B,
    BCond,
    Br,
    Switch,
    Invoke,
    Resume,
    CleanupRet,
    CatchRet,
    CatchSwitch,
    Brk,
}

impl AsmAarch64Program {
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

fn format_instruction(inst: &Aarch64InstructionDetail) -> String {
    let mnemonic = match inst.condition.as_ref() {
        Some(condition) if matches!(inst.opcode.as_str(), "cmp" | "csel") => {
            format!("{}.{}", inst.opcode, format_condition(condition))
        }
        _ => inst.opcode.clone(),
    };
    if inst.operands.is_empty() {
        mnemonic
    } else {
        format!(
            "{} {}",
            mnemonic,
            inst.operands
                .iter()
                .map(format_operand)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

fn format_terminator(term: &Aarch64TerminatorDetail) -> String {
    let mnemonic = match term.opcode {
        Aarch64TerminatorOpcode::BCond => format!(
            "b.{}",
            format_condition(
                term.condition
                    .as_ref()
                    .unwrap_or(&Aarch64ConditionCode::NonZero)
            )
        ),
        Aarch64TerminatorOpcode::Ret => "ret".to_string(),
        Aarch64TerminatorOpcode::B => "b".to_string(),
        Aarch64TerminatorOpcode::Br => "br".to_string(),
        Aarch64TerminatorOpcode::Switch => "switch".to_string(),
        Aarch64TerminatorOpcode::Invoke => "invoke".to_string(),
        Aarch64TerminatorOpcode::Resume => "resume".to_string(),
        Aarch64TerminatorOpcode::CleanupRet => "cleanupret".to_string(),
        Aarch64TerminatorOpcode::CatchRet => "catchret".to_string(),
        Aarch64TerminatorOpcode::CatchSwitch => "catchswitch".to_string(),
        Aarch64TerminatorOpcode::Brk => "brk".to_string(),
    };
    if term.targets.is_empty() {
        mnemonic
    } else {
        format!(
            "{} {}",
            mnemonic,
            term.targets
                .iter()
                .map(|id| format!("bb{id}"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

fn format_operand(operand: &Aarch64Operand) -> String {
    match operand {
        Aarch64Operand::Register { reg, .. } => format_register(reg),
        Aarch64Operand::Immediate(value) => format!("#{value}"),
        Aarch64Operand::Memory(mem) => format_memory(mem),
        Aarch64Operand::Block(id) => format!("bb{id}"),
        Aarch64Operand::Symbol(name) => name.to_string(),
    }
}

fn format_register(reg: &Aarch64Register) -> String {
    match reg {
        Aarch64Register::Physical { name, .. } => name.clone(),
        Aarch64Register::Virtual { id, size_bits } => format!("v{id}:{size_bits}"),
    }
}

fn format_memory(mem: &Aarch64MemoryOperand) -> String {
    let mut inner = String::new();
    if let Some(base) = &mem.base {
        inner.push_str(&format_register(base));
    }
    if let Some(index) = &mem.index {
        if !inner.is_empty() {
            inner.push_str(", ");
        }
        inner.push_str(&format_register(index));
        if mem.scale != 1 {
            inner.push_str(", lsl #");
            inner.push_str(&mem.scale.to_string());
        }
    }
    if mem.displacement != 0 {
        if !inner.is_empty() {
            inner.push_str(", #");
            inner.push_str(&mem.displacement.to_string());
        } else {
            inner.push_str(&format!("#{}", mem.displacement));
        }
    }
    if inner.is_empty() {
        inner.push_str("#0");
    }
    match mem.size_bytes {
        Some(size_bytes) => format!("[{inner}]:{size_bytes}"),
        None => format!("[{inner}]"),
    }
}

fn format_condition(condition: &Aarch64ConditionCode) -> &'static str {
    match condition {
        Aarch64ConditionCode::Eq => "eq",
        Aarch64ConditionCode::Ne => "ne",
        Aarch64ConditionCode::Lt => "lt",
        Aarch64ConditionCode::Le => "le",
        Aarch64ConditionCode::Gt => "gt",
        Aarch64ConditionCode::Ge => "ge",
        Aarch64ConditionCode::NonZero => "nz",
    }
}

fn parse_condition(token: &str) -> Result<Aarch64ConditionCode> {
    match token {
        "eq" => Ok(Aarch64ConditionCode::Eq),
        "ne" => Ok(Aarch64ConditionCode::Ne),
        "lt" => Ok(Aarch64ConditionCode::Lt),
        "le" => Ok(Aarch64ConditionCode::Le),
        "gt" => Ok(Aarch64ConditionCode::Gt),
        "ge" => Ok(Aarch64ConditionCode::Ge),
        "nz" => Ok(Aarch64ConditionCode::NonZero),
        _ => Err(Error::from(format!("unknown aarch64 condition: {token}"))),
    }
}

#[derive(Default)]
struct Parser {
    functions: Vec<AsmAarch64Function>,
    current_function: Option<AsmAarch64Function>,
    current_block: Option<AsmAarch64Block>,
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
                    .map_err(|_| Error::from(format!("invalid aarch64 block label: {label}")))?;
                self.start_block(block_id)?;
            } else {
                self.start_function(Name::new(label.trim()))?;
            }
            return Ok(());
        }
        self.push_statement(line)
    }

    fn finish(mut self) -> Result<AsmAarch64Program> {
        self.finish_block()?;
        self.finish_function()?;
        Ok(AsmAarch64Program {
            functions: self.functions,
        })
    }

    fn start_function(&mut self, name: Name) -> Result<()> {
        self.finish_block()?;
        if matches!(self.current_function.as_ref(), Some(function) if function.name == name) {
            return Ok(());
        }
        self.finish_function()?;
        self.current_function = Some(AsmAarch64Function {
            name,
            blocks: Vec::new(),
        });
        Ok(())
    }

    fn start_block(&mut self, id: u32) -> Result<()> {
        self.finish_block()?;
        self.current_function
            .as_ref()
            .ok_or_else(|| Error::from("aarch64 block seen before function"))?;
        self.current_block = Some(AsmAarch64Block {
            id,
            instructions: Vec::new(),
            terminator: Aarch64TerminatorDetail {
                opcode: Aarch64TerminatorOpcode::Brk,
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
            .ok_or_else(|| Error::from("aarch64 instruction seen before block"))?;
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
                .ok_or_else(|| Error::from("aarch64 block without function"))?;
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
        "ret"
            | "b"
            | "br"
            | "switch"
            | "invoke"
            | "resume"
            | "cleanupret"
            | "catchret"
            | "catchswitch"
            | "brk"
    ) || mnemonic.starts_with("b.")
}

fn parse_instruction(line: &str) -> Result<Aarch64InstructionDetail> {
    let (mnemonic, operands_text) = split_mnemonic_operands(line);
    let operands = operands_text.map(split_operands).unwrap_or_default();
    let (opcode, condition) = match mnemonic.split_once('.') {
        Some((base, suffix)) if matches!(base, "cmp" | "csel") => {
            (base.to_string(), Some(parse_condition(suffix)?))
        }
        _ => (mnemonic.to_string(), None),
    };
    let mut parsed_operands = Vec::with_capacity(operands.len());
    for (index, operand) in operands.iter().enumerate() {
        parsed_operands.push(parse_operand(operand, operand_access(&opcode, index))?);
    }
    let call_target = if opcode == "bl" {
        parsed_operands
            .first()
            .map(call_target_from_operand)
            .transpose()?
    } else {
        None
    };
    Ok(Aarch64InstructionDetail {
        opcode,
        operands: parsed_operands,
        condition,
        call_target,
    })
}

fn parse_terminator(line: &str) -> Result<Aarch64TerminatorDetail> {
    let (mnemonic, operands_text) = split_mnemonic_operands(line);
    let operands = operands_text.map(split_operands).unwrap_or_default();
    let (opcode, condition) = if let Some(cond) = mnemonic.strip_prefix("b.") {
        (Aarch64TerminatorOpcode::BCond, Some(parse_condition(cond)?))
    } else {
        (
            match mnemonic {
                "ret" => Aarch64TerminatorOpcode::Ret,
                "b" => Aarch64TerminatorOpcode::B,
                "br" => Aarch64TerminatorOpcode::Br,
                "switch" => Aarch64TerminatorOpcode::Switch,
                "invoke" => Aarch64TerminatorOpcode::Invoke,
                "resume" => Aarch64TerminatorOpcode::Resume,
                "cleanupret" => Aarch64TerminatorOpcode::CleanupRet,
                "catchret" => Aarch64TerminatorOpcode::CatchRet,
                "catchswitch" => Aarch64TerminatorOpcode::CatchSwitch,
                "brk" => Aarch64TerminatorOpcode::Brk,
                _ => {
                    return Err(Error::from(format!(
                        "unknown aarch64 terminator: {mnemonic}"
                    )));
                }
            },
            None,
        )
    };
    let targets = operands
        .iter()
        .map(|operand| parse_block_id(operand))
        .collect::<Result<Vec<_>>>()?;
    Ok(Aarch64TerminatorDetail {
        opcode,
        condition,
        targets,
    })
}

fn operand_access(opcode: &str, index: usize) -> OperandAccess {
    if matches!(opcode, "cmp" | "str" | "bl") {
        OperandAccess::Read
    } else if index == 0 {
        OperandAccess::Write
    } else {
        OperandAccess::Read
    }
}

fn parse_operand(token: &str, access: OperandAccess) -> Result<Aarch64Operand> {
    let token = token.trim();
    if token.starts_with('[') {
        return Ok(Aarch64Operand::Memory(parse_memory(token)?));
    }
    if token.starts_with("bb") {
        return Ok(Aarch64Operand::Block(parse_block_id(token)?));
    }
    if token.starts_with('#') || token.parse::<i128>().is_ok() {
        return Ok(Aarch64Operand::Immediate(parse_i128(token)?));
    }
    if token.starts_with('v') && token.contains(':') || is_register_name(token) {
        return Ok(Aarch64Operand::Register {
            reg: parse_register(token)?,
            access,
        });
    }
    Ok(Aarch64Operand::Symbol(Name::new(token.to_string())))
}

fn parse_register(token: &str) -> Result<Aarch64Register> {
    if let Some((id, size_bits)) = token
        .strip_prefix('v')
        .and_then(|rest| rest.split_once(':'))
    {
        return Ok(Aarch64Register::Virtual {
            id: id
                .parse::<u32>()
                .map_err(|_| Error::from(format!("invalid aarch64 virtual register: {token}")))?,
            size_bits: parse_u16(size_bits, "aarch64 register size")?,
        });
    }
    Ok(Aarch64Register::Physical {
        name: token.to_string(),
        size_bits: infer_physical_size(token),
    })
}

fn infer_physical_size(name: &str) -> u16 {
    match name.chars().next() {
        Some('w') | Some('s') => 32,
        Some('x') | Some('d') => 64,
        Some('q') | Some('v') => 128,
        _ => 64,
    }
}

fn is_register_name(token: &str) -> bool {
    matches!(token.chars().next(), Some('x' | 'w' | 's' | 'd' | 'q'))
}

fn parse_memory(token: &str) -> Result<Aarch64MemoryOperand> {
    let (inner, size_bytes) = if let Some(index) = token.rfind(':') {
        if token[..index].ends_with(']') {
            (
                &token[..index],
                Some(parse_u16(&token[index + 1..], "aarch64 memory size")?),
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
        .ok_or_else(|| Error::from(format!("invalid aarch64 memory operand: {token}")))?
        .trim();
    let parts = split_operands(inner);
    let mut base = None;
    let mut index = None;
    let mut scale = 1u8;
    let mut displacement = 0i64;
    for part in parts {
        let part = part.trim();
        if let Some(amount) = part.strip_prefix("lsl #") {
            scale = amount
                .parse::<u8>()
                .map_err(|_| Error::from(format!("invalid aarch64 shift: {part}")))?;
        } else if part.starts_with('#') || part.parse::<i64>().is_ok() {
            displacement = part
                .trim_start_matches('#')
                .parse::<i64>()
                .map_err(|_| Error::from(format!("invalid aarch64 displacement: {part}")))?;
        } else {
            let reg = parse_register(part)?;
            if base.is_none() {
                base = Some(reg);
            } else {
                index = Some(reg);
            }
        }
    }
    Ok(Aarch64MemoryOperand {
        base,
        index,
        scale,
        displacement,
        size_bytes,
    })
}

fn call_target_from_operand(operand: &Aarch64Operand) -> Result<Aarch64CallTarget> {
    match operand {
        Aarch64Operand::Symbol(name) => Ok(Aarch64CallTarget::Symbol(name.clone())),
        Aarch64Operand::Register { reg, .. } => Ok(Aarch64CallTarget::Register(reg.clone())),
        _ => Err(Error::from(
            "aarch64 call target must be symbol or register",
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn aarch64_text_roundtrip_preserves_structure() {
        let program = AsmAarch64Program {
            functions: vec![AsmAarch64Function {
                name: Name::new("main"),
                blocks: vec![AsmAarch64Block {
                    id: 0,
                    instructions: vec![
                        Aarch64InstructionDetail {
                            opcode: "add".to_string(),
                            operands: vec![
                                Aarch64Operand::Register {
                                    reg: Aarch64Register::Virtual {
                                        id: 1,
                                        size_bits: 64,
                                    },
                                    access: OperandAccess::Write,
                                },
                                Aarch64Operand::Register {
                                    reg: Aarch64Register::Physical {
                                        name: "x0".to_string(),
                                        size_bits: 64,
                                    },
                                    access: OperandAccess::Read,
                                },
                                Aarch64Operand::Immediate(4),
                            ],
                            condition: None,
                            call_target: None,
                        },
                        Aarch64InstructionDetail {
                            opcode: "bl".to_string(),
                            operands: vec![Aarch64Operand::Symbol(Name::new("callee"))],
                            condition: None,
                            call_target: Some(Aarch64CallTarget::Symbol(Name::new("callee"))),
                        },
                    ],
                    terminator: Aarch64TerminatorDetail {
                        opcode: Aarch64TerminatorOpcode::BCond,
                        condition: Some(Aarch64ConditionCode::Eq),
                        targets: vec![1, 2],
                    },
                }],
            }],
        };

        let text = program.to_text();
        let parsed = AsmAarch64Program::parse_text(&text).unwrap();
        assert_eq!(parsed, program);
    }
}
