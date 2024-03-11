use eyre::bail;
use eyre::Result;
use lang_core::ops::BinOpKind;
use strum_macros::{Display, EnumString};
#[derive(Debug, Clone, Copy, EnumString, Display)]
#[strum(serialize_all = "lowercase")]
pub enum MipsOpcode {
    // R-type instructions
    Add,
    Sub,
    And,
    Or,
    // I-type instructions
    Addi,
    Lw,
    Li,
    Sw,
    Beq,
    // J-type instructions
    J,
    Jal,
    Jr,
    // Other instructions
    Nop,
    Halt,

    Mult,
    Div,
    Mod,

    Mflo,
}

impl MipsOpcode {
    pub fn is_r_type(&self) -> bool {
        match self {
            MipsOpcode::Add | MipsOpcode::Sub | MipsOpcode::And | MipsOpcode::Or => true,
            _ => false,
        }
    }
    pub fn followed_by_mflo(&self) -> bool {
        match self {
            MipsOpcode::Mult | MipsOpcode::Div | MipsOpcode::Mod => true,
            _ => false,
        }
    }
    pub fn from_binop(bin_op_kind: BinOpKind) -> Result<Self> {
        let ret = match bin_op_kind {
            BinOpKind::Add => MipsOpcode::Add,
            BinOpKind::Sub => MipsOpcode::Sub,
            BinOpKind::Mul => MipsOpcode::Mult,
            BinOpKind::Div => MipsOpcode::Div,
            BinOpKind::Mod => MipsOpcode::Mod,
            // BinOpKind::BitXor => MipsOpcode::Nop,
            BinOpKind::BitAnd => MipsOpcode::And,
            BinOpKind::BitOr => MipsOpcode::Or,
            _ => bail!("Unsupported binop {} with type int", bin_op_kind),
        };
        Ok(ret)
    }
}
