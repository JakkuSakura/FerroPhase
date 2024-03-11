mod opcode;
pub use opcode::*;

use crate::register::MipsRegister;
use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum MipsInstruction {
    Label {
        name: String,
    },
    // R-Type instructions
    Add {
        rd: MipsRegister,
        rs: MipsRegister,
        rt: MipsRegister,
    },
    Sub {
        rd: MipsRegister,
        rs: MipsRegister,
        rt: MipsRegister,
    },
    And {
        rd: MipsRegister,
        rs: MipsRegister,
        rt: MipsRegister,
    },
    Or {
        rd: MipsRegister,
        rs: MipsRegister,
        rt: MipsRegister,
    },
    // I-Type instructions
    Addi {
        rs: MipsRegister,
        rt: MipsRegister,
        immediate: i16,
    },
    Lw {
        rs: MipsRegister,
        rt: MipsRegister,
        offset: i16,
    },
    Li {
        rt: MipsRegister,
        immediate: i16,
    },
    Sw {
        rs: MipsRegister,
        rt: MipsRegister,
        offset: i16,
    },
    Beq {
        rs: MipsRegister,
        rt: MipsRegister,
        offset: i16,
    },
    // J-Type instructions
    J {
        label: String,
    },
    Jal {
        label: String,
    },
    Jr {
        rs: MipsRegister,
    },
    // Other instructions
    Nop,
    Halt,
    Mult {
        lhs: MipsRegister,
        rhs: MipsRegister,
    },
    Div {
        lhs: MipsRegister,
        rhs: MipsRegister,
    },
    Mod {
        lhs: MipsRegister,
        rhs: MipsRegister,
    },
    Mflo {
        rd: MipsRegister,
    },
}
impl MipsInstruction {
    pub fn from_opcode_r(
        opcode: MipsOpcode,
        ret: MipsRegister,
        lhs: MipsRegister,
        rhs: MipsRegister,
    ) -> Self {
        match opcode {
            MipsOpcode::Add => MipsInstruction::Add {
                rd: ret,
                rs: lhs,
                rt: rhs,
            },
            MipsOpcode::Sub => MipsInstruction::Sub {
                rd: ret,
                rs: lhs,
                rt: rhs,
            },
            MipsOpcode::And => MipsInstruction::And {
                rd: ret,
                rs: lhs,
                rt: rhs,
            },
            MipsOpcode::Or => MipsInstruction::Or {
                rd: ret,
                rs: lhs,
                rt: rhs,
            },
            _ => panic!("Unsupported opcode {}", opcode),
        }
    }
    pub fn from_opcode_mult_div_mod(
        opcode: MipsOpcode,
        lhs: MipsRegister,
        rhs: MipsRegister,
    ) -> Self {
        match opcode {
            MipsOpcode::Mult => MipsInstruction::Mult { lhs, rhs },
            MipsOpcode::Div => MipsInstruction::Div { lhs, rhs },
            MipsOpcode::Mod => MipsInstruction::Mod { lhs, rhs },
            _ => panic!("Unsupported opcode {}", opcode),
        }
    }
}
impl Display for MipsInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            MipsInstruction::Add { rd, rs, rt } => {
                write!(f, "add {}, {}, {}", rd, rs, rt)
            }
            MipsInstruction::Sub { rd, rs, rt } => {
                write!(f, "sub {}, {}, {}", rd, rs, rt)
            }
            MipsInstruction::And { rd, rs, rt } => {
                write!(f, "and {}, {}, {}", rd, rs, rt)
            }
            MipsInstruction::Or { rd, rs, rt } => {
                write!(f, "or {}, {}, {}", rd, rs, rt)
            }
            MipsInstruction::Addi { rs, rt, immediate } => {
                write!(f, "addi {}, {}, {}", rt, rs, immediate)
            }
            MipsInstruction::Lw { rs, rt, offset } => {
                write!(f, "lw {}, {}({})", rt, offset, rs)
            }
            MipsInstruction::Li { rt, immediate } => {
                write!(f, "li {}, {}", rt, immediate)
            }
            MipsInstruction::Sw { rs, rt, offset } => {
                write!(f, "sw {}, {}({})", rt, offset, rs)
            }
            MipsInstruction::Beq { rs, rt, offset } => {
                write!(f, "beq {}, {}, {}", rs, rt, offset)
            }
            MipsInstruction::J { label } => {
                write!(f, "j {}", label)
            }
            MipsInstruction::Jal { label } => {
                write!(f, "jal {}", label)
            }
            MipsInstruction::Jr { rs } => {
                write!(f, "jr {}", rs)
            }
            MipsInstruction::Nop => {
                write!(f, "nop")
            }
            MipsInstruction::Halt => {
                write!(f, "halt")
            }
            MipsInstruction::Mult { lhs, rhs } => {
                write!(f, "mult {}, {}", lhs, rhs)
            }
            MipsInstruction::Div { lhs, rhs } => {
                write!(f, "div {}, {}", lhs, rhs)
            }
            MipsInstruction::Mod { lhs, rhs } => {
                write!(f, "mod {}, {}", lhs, rhs)
            }
            MipsInstruction::Mflo { rd } => {
                write!(f, "mflo {}", rd)
            }
            MipsInstruction::Label { name } => {
                write!(f, "{}:", name)
            }
        }
    }
}
