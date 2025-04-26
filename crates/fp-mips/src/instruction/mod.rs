pub use opcode::*;

use crate::storage::register::MipsRegister;

mod opcode;

#[derive(Debug, Clone, parse_display::Display)]
pub enum MipsInstruction {
    #[display("{name}:")]
    Label { name: String },
    /// R\[rd] = R\[rs] + R\[rt]
    #[display("add {rd}, {rs}, {rt}")]
    Add {
        rd: MipsRegister,
        rs: MipsRegister,
        rt: MipsRegister,
    },
    /// R\[rd] = R\[rs] - R\[rt]
    #[display("sub {rd}, {rs}, {rt}")]
    Sub {
        rd: MipsRegister,
        rs: MipsRegister,
        rt: MipsRegister,
    },
    /// R\[rd] = R\[rs] & R\[rt]
    /// Bitwise AND
    #[display("and {rd}, {rs}, {rt}")]
    And {
        rd: MipsRegister,
        rs: MipsRegister,
        rt: MipsRegister,
    },
    /// R\[rd] = R\[rs] | R\[rt]
    /// Bitwise OR
    #[display("or {rd}, {rs}, {rt}")]
    Or {
        rd: MipsRegister,
        rs: MipsRegister,
        rt: MipsRegister,
    },
    /// R\[rd] = R\[rs] < R\[rt]
    #[display("slt {rd}, {rs}, {rt}")]
    Slt {
        rd: MipsRegister,
        rs: MipsRegister,
        rt: MipsRegister,
    },
    /// R\[rt] = R\[rs] + immediate
    #[display("addi {rs}, {rt}, {immediate}")]
    Addi {
        rs: MipsRegister,
        rt: MipsRegister,
        immediate: i16,
    },
    /// M\[rt] = M\[R\[rs] + offset]
    #[display("lw {rt}, {offset}({rs})")]
    Lw {
        rt: MipsRegister,
        rs: MipsRegister,
        offset: i16,
    },
    /// R\[rt] = immediate
    #[display("li {rt}, {immediate}")]
    Li { rt: MipsRegister, immediate: i16 },
    /// M\[R\[rs] + offset] = R\[rt]
    #[display("sw {rt}, {offset}({rs})")]
    Sw {
        rt: MipsRegister,
        rs: MipsRegister,
        offset: i16,
    },
    /// R\[rt] = R\[rs] < R\[rt]
    #[display("slti {rs}, {rt}, {immediate}")]
    Slti {
        rs: MipsRegister,
        rt: MipsRegister,
        immediate: i16,
    },
    /// R\[rd] = R\[rs] == R\[rt]
    #[display("beq {rs}, {rt}, {label}")]
    Beq {
        rs: MipsRegister,
        rt: MipsRegister,
        label: String,
    },
    /// R\[rd] = R\[rs] != R\[rt]
    #[display("bne {rs}, {rt}, {label}")]
    Bne {
        rs: MipsRegister,
        rt: MipsRegister,
        label: String,
    },
    /// Jump to label
    #[display("j {label}")]
    J { label: String },
    /// Jump and link to label
    #[display("jal {label}")]
    Jal { label: String },
    /// Jump to register
    #[display("jr {rs}")]
    Jr { rs: MipsRegister },
    /// No operation
    #[display("nop")]
    Nop,
    /// Halt
    #[display("halt")]
    Halt,
    /// Multiply
    /// $hi, $lo = $rs * $rt
    #[display("mult {lhs}, {rhs}")]
    Mult {
        lhs: MipsRegister,
        rhs: MipsRegister,
    },
    /// Divide
    #[display("div {lhs}, {rhs}")]
    Div {
        lhs: MipsRegister,
        rhs: MipsRegister,
    },
    /// Modulo
    #[display("mod {lhs}, {rhs}")]
    Mod {
        lhs: MipsRegister,
        rhs: MipsRegister,
    },
    /// Move from LO
    /// due to how MIPS pipeline works,
    /// we need to avoid calling mult and div in next 2 instructions
    #[display("mflo {rd}")]
    Mflo { rd: MipsRegister },
    /// Move from HI
    /// due to how MIPS pipeline works,
    /// we need to avoid calling mult and div in next 2 instructions
    #[display("mfhi {rd}")]
    Mfhi { rd: MipsRegister },
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
