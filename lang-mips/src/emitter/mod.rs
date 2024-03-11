use crate::emitter::expr::EmitExprResult;
use crate::instruction::MipsInstruction;
use crate::register::MipsRegister;

mod expr;

pub struct MipsEmitter {
    register_s: Vec<MipsRegister>,
    register_t: Vec<MipsRegister>,
    label_counter: usize,
}

impl MipsEmitter {
    pub fn new() -> Self {
        Self {
            register_s: vec![
                MipsRegister::S1,
                MipsRegister::S2,
                MipsRegister::S3,
                MipsRegister::S4,
                MipsRegister::S5,
                MipsRegister::S6,
                MipsRegister::S7,
            ],
            register_t: vec![
                MipsRegister::T0,
                MipsRegister::T1,
                MipsRegister::T2,
                MipsRegister::T3,
                MipsRegister::T4,
                MipsRegister::T5,
                MipsRegister::T6,
                MipsRegister::T7,
                MipsRegister::T8,
            ],
            label_counter: 0,
        }
    }
    pub fn get_label(&mut self) -> String {
        let label = format!("L{}", self.label_counter);
        self.label_counter += 1;
        label
    }
    pub fn get_register_t(&mut self) -> MipsRegister {
        self.register_t.pop().unwrap()
    }
    pub fn get_register_s(&mut self) -> MipsRegister {
        self.register_s.pop().unwrap()
    }
    pub fn return_register(&mut self, reg: MipsRegister) {
        if reg.is_s() {
            self.register_s.push(reg);
        } else if reg.is_t() {
            self.register_t.push(reg);
        }
    }

    // MIPS only allows 16-bit immediate values, so we need to use the `li` instruction to load
    pub fn emit_load_immediate(&self, ret: MipsRegister, value: i16) -> EmitExprResult {
        let ins = MipsInstruction::Li {
            rt: ret,
            immediate: value,
        };
        EmitExprResult {
            ret,
            instructions: vec![ins],
        }
    }
}
