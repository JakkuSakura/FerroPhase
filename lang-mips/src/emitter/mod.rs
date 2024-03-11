use std::rc::Rc;

use crate::emitter::expr::MipsEmitExprResult;
use crate::instruction::MipsInstruction;
use crate::register::RegisterManager;

mod expr;

pub struct MipsEmitter {
    register: Rc<RegisterManager>,
    label_counter: usize,
}

impl MipsEmitter {
    pub fn new() -> Self {
        Self {
            register: Rc::new(RegisterManager::new()),
            label_counter: 0,
        }
    }
    pub fn get_label(&mut self) -> String {
        let label = format!("L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    // MIPS only allows 16-bit immediate values, so we need to use the `li` instruction to load
    pub fn emit_load_immediate(&self, value: i16) -> MipsEmitExprResult {
        let ret = self.register.acquire_t().unwrap();

        let ins = MipsInstruction::Li {
            rt: ret.register,
            immediate: value,
        };
        MipsEmitExprResult {
            ret,
            instructions: vec![ins],
        }
    }
}
