use crate::emitter::expr::MipsEmitExprResult;
use crate::instruction::MipsInstruction;
use crate::storage::MipsStorage;

mod expr;
mod func;
pub struct MipsEmitter {
    stack: MipsStorage,
    label_counter: usize,
}

impl MipsEmitter {
    pub fn new() -> Self {
        let mut this = Self {
            stack: MipsStorage::new(),
            label_counter: 0,
        };
        this.stack.push_frame();
        this
    }
    pub fn get_label(&mut self) -> String {
        let label = format!("L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    // MIPS only allows 16-bit immediate values, so we need to use the `li` instruction to load
    pub fn emit_load_immediate(&self, value: i16) -> MipsEmitExprResult {
        let ret = self.stack.register_manager().acquire_t().unwrap();

        let ins = MipsInstruction::Li {
            rt: ret.register.register,
            immediate: value,
        };
        MipsEmitExprResult {
            ret,
            instructions: vec![ins],
        }
    }
}
