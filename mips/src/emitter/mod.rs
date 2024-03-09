use std::sync::atomic::AtomicUsize;

use crate::emitter::expr::EmitExprResult;
use crate::instruction::MipsInstruction;
use crate::register::MipsRegister;

mod expr;

pub struct MipsEmitter {
    register_count: AtomicUsize,
}

impl MipsEmitter {
    pub fn new() -> Self {
        Self {
            register_count: AtomicUsize::new(0),
        }
    }
    pub fn get_register_t(&self) -> MipsRegister {
        const GROUP: [MipsRegister; 9] = [
            MipsRegister::T0,
            MipsRegister::T1,
            MipsRegister::T2,
            MipsRegister::T3,
            MipsRegister::T4,
            MipsRegister::T5,
            MipsRegister::T6,
            MipsRegister::T7,
            MipsRegister::T8,
        ];
        let t = self
            .register_count
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        GROUP[t % GROUP.len()]
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
