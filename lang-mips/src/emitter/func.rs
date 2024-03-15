use crate::emitter::MipsEmitter;
use crate::instruction::MipsInstruction;
use crate::storage::register::MipsRegister;

impl MipsEmitter {
    pub fn emit_invoke_function(&mut self, func_name: &str) -> Vec<MipsInstruction> {
        let mut instructions = Vec::new();
        // save all borrowed registers to stack
        if let Some(frame) = self.stack.frames.last() {
            for register in frame.register.list_borrowed() {
                let ins = MipsInstruction::Addi {
                    rt: MipsRegister::Sp,
                    rs: MipsRegister::Sp,
                    immediate: -4,
                };
                instructions.push(ins);
                let ins = MipsInstruction::Sw {
                    rt: register,
                    rs: MipsRegister::Sp,
                    offset: 0,
                };
                instructions.push(ins);
            }
        }
        // TODO: in the function body, push $ra to stack first
        // let ins = MipsInstruction::Addi {
        //     rt: MipsRegister::Sp,
        //     rs: MipsRegister::Sp,
        //     immediate: -4,
        // };
        // instructions.push(ins);

        // jump to the function
        let ins = MipsInstruction::Jal {
            label: func_name.to_string(),
        };
        instructions.push(ins);

        if let Some(frame) = self.stack.frames.last() {
            for register in frame.register.list_borrowed().into_iter().rev() {
                let ins = MipsInstruction::Lw {
                    rt: register,
                    rs: MipsRegister::Sp,
                    offset: 0,
                };
                instructions.push(ins);
                let ins = MipsInstruction::Addi {
                    rt: MipsRegister::Sp,
                    rs: MipsRegister::Sp,
                    immediate: 4,
                };
                instructions.push(ins);
            }
        }
        instructions
    }
}
