use eyre::Result;

use lang_core::ast::DefFunction;
use lang_core::context::SharedScopedContext;

use crate::emitter::expr::MipsEmitExprResult;
use crate::emitter::MipsEmitter;
use crate::instruction::MipsInstruction;
use crate::storage::register::{MipsRegister, MipsRegisterOwned};

impl MipsEmitter {
    // A function (i.e. callee) must preserve $s0-$s7,
    // the global pointer $gp, the stack pointer $sp,
    // and the frame pointer $fp

    pub fn emit_def_function(
        &mut self,
        func: &DefFunction,
        ctx: &SharedScopedContext,
    ) -> Result<MipsEmitExprResult> {
        let mut instructions = Vec::new();
        // emit function label
        let ins = MipsInstruction::Label {
            name: func.name.to_string(),
        };
        instructions.push(ins);
        let preservation = [
            MipsRegister::Ra,
            MipsRegister::S0,
            MipsRegister::S1,
            MipsRegister::S2,
            MipsRegister::S3,
            // MipsRegister::S4,
            // MipsRegister::S5,
            // MipsRegister::S6,
            // MipsRegister::S7,
        ];
        for register in preservation.iter() {
            instructions.extend(self.emit_push_stack(*register));
        }

        // emit function body
        let ret = self.emit_expr(&func.value.body, ctx)?;
        instructions.extend(ret.instructions);
        // push return value to stack
        // instructions.extend(self.emit_push_stack(ret.ret.get()));
        instructions.push(MipsInstruction::Add {
            rd: MipsRegister::V0,
            rt: ret.ret.get(),
            rs: MipsRegister::Zero,
        });
        for register in preservation.iter().rev() {
            instructions.extend(self.emit_pop_stack(*register));
        }
        // emit function return
        let ins = MipsInstruction::Jr {
            rs: MipsRegister::Ra,
        };
        instructions.push(ins);

        Ok(MipsEmitExprResult::new(
            MipsRegisterOwned::new_free(MipsRegister::V0),
            instructions,
        ))
    }
    pub fn emit_invoke_function(&mut self, func_name: &str) -> MipsEmitExprResult {
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
        MipsEmitExprResult {
            ret: MipsRegisterOwned::new_free(MipsRegister::V0),
            instructions,
        }
    }
}
