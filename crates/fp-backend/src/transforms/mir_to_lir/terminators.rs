use super::*;
use fp_core::error::Result;
use fp_core::{lir, mir};

impl MirToLirLowerer {
    pub(super) fn transform_terminator(
        &mut self,
        terminator: &mir::Terminator,
        block: &mut lir::LirBasicBlock,
    ) -> Result<lir::LirTerminator> {
        match &terminator.kind {
            mir::TerminatorKind::Return => Ok(lir::LirTerminator::Return(
                self.prepare_return_value(block)?,
            )),
            mir::TerminatorKind::Goto { target } => Ok(lir::LirTerminator::Br(*target)),
            mir::TerminatorKind::Unreachable => Ok(lir::LirTerminator::Unreachable),
            mir::TerminatorKind::Call {
                func,
                args,
                destination,
                cleanup,
                ..
            } => self.transform_call_terminator(func, args, destination, cleanup, block),
            mir::TerminatorKind::SwitchInt {
                discr,
                switch_ty,
                targets,
            } => {
                let discr_value = self.transform_operand(discr)?;
                block.instructions.extend(self.take_queued_instructions());
                if targets.values.len() == 1 {
                    let true_target = targets.targets[0];
                    let false_target = targets.otherwise;
                    let switch_lir_ty = self.lir_type_from_ty(switch_ty);
                    let case_value = self.switch_constant_for_value(
                        switch_ty,
                        targets.values[0],
                        &switch_lir_ty,
                    )?;
                    let cmp_id = self.next_id();
                    block.instructions.push(lir::LirInstruction {
                        id: cmp_id,
                        kind: lir::LirInstructionKind::Eq(
                            discr_value,
                            lir::LirValue::constant(case_value),
                        ),
                        result: Some(lir::LirRegister {
                            id: cmp_id,
                            ty: lir::LirType::I1,
                        }),
                        debug_info: None,
                    });
                    Ok(lir::LirTerminator::CondBr {
                        condition: lir::LirValue::register(cmp_id, lir::LirType::I1),
                        if_true: true_target,
                        if_false: false_target,
                    })
                } else {
                    let cases = targets
                        .values
                        .iter()
                        .zip(targets.targets.iter())
                        .map(|(value, target)| (*value as u64, *target))
                        .collect();
                    Ok(lir::LirTerminator::Switch {
                        value: discr_value,
                        default: targets.otherwise,
                        cases,
                    })
                }
            }
            other => Err(crate::error::optimization_error(format!(
                "unhandled MIR terminator: {other:?}"
            ))),
        }
    }
}
