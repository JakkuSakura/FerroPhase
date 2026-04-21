use fp_core::error::Error;
use fp_core::lir::LirProgram;

use crate::error::optimization_error;

mod cfg_cleanup;
mod promote_stack;
mod simplify_bool;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LirPassName {
    RemoveUnreachableBlocks,
    PromoteStackToRegister,
    SimplifyBoolConditions,
}

impl LirPassName {
    pub fn as_str(self) -> &'static str {
        match self {
            LirPassName::RemoveUnreachableBlocks => "remove_unreachable_blocks",
            LirPassName::PromoteStackToRegister => "promote_stack_to_register",
            LirPassName::SimplifyBoolConditions => "simplify_bool_conditions",
        }
    }
}

#[derive(Debug, Clone)]
pub struct LirOptimizationPlan {
    pub passes: Vec<LirPassName>,
}

impl LirOptimizationPlan {
    pub fn empty() -> Self {
        Self { passes: Vec::new() }
    }

    pub fn for_level(level: u8) -> Self {
        if level == 0 {
            return Self::empty();
        }
        Self {
            passes: vec![
                LirPassName::RemoveUnreachableBlocks,
                LirPassName::PromoteStackToRegister,
                LirPassName::SimplifyBoolConditions,
                LirPassName::RemoveUnreachableBlocks,
            ],
        }
    }

    pub fn is_empty(&self) -> bool {
        self.passes.is_empty()
    }
}

#[derive(Debug, Default, Clone)]
pub struct LirOptimizationReport {
    pub total_changes: usize,
    pub per_pass: Vec<(LirPassName, usize)>,
}

pub struct LirOptimizer;

impl LirOptimizer {
    pub fn new() -> Self {
        Self
    }

    pub fn apply_plan(
        &self,
        program: &mut LirProgram,
        plan: &LirOptimizationPlan,
    ) -> Result<LirOptimizationReport, Error> {
        let mut report = LirOptimizationReport::default();

        for pass_name in &plan.passes {
            let changes = match pass_name {
                LirPassName::RemoveUnreachableBlocks => {
                    cfg_cleanup::remove_unreachable_blocks(program)?
                }
                LirPassName::PromoteStackToRegister => {
                    promote_stack::promote_stack_to_register(program)?
                }
                LirPassName::SimplifyBoolConditions => {
                    simplify_bool::simplify_bool_conditions(program)?
                }
            };
            report.total_changes += changes;
            report.per_pass.push((*pass_name, changes));
        }

        Ok(report)
    }
}

pub fn parse_lir_pass_ident(ident: &str) -> Result<LirPassName, Error> {
    match ident.trim().to_ascii_lowercase().as_str() {
        "remove_unreachable_blocks" | "remove-unreachable-blocks" | "cfg-cleanup" => {
            Ok(LirPassName::RemoveUnreachableBlocks)
        }
        "promote_stack_to_register" | "promote-stack-to-register" | "mem2reg" => {
            Ok(LirPassName::PromoteStackToRegister)
        }
        "simplify_bool_conditions" | "simplify-bool-conditions" | "bool-conds" => {
            Ok(LirPassName::SimplifyBoolConditions)
        }
        other => Err(optimization_error(format!(
            "unknown LIR optimization pass: {other}"
        ))),
    }
}
