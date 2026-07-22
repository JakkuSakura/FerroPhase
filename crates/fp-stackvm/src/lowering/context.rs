//! Program-level lowering context and entry point.
//!
//! [`LoweringContext`] owns the output [`LirProgram`] and iterates over
//! bytecode functions, delegating per-function lowering to
//! [`FunctionLowering`][super::function::FunctionLowering].

use fp_bytecode::{BytecodeFunction, BytecodeProgram};
use fp_core::lir::LirProgram;

use super::function::FunctionLowering;
use super::LowerResult;

/// Top-level entry point: convert a complete bytecode program into LIR.
pub fn lower_program(program: &BytecodeProgram) -> LowerResult<LirProgram> {
    let mut ctx = LoweringContext::new(program);
    for func in &program.functions {
        let lir_func = ctx.lower_function(func)?;
        ctx.program.add_function(lir_func);
    }
    Ok(ctx.program)
}

/// Accumulates the output [`LirProgram`] while lowering each bytecode
/// function in turn.
pub(crate) struct LoweringContext<'a> {
    pub program: LirProgram,
    pub bytecode: &'a BytecodeProgram,
}

impl<'a> LoweringContext<'a> {
    pub fn new(bytecode: &'a BytecodeProgram) -> Self {
        Self {
            program: LirProgram::new(),
            bytecode,
        }
    }

    /// Lower a single bytecode function into an [`LirFunction`].
    pub fn lower_function(&mut self, func: &BytecodeFunction) -> LowerResult<fp_core::lir::LirFunction> {
        let entry_block_id = func.blocks.first().map(|b| b.id).unwrap_or(0);
        let sig = fp_core::lir::LirFunctionSignature {
            params: vec![fp_core::lir::LirType::I64; func.params as usize],
            return_type: fp_core::lir::LirType::I64,
            is_variadic: false,
        };

        let locals: Vec<fp_core::lir::LirLocal> = (0..func.locals)
            .map(|i| fp_core::lir::LirLocal {
                id: i,
                ty: fp_core::lir::LirType::I64,
                name: Some(format!("local_{i}")),
                is_argument: i > 0 && i <= func.params,
            })
            .collect();

        let mut lir_func = fp_core::lir::LirFunction::new(
            fp_core::lir::Name::new(func.name.clone()),
            sig,
            fp_core::lir::CallingConvention::C,
            fp_core::lir::Linkage::Internal,
        );
        lir_func.locals = locals;

        let mut fl = FunctionLowering::new(self.bytecode, &mut lir_func, entry_block_id);

        // Allocate a stack slot (`Alloca`) for every bytecode local.
        //
        // Layout in the bytecode VM:
        //   local 0          → return value slot
        //   local 1..=params → arguments
        //   local >params    → general-purpose scratch
        //
        // We allocate slots for all of them so that LoadLocal/StoreLocal
        // always resolve.  The runtime loads argument values from
        // registers (r1, r2, …) into these slots before execution.
        for i in 0..func.locals {
            fl.emit_in_entry_block(fp_core::lir::LirInstructionKind::Alloca {
                size: fp_core::lir::LirValue::Constant(fp_core::lir::LirConstant::Int(8, fp_core::lir::LirType::I64)),
                alignment: 8,
            })?;
            let slot_reg = fl.last_reg();
            fl.set_local_addr(i, slot_reg);
        }

        // Lower each basic block.
        for block in &func.blocks {
            fl.lower_block(block)?;
        }

        // Compute predecessor/successor sets for each block.
        super::cfg::compute_cfg(&mut lir_func);

        Ok(lir_func)
    }
}
