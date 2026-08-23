//! Program-level lowering context and entry point.
//!
//! [`LoweringContext`] owns the output [`LirBlob`] and iterates over
//! bytecode functions, delegating per-function lowering to
//! [`FunctionLowering`][super::function::FunctionLowering].

use fp_bytecode::{BytecodeFunction, BytecodeProgram};
use fp_core::lir::{LirDataLayout, LirBlob};

use super::LowerResult;
use super::function::FunctionLowering;

/// Top-level entry point: convert a complete bytecode program into LIR.
pub fn lower_program(program: &BytecodeProgram) -> LowerResult<LirBlob> {
    let mut ctx = LoweringContext::new(program);
    for func in &program.functions {
        let lir_func = ctx.lower_function(func)?;
        ctx.program.add_function(lir_func);
    }
    Ok(ctx.program)
}

/// Accumulates the output [`LirBlob`] while lowering each bytecode
/// function in turn.
pub(crate) struct LoweringContext<'a> {
    pub program: LirBlob,
    pub bytecode: &'a BytecodeProgram,
}

impl<'a> LoweringContext<'a> {
    pub fn new(bytecode: &'a BytecodeProgram) -> Self {
        Self {
            program: LirBlob::new(
                LirDataLayout::new(
                    64,
                    8,
                    vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
                )
                .expect("valid stack VM data layout"),
            ),
            bytecode,
        }
    }

    /// Lower a single bytecode function into an [`LirFunction`].
    pub fn lower_function(
        &mut self,
        func: &BytecodeFunction,
    ) -> LowerResult<fp_core::lir::LirFunction> {
        let entry_block_id = func.blocks.first().map(|b| b.id).unwrap_or(0);
        let sig = fp_core::lir::LirFunctionSignature {
            params: func.param_types.clone(),
            return_type: func.return_type.clone(),
            is_variadic: false,
        };

        let locals: Vec<fp_core::lir::LirLocal> = func
            .local_types
            .iter()
            .enumerate()
            .map(|(i, ty)| fp_core::lir::LirLocal {
                id: i as u32,
                ty: ty.clone(),
                name: Some(format!("local_{i}")),
                is_argument: i > 0 && i <= func.param_types.len(),
            })
            .collect();

        let mut lir_func = fp_core::lir::LirFunction::new(
            fp_core::lir::Name::new(func.name.clone()),
            sig,
            fp_core::lir::CallingConvention::C,
            fp_core::lir::Linkage::Internal,
        );
        lir_func.locals = locals;

        let local_types = lir_func
            .locals
            .iter()
            .map(|local| local.ty.clone())
            .collect();
        let mut fl =
            FunctionLowering::new(self.bytecode, &mut lir_func, entry_block_id, local_types);

        // The LirInterpreter pre-allocates stack slots for all locals
        // declared in func.locals during run_function().  We reference
        // them by local index rather than emitting Alloca
        // instructions, which would permanently consume stack space.

        // Lower each basic block.
        for block in &func.blocks {
            fl.lower_block(block)?;
        }

        // Compute predecessor/successor sets for each block.
        super::cfg::compute_cfg(&mut lir_func);

        Ok(lir_func)
    }
}
