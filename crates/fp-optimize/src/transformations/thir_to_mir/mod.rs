use fp_core::error::Result;
use fp_core::hir::ty as hir_types;
use fp_core::hir::typed as thir;
use fp_core::mir;
use fp_core::mir::ty as mir_types;
use fp_core::ops::BinOpKind;
use fp_core::span::Span;
use mir_types::{Ty, TyKind};
// use fp_core::ast::Visibility; // not used here
use std::collections::HashMap;

use super::IrTransform;
mod context;
mod expressions;
mod functions;
#[cfg(test)]
mod tests;
mod type_utils;

/// Generator for transforming THIR to MIR (Mid-level IR)
pub struct MirGenerator {
    next_mir_id: mir::MirId,
    next_local_id: u32,
    next_bb_id: u32,
    current_locals: HashMap<thir::ThirId, mir::LocalId>,
    current_blocks: Vec<mir::BasicBlockData>,
    local_decls: Vec<mir::LocalDecl>,
    loop_stack: Vec<LoopContext>,
    current_return_local: Option<mir::LocalId>,
}

struct ExprOutcome {
    place: mir::Place,
    block: mir::BasicBlockId,
}

#[derive(Clone, Copy)]
struct LoopContext {
    continue_block: mir::BasicBlockId,
    break_block: mir::BasicBlockId,
    break_result: Option<mir::LocalId>,
}

impl MirGenerator {
    pub fn new() -> Self {
        Self {
            next_mir_id: 0,
            next_local_id: 0,
            next_bb_id: 0,
            current_locals: HashMap::new(),
            current_blocks: Vec::new(),
            local_decls: Vec::new(),
            loop_stack: Vec::new(),
            current_return_local: None,
        }
    }

    /// Transform a THIR program to MIR

    pub fn transform(&mut self, thir_program: thir::Program) -> Result<mir::Program> {
        let mut mir_program = mir::Program::new();

        for item in &thir_program.items {
            match &item.kind {
                thir::ItemKind::Function(func) => {
                    let (mir_func, maybe_body) =
                        self.transform_function_with_body(func.clone(), &thir_program)?;
                    if let Some((body_id, body)) = maybe_body {
                        mir_program.bodies.insert(body_id, body);
                    }
                    mir_program.items.push(mir::Item {
                        mir_id: self.next_id(),
                        kind: mir::ItemKind::Function(mir_func),
                    });
                }
                thir::ItemKind::Struct(_) => {
                    // Structs don't need MIR representation - they're handled at type level
                }
                thir::ItemKind::Const(_const_item) => {
                    // Skip const items for now
                }
                thir::ItemKind::Impl(impl_block) => {
                    for impl_item in &impl_block.items {
                        if let thir::ImplItemKind::Method(method) = &impl_item.kind {
                            let (mir_func, maybe_body) =
                                self.transform_function_with_body(method.clone(), &thir_program)?;
                            if let Some((body_id, body)) = maybe_body {
                                mir_program.bodies.insert(body_id, body);
                            }
                            mir_program.items.push(mir::Item {
                                mir_id: self.next_id(),
                                kind: mir::ItemKind::Function(mir_func),
                            });
                        }
                    }
                }
            }
        }

        Ok(mir_program)
    }
}

impl IrTransform<thir::Program, mir::Program> for MirGenerator {
    fn transform(&mut self, source: thir::Program) -> Result<mir::Program> {
        MirGenerator::transform(self, source)
    }
}

impl Default for MirGenerator {
    fn default() -> Self {
        Self::new()
    }
}
