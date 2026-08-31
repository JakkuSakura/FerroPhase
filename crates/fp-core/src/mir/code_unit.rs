use std::collections::{BTreeSet, HashMap};

use super::{Body, BodyId, ConstantKind, Item, Operand, Rvalue, StatementKind, TerminatorKind};

/// One `hir::DefId`'s worth of lowered MIR content — usually one item plus
/// its one body, occasionally more when lowering that item pulled in
/// something it directly references (e.g. a synthetic comptime probe's
/// item, or a nested item discovered along the way). Distinct from
/// a whole `MirPackage`'s combined content: a `MirCodeUnit` is
/// deliberately partial and keyed by the `DefId` that produced it (see
/// `MirPackage::units`), so re-lowering one item after a comptime value
/// resolves means replacing its one unit, not rebuilding the whole
/// package's content.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct MirCodeUnit {
    pub items: Vec<Item>,
    pub bodies: HashMap<BodyId, Body>,
}

impl MirCodeUnit {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            bodies: HashMap::new(),
        }
    }

    /// Returns the language functions directly referenced by this unit's
    /// bodies.  This is the MIR equivalent of rustc's dependency-MIR query:
    /// downstream work asks for the concrete `DefId`s it actually reaches,
    /// rather than scanning every method or function known to the session.
    pub fn referenced_function_def_ids(&self) -> Vec<crate::hir::DefId> {
        let mut ids = BTreeSet::new();
        for body in self.bodies.values() {
            for block in &body.basic_blocks {
                for statement in &block.statements {
                    match &statement.kind {
                        StatementKind::Assign(_, rvalue) => collect_rvalue(rvalue, &mut ids),
                        StatementKind::IntrinsicCall { args, .. } => {
                            for arg in args {
                                collect_operand(arg, &mut ids);
                            }
                        }
                        StatementKind::SetDiscriminant { .. }
                        | StatementKind::StorageLive(_)
                        | StatementKind::StorageDead(_)
                        | StatementKind::Retag(_, _)
                        | StatementKind::AscribeUserType(_, _, _)
                        | StatementKind::Nop => {}
                    }
                }
                if let Some(terminator) = &block.terminator {
                    match &terminator.kind {
                        TerminatorKind::SwitchInt { discr, .. } => collect_operand(discr, &mut ids),
                        TerminatorKind::DropAndReplace { value, .. } => {
                            collect_operand(value, &mut ids)
                        }
                        TerminatorKind::Call { func, args, .. } => {
                            collect_operand(func, &mut ids);
                            for arg in args {
                                collect_operand(arg, &mut ids);
                            }
                        }
                        TerminatorKind::Assert { cond, .. } => collect_operand(cond, &mut ids),
                        TerminatorKind::Yield { value, .. } => collect_operand(value, &mut ids),
                        TerminatorKind::Goto { .. }
                        | TerminatorKind::Resume
                        | TerminatorKind::Abort
                        | TerminatorKind::Return
                        | TerminatorKind::Unreachable
                        | TerminatorKind::Drop { .. }
                        | TerminatorKind::GeneratorDrop
                        | TerminatorKind::FalseEdge { .. }
                        | TerminatorKind::FalseUnwind { .. }
                        | TerminatorKind::InlineAsm { .. } => {}
                    }
                }
            }
        }
        ids.into_iter().collect()
    }
}

fn collect_operand(operand: &Operand, ids: &mut BTreeSet<crate::hir::DefId>) {
    let Operand::Constant(constant) = operand else {
        return;
    };
    if let ConstantKind::FnDef(def_id, _) = &constant.literal {
        ids.insert(def_id.clone());
    }
}

fn collect_rvalue(rvalue: &Rvalue, ids: &mut BTreeSet<crate::hir::DefId>) {
    match rvalue {
        Rvalue::Use(operand)
        | Rvalue::Repeat(operand, _)
        | Rvalue::UnaryOp(_, operand)
        | Rvalue::ShallowInitBox(operand, _) => collect_operand(operand, ids),
        Rvalue::IntrinsicCall { args, .. } => {
            for arg in args {
                collect_operand(arg, ids);
            }
        }
        Rvalue::Cast(_, operand, _) => collect_operand(operand, ids),
        Rvalue::BinaryOp(_, left, right) | Rvalue::CheckedBinaryOp(_, left, right) => {
            collect_operand(left, ids);
            collect_operand(right, ids);
        }
        Rvalue::Aggregate(_, operands) => {
            for operand in operands {
                collect_operand(operand, ids);
            }
        }
        Rvalue::ContainerLiteral { elements, .. } => {
            for element in elements {
                collect_operand(element, ids);
            }
        }
        Rvalue::ContainerMapLiteral { entries, .. } => {
            for (key, value) in entries {
                collect_operand(key, ids);
                collect_operand(value, ids);
            }
        }
        Rvalue::ContainerLen { container, .. } => collect_operand(container, ids),
        Rvalue::ContainerGet { container, key, .. } => {
            collect_operand(container, ids);
            collect_operand(key, ids);
        }
        Rvalue::ContainerPush {
            container, value, ..
        } => {
            collect_operand(container, ids);
            collect_operand(value, ids);
        }
        Rvalue::StrFromRawParts { ptr, len } => {
            collect_operand(ptr, ids);
            collect_operand(len, ids);
        }
        Rvalue::TypeValue(_)
        | Rvalue::Query(_)
        | Rvalue::Ref(_, _, _)
        | Rvalue::ThreadLocalRef(_)
        | Rvalue::AddressOf(_, _)
        | Rvalue::Len(_)
        | Rvalue::NullaryOp(_, _)
        | Rvalue::Discriminant(_) => {}
    }
}
