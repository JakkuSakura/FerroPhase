use super::*;
use fp_core::mir;
use fp_core::mir::ty::TyKind;

impl MirToLirLowerer {
    pub(super) fn reset_for_new_function(&mut self) {
        self.next_label = 0;
        self.register_map.clear();
        self.current_function = None;
        self.const_values.clear();
        self.local_types.clear();
        self.current_return_type = None;
        self.return_local = None;
        self.mutable_locals.clear();
        self.local_storage.clear();
        self.entry_allocas.clear();
        self.queued_instructions.clear();
    }

    pub(super) fn collect_struct_layouts(&mut self, body: &mir::Body) {
        for block in &body.basic_blocks {
            for statement in &block.statements {
                match &statement.kind {
                    mir::StatementKind::Assign(place, value) => {
                        self.collect_place_struct_layout(place, body);
                        self.collect_rvalue_struct_layouts(value, body);
                    }
                    mir::StatementKind::IntrinsicCall { args, .. } => {
                        for arg in args {
                            self.collect_operand_struct_layout(arg, body);
                        }
                    }
                    mir::StatementKind::SetDiscriminant { place, .. }
                    | mir::StatementKind::Retag(_, place)
                    | mir::StatementKind::AscribeUserType(place, _, _) => {
                        self.collect_place_struct_layout(place, body);
                    }
                    mir::StatementKind::StorageLive(_)
                    | mir::StatementKind::StorageDead(_)
                    | mir::StatementKind::Nop => {}
                }
            }

            let Some(terminator) = &block.terminator else {
                continue;
            };
            match &terminator.kind {
                mir::TerminatorKind::SwitchInt { discr, .. }
                | mir::TerminatorKind::Assert { cond: discr, .. } => {
                    self.collect_operand_struct_layout(discr, body);
                }
                mir::TerminatorKind::Drop { place, .. } => {
                    self.collect_place_struct_layout(place, body);
                }
                mir::TerminatorKind::DropAndReplace { place, value, .. } => {
                    self.collect_place_struct_layout(place, body);
                    self.collect_operand_struct_layout(value, body);
                }
                mir::TerminatorKind::Call {
                    func,
                    args,
                    destination,
                    ..
                } => {
                    self.collect_operand_struct_layout(func, body);
                    for arg in args {
                        self.collect_operand_struct_layout(arg, body);
                    }
                    if let Some((place, _)) = destination {
                        self.collect_place_struct_layout(place, body);
                    }
                }
                mir::TerminatorKind::Yield {
                    value, resume_arg, ..
                } => {
                    self.collect_operand_struct_layout(value, body);
                    self.collect_place_struct_layout(resume_arg, body);
                }
                mir::TerminatorKind::Goto { .. }
                | mir::TerminatorKind::Resume
                | mir::TerminatorKind::Abort
                | mir::TerminatorKind::Return
                | mir::TerminatorKind::Unreachable
                | mir::TerminatorKind::GeneratorDrop
                | mir::TerminatorKind::FalseEdge { .. }
                | mir::TerminatorKind::FalseUnwind { .. }
                | mir::TerminatorKind::InlineAsm { .. } => {}
            }
        }
    }

    pub(super) fn collect_rvalue_struct_layouts(&mut self, value: &mir::Rvalue, body: &mir::Body) {
        match value {
            mir::Rvalue::Use(operand)
            | mir::Rvalue::Repeat(operand, _)
            | mir::Rvalue::Cast(_, operand, _)
            | mir::Rvalue::UnaryOp(_, operand)
            | mir::Rvalue::ShallowInitBox(operand, _) => {
                self.collect_operand_struct_layout(operand, body);
            }
            mir::Rvalue::IntrinsicCall { args, .. }
            | mir::Rvalue::Aggregate(_, args)
            | mir::Rvalue::ContainerLiteral { elements: args, .. } => {
                for arg in args {
                    self.collect_operand_struct_layout(arg, body);
                }
            }
            mir::Rvalue::BinaryOp(_, left, right)
            | mir::Rvalue::CheckedBinaryOp(_, left, right) => {
                self.collect_operand_struct_layout(left, body);
                self.collect_operand_struct_layout(right, body);
            }
            mir::Rvalue::ContainerMapLiteral { entries, .. } => {
                for (key, value) in entries {
                    self.collect_operand_struct_layout(key, body);
                    self.collect_operand_struct_layout(value, body);
                }
            }
            mir::Rvalue::ContainerLen { container, .. } => {
                self.collect_operand_struct_layout(container, body);
            }
            mir::Rvalue::ContainerGet { container, key, .. } => {
                self.collect_operand_struct_layout(container, body);
                self.collect_operand_struct_layout(key, body);
            }
            mir::Rvalue::ContainerPush {
                container, value, ..
            } => {
                self.collect_operand_struct_layout(container, body);
                self.collect_operand_struct_layout(value, body);
            }
            mir::Rvalue::StrFromRawParts { ptr, len } => {
                self.collect_operand_struct_layout(ptr, body);
                self.collect_operand_struct_layout(len, body);
            }
            mir::Rvalue::Ref(_, _, place)
            | mir::Rvalue::AddressOf(_, place)
            | mir::Rvalue::Len(place)
            | mir::Rvalue::Discriminant(place) => {
                self.collect_place_struct_layout(place, body);
            }
            mir::Rvalue::Query(_)
            | mir::Rvalue::ThreadLocalRef(_)
            | mir::Rvalue::NullaryOp(_, _) => {}
        }
    }

    pub(super) fn collect_operand_struct_layout(
        &mut self,
        operand: &mir::Operand,
        body: &mir::Body,
    ) {
        match operand {
            mir::Operand::Copy(place) | mir::Operand::Move(place) => {
                self.collect_place_struct_layout(place, body);
            }
            mir::Operand::Constant(_) => {}
        }
    }

    pub(super) fn collect_place_struct_layout(&mut self, place: &mir::Place, body: &mir::Body) {
        let Some(mut ty) = body
            .locals
            .get(place.local as usize)
            .map(|local| local.ty.clone())
        else {
            return;
        };
        for projection in &place.projection {
            match projection {
                mir::PlaceElem::Field(index, field_ty) => {
                    // Enums are deliberately excluded here. Their payload
                    // slot(s) already have a dedicated, correct layout
                    // computed elsewhere (`full_layouts`/
                    // `opaque_payload_sizes`/`lookup_adt_def`) — one that
                    // accounts for *all* variants at once, using an opaque
                    // byte-array union slot when variants disagree on the
                    // payload's shape (e.g. `json::Value`, whose variants
                    // carry a `bool`, a `Number`, a `&str`, a `Vec<Value>`,
                    // etc.). A place projection only ever sees *one*
                    // variant's concrete field type at a time (e.g.
                    // `Value::Array(values)`'s pattern binding projects
                    // `Vec<Value>` specifically) — caching that here, keyed
                    // only by the enum's `DefId`, would clobber the
                    // correct union-slot type with whichever variant's
                    // field type happened to be observed last across the
                    // whole program (nondeterministically, since functions
                    // are processed in HashMap-derived order).
                    if let TyKind::Adt(adt, substs) = &ty.kind {
                        if !adt.flags.contains(mir::ty::AdtFlags::IS_ENUM) {
                            let field_lir_ty = self.lir_type_from_ty(field_ty);
                            let key = (adt.did.clone(), Self::adt_substs_types(substs));
                            let mut layouts = self.struct_layouts.borrow_mut();
                            let fields = layouts.entry(key).or_default();
                            if fields.len() <= *index {
                                fields.resize(index + 1, None);
                            }
                            fields[*index] = Some(field_lir_ty);
                        }
                    }
                    ty = field_ty.clone();
                }
                mir::PlaceElem::Deref => match ty.kind {
                    TyKind::Ref(_, inner, _) | TyKind::RawPtr(TypeAndMut { ty: inner, .. }) => {
                        ty = *inner;
                    }
                    _ => return,
                },
                mir::PlaceElem::Index(_index) => {
                    ty = match ty.kind {
                        TyKind::Array(element, _) | TyKind::Slice(element) => *element,
                        _ => return,
                    };
                }
                mir::PlaceElem::ConstantIndex { .. }
                | mir::PlaceElem::Subslice { .. }
                | mir::PlaceElem::Downcast(_, _) => {}
            }
        }
    }
}
