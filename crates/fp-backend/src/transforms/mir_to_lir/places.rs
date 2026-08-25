use super::*;
use fp_core::error::Result;
use fp_core::lir;
use fp_core::mir;
use fp_core::mir::ty::TyKind;

impl MirToLirLowerer {
    pub(super) fn get_or_create_register_for_place(&mut self, place: &mir::Place) -> Result<lir::LirValue> {
        if let Some(storage) = self.local_storage.get(&place.local) {
            return Ok(storage.ptr_value.clone());
        }
        let existing_reg = self.register_map.get(&place.local).cloned();

        if let Some(place_ty) = self.lookup_place_type(place) {
            if Self::is_zero_sized(&place_ty) {
                // Use a dedicated empty-struct constant for zero-sized values to avoid
                // creating "struct ptr i8 { }" constants when the place type lowers to Ptr(I8).
                let empty_ty = lir::LirType::Struct {
                    fields: Vec::new(),
                    packed: false,
                    name: None,
                };
                let value = lir::LirValue::constant(lir::LirConstant::aggregate(
                    empty_ty.clone(),
                    lir::LirConstantAggregate::Struct(Vec::new()),
                ));
                self.register_map.insert(place.local, value.clone());
                return Ok(value);
            }

            let lir_ty = self.lir_type_from_ty(&place_ty);
            let mut alloca_elem_ty = lir_ty.clone();
            let mut alloca_count = 1i64;
            if !matches!(place_ty.kind, TyKind::Array(_, _)) {
                if let Some(existing) = existing_reg.as_ref() {
                    if let lir::LirType::Array(elem, len) = &existing.ty {
                        alloca_elem_ty = elem.as_ref().clone();
                        alloca_count = *len as i64;
                    }
                }
            }
            let alignment = self.alignment_for_lir_type(&alloca_elem_ty);
            if alignment > 0 {
                let pointer_type = lir::LirType::Ptr(Box::new(alloca_elem_ty.clone()));
                let size_value = lir::LirValue::constant(
                    self.integer_constant(&lir::LirType::I32, alloca_count)?,
                );
                let alloca_id = self.next_id();
                self.queued_instructions.push(lir::LirInstruction {
                    id: alloca_id,
                    kind: lir::LirInstructionKind::Alloca {
                        size: size_value,
                        alignment,
                    },
                    result: Some(lir::LirRegister {
                        id: alloca_id,
                        ty: pointer_type.clone(),
                    }),
                    debug_info: None,
                });

                let ptr_value = lir::LirValue::register(alloca_id, pointer_type);
                self.local_storage.insert(
                    place.local,
                    LocalStorage {
                        ptr_value: ptr_value.clone(),
                        element_type: alloca_elem_ty,
                        alignment,
                    },
                );

                if let Some(existing) = existing_reg {
                    let store_id = self.next_id();
                    self.queued_instructions.push(lir::LirInstruction {
                        id: store_id,
                        kind: lir::LirInstructionKind::Store {
                            value: existing,
                            address: ptr_value.clone(),
                            alignment: Some(alignment),
                            volatile: false,
                        },
                        result: None,
                        debug_info: None,
                    });
                }

                return Ok(ptr_value);
            }
        }
        Err(crate::error::optimization_error(format!(
            "MIR→LIR: missing value for local {} (place={:?}); cannot lower MIR",
            place.local, place
        )))
    }

    pub(super) fn resolve_place(&mut self, place: &mir::Place) -> Result<PlaceAccess> {
        if place.projection.is_empty() {
            let ty = self
                .local_types
                .get(place.local as usize)
                .cloned()
                .ok_or_else(|| {
                    crate::error::optimization_error(format!(
                        "MIR→LIR: no type information for local {}",
                        place.local
                    ))
                })?;

            if let Some(storage) = self.local_storage.get(&place.local).cloned() {
                return Ok(PlaceAccess::Address(PlaceAddress {
                    ptr: storage.ptr_value,
                    ty,
                    lir_ty: storage.element_type,
                    alignment: storage.alignment,
                }));
            }

            if let Some(value) = self.register_map.get(&place.local).cloned() {
                let lir_ty = self.lir_type_from_ty(&ty);
                return Ok(PlaceAccess::Value { value, ty, lir_ty });
            }

            if let Ok(value) = self.get_or_create_register_for_place(place) {
                if let Some(storage) = self.local_storage.get(&place.local).cloned() {
                    return Ok(PlaceAccess::Address(PlaceAddress {
                        ptr: storage.ptr_value,
                        ty,
                        lir_ty: storage.element_type,
                        alignment: storage.alignment,
                    }));
                }
                let lir_ty = self.lir_type_from_ty(&ty);
                return Ok(PlaceAccess::Value { value, ty, lir_ty });
            }

            return Err(crate::error::optimization_error(format!(
                "MIR→LIR: unresolved place local {} — no register or storage allocated",
                place.local
            )));
        }

        let mut base_place = place.clone();
        let last_projection = base_place
            .projection
            .pop()
            .expect("projection should be non-empty here");
        let base_access = self.resolve_place(&base_place)?;

        match last_projection {
            mir::PlaceElem::Deref => self.apply_deref_projection(&base_place, base_access),
            mir::PlaceElem::Field(idx, field_ty) => {
                self.apply_field_projection(&base_place, base_access, place.local, idx, &field_ty)
            }
            mir::PlaceElem::Index(index_local) => {
                self.apply_index_projection(&base_place, base_access, index_local)
            }
            mir::PlaceElem::ConstantIndex {
                offset, from_end, ..
            } => {
                if from_end {
                    return Err(crate::error::optimization_error(
                        "MIR→LIR: from_end constant index is not yet supported",
                    ));
                }
                let index_value = lir::LirValue::constant(
                    self.integer_constant(&lir::LirType::I64, offset as i64)
                        .expect("constant index must fit i64"),
                );
                self.apply_index_projection_value(&base_place, base_access, index_value)
            }
            mir::PlaceElem::Subslice { from, to, from_end } => {
                let base_ty = self.lookup_place_type(&base_place).ok_or_else(|| {
                    crate::error::optimization_error("MIR→LIR: missing type for subslice")
                })?;
                let element_ty = match &base_ty.kind {
                    TyKind::Array(elem, _) => *elem.clone(),
                    TyKind::Slice(elem) => *elem.clone(),
                    _ => {
                        return Err(crate::error::optimization_error(
                            "MIR→LIR: subslice requires array or slice type",
                        ));
                    }
                };

                let start_value = lir::LirValue::constant(
                    self.integer_constant(&lir::LirType::I64, from as i64)
                        .expect("subslice offset must fit i64"),
                );
                let base_access_for_len = base_access.clone();
                let slice_ptr_access =
                    self.apply_index_projection_value(&base_place, base_access, start_value)?;
                let slice_ptr = match slice_ptr_access {
                    PlaceAccess::Address(addr) => addr.ptr,
                    PlaceAccess::Value { .. } => {
                        return Err(crate::error::optimization_error(
                            "MIR→LIR: subslice base did not resolve to address",
                        ));
                    }
                };

                let elem_lir_ty = self.lir_type_from_ty(&element_ty);

                match &base_ty.kind {
                    TyKind::Array(_, len) => {
                        let base_len = self.array_length_from_const(len);
                        let end = if from_end {
                            base_len.saturating_sub(to)
                        } else {
                            to
                        };
                        let slice_len = end.saturating_sub(from);
                        let mut instructions = Vec::new();
                        let slice_value = self.build_slice_value(
                            slice_ptr,
                            slice_len,
                            &elem_lir_ty,
                            &mut instructions,
                        )?;
                        self.queued_instructions.extend(instructions);
                        Ok(PlaceAccess::Value {
                            value: slice_value,
                            ty: base_ty,
                            lir_ty: self.slice_lir_type(&elem_lir_ty),
                        })
                    }
                    TyKind::Slice(_) => {
                        let mut instructions = Vec::new();
                        let slice_value = match base_access_for_len {
                            PlaceAccess::Address(addr) => {
                                let load_id = self.next_id();
                                instructions.push(lir::LirInstruction {
                                    id: load_id,
                                    kind: lir::LirInstructionKind::Load {
                                        address: addr.ptr,
                                        alignment: Some(addr.alignment),
                                        volatile: false,
                                    },
                                    result: Some(lir::LirRegister {
                                        id: load_id,
                                        ty: addr.lir_ty.clone(),
                                    }),
                                    debug_info: None,
                                });
                                lir::LirValue::register(load_id, addr.lir_ty)
                            }
                            PlaceAccess::Value { value, .. } => value,
                        };
                        let mut len_value = self.extract_slice_field(
                            slice_value,
                            1,
                            lir::LirType::I64,
                            &mut instructions,
                        );
                        len_value = self.ensure_i64_value(len_value, &mut instructions);

                        let end_value = if from_end {
                            let to_value = lir::LirValue::constant(
                                self.integer_constant(&lir::LirType::I64, to as i64)
                                    .expect("subslice bound must fit i64"),
                            );
                            let sub_id = self.next_id();
                            instructions.push(lir::LirInstruction {
                                id: sub_id,
                                kind: lir::LirInstructionKind::Sub(len_value, to_value),
                                result: Some(lir::LirRegister {
                                    id: sub_id,
                                    ty: lir::LirType::I64,
                                }),
                                debug_info: None,
                            });
                            lir::LirValue::register(sub_id, lir::LirType::I64)
                        } else {
                            lir::LirValue::constant(
                                self.integer_constant(&lir::LirType::I64, to as i64)
                                    .expect("subslice bound must fit i64"),
                            )
                        };

                        let slice_len = if from == 0 {
                            end_value
                        } else {
                            let start_value = lir::LirValue::constant(
                                self.integer_constant(&lir::LirType::I64, from as i64)
                                    .expect("subslice offset must fit i64"),
                            );
                            let sub_id = self.next_id();
                            instructions.push(lir::LirInstruction {
                                id: sub_id,
                                kind: lir::LirInstructionKind::Sub(end_value, start_value),
                                result: Some(lir::LirRegister {
                                    id: sub_id,
                                    ty: lir::LirType::I64,
                                }),
                                debug_info: None,
                            });
                            lir::LirValue::register(sub_id, lir::LirType::I64)
                        };

                        let slice_value = self.build_slice_value_with_len_value(
                            slice_ptr,
                            slice_len,
                            &elem_lir_ty,
                            &mut instructions,
                        )?;
                        self.queued_instructions.extend(instructions);
                        Ok(PlaceAccess::Value {
                            value: slice_value,
                            ty: base_ty,
                            lir_ty: self.slice_lir_type(&elem_lir_ty),
                        })
                    }
                    _ => Err(crate::error::optimization_error(
                        "MIR→LIR: subslice requires array or slice type",
                    )),
                }
            }
            mir::PlaceElem::Downcast(_, _) => Err(crate::error::optimization_error(
                "MIR→LIR: downcast place projection is not supported",
            )),
        }
    }

    pub(super) fn apply_deref_projection(
        &mut self,
        base_place: &mir::Place,
        access: PlaceAccess,
    ) -> Result<PlaceAccess> {
        let base_ty = self.lookup_place_type(base_place).ok_or_else(|| {
            crate::error::optimization_error("MIR→LIR: missing type for deref projection")
        })?;

        // A reference to an unsized slice (`&str`/`&[T]`) is represented in
        // this backend as the `{ptr, len}` fat-pointer value directly — see
        // `lir_type_from_ty`, where both `TyKind::Slice(_)` and
        // `TyKind::Ref(_, Slice(_), _)` map to the same `__slice` struct.
        // There is no separate, further-indirected pointee to load through:
        // the reference's own storage *is* the slice value's storage.
        // Dereferencing such a place is a type-level no-op — reuse the same
        // address/value, just retagged with the pointee type — rather than
        // treating it like a thin pointer (which would load a bogus "pointer
        // value" out of the first 8 bytes of the fat pointer and then
        // dereference that garbage address).
        if let TyKind::Ref(_, inner, _) = &base_ty.kind {
            if Self::slice_ref_element_ty(inner).is_some() {
                let pointee_ty = (**inner).clone();
                let pointee_lir_ty = self.lir_type_from_ty(&pointee_ty);
                return Ok(match access {
                    PlaceAccess::Address(addr) => PlaceAccess::Address(PlaceAddress {
                        ptr: addr.ptr,
                        ty: pointee_ty,
                        lir_ty: pointee_lir_ty,
                        alignment: addr.alignment,
                    }),
                    PlaceAccess::Value { value, .. } => PlaceAccess::Value {
                        value,
                        ty: pointee_ty,
                        lir_ty: pointee_lir_ty,
                    },
                });
            }
        }

        let (inner_ty, pointer_lir_ty) = match base_ty.kind {
            TyKind::Ref(_, inner, _) => {
                let pointee = (*inner).clone();
                let lir = self.lir_type_from_ty(&pointee);
                (pointee, lir::LirType::Ptr(Box::new(lir.clone())))
            }
            TyKind::RawPtr(TypeAndMut { ty: inner, .. }) => {
                let pointee = (*inner).clone();
                let lir = self.lir_type_from_ty(&pointee);
                (pointee, lir::LirType::Ptr(Box::new(lir.clone())))
            }
            _ => {
                return Err(crate::error::optimization_error(
                    "MIR→LIR: cannot dereference non-pointer place",
                ));
            }
        };

        let pointer_value = match access {
            PlaceAccess::Address(addr) => {
                let load_id = self.next_id();
                self.queued_instructions.push(lir::LirInstruction {
                    id: load_id,
                    kind: lir::LirInstructionKind::Load {
                        address: addr.ptr,
                        alignment: Some(addr.alignment),
                        volatile: false,
                    },
                    result: Some(lir::LirRegister {
                        id: load_id,
                        ty: pointer_lir_ty.clone(),
                    }),
                    debug_info: None,
                });
                lir::LirValue::register(load_id, pointer_lir_ty)
            }
            PlaceAccess::Value { value, .. } => value,
        };

        let pointee_lir_ty = self.lir_type_from_ty(&inner_ty);

        let alignment = self.alignment_for_lir_type(&pointee_lir_ty);
        Ok(PlaceAccess::Address(PlaceAddress {
            ptr: pointer_value,
            ty: inner_ty,
            lir_ty: pointee_lir_ty,
            alignment,
        }))
    }

    pub(super) fn apply_field_projection(
        &mut self,
        _base_place: &mir::Place,
        access: PlaceAccess,
        _local: mir::LocalId,
        field_index: usize,
        field_ty: &Ty,
    ) -> Result<PlaceAccess> {
        let base_addr = match access {
            PlaceAccess::Address(addr) => addr,
            PlaceAccess::Value { value, ty, lir_ty } => {
                let alignment = self.alignment_for_lir_type(&lir_ty).max(1);
                let pointer_type = lir::LirType::Ptr(Box::new(lir_ty.clone()));
                let size_value = lir::LirValue::constant(
                    self.integer_constant(&lir::LirType::I32, 1)
                        .expect("one must fit i32"),
                );
                let alloca_id = self.next_id();
                self.queued_instructions.push(lir::LirInstruction {
                    id: alloca_id,
                    kind: lir::LirInstructionKind::Alloca {
                        size: size_value,
                        alignment,
                    },
                    result: Some(lir::LirRegister {
                        id: alloca_id,
                        ty: pointer_type.clone(),
                    }),
                    debug_info: None,
                });
                let ptr_value = lir::LirValue::register(alloca_id, pointer_type);

                let store_id = self.next_id();
                self.queued_instructions.push(lir::LirInstruction {
                    id: store_id,
                    kind: lir::LirInstructionKind::Store {
                        value,
                        address: ptr_value.clone(),
                        alignment: Some(alignment),
                        volatile: false,
                    },
                    result: None,
                    debug_info: None,
                });

                PlaceAddress {
                    ptr: ptr_value,
                    ty,
                    lir_ty,
                    alignment,
                }
            }
        };

        let field_lir_ty = self.lir_type_from_ty(field_ty);

        let offset = if let Some(layout) = self
            .data_layout
            .struct_layout(&base_addr.lir_ty)
            .ok()
            .flatten()
        {
            *layout.field_offsets.get(field_index).ok_or_else(|| {
                crate::error::optimization_error(format!(
                    "MIR→LIR: field index {} out of bounds for LIR struct",
                    field_index
                ))
            })?
        } else if let TyKind::Tuple(elements) = &base_addr.ty.kind {
            let mut offset = 0u64;
            for elem_ty in elements.iter().take(field_index) {
                let elem_lir_ty = self.lir_type_from_ty(elem_ty);
                offset = offset.saturating_add(self.size_of_lir_type(&elem_lir_ty));
            }
            offset
        } else if field_index == 0 {
            // No real struct/tuple layout to consult — this is expected
            // for an enum's opaque, byte-blob-shaped shared payload slot
            // (heterogeneous per-variant types collapse to a plain
            // `Array(I8, N)` at the LIR level, with no field structure of
            // its own). Field 0 of *anything* starts at offset 0
            // regardless of the base's shape, so this needs no layout
            // lookup at all — only a non-zero index on a genuinely
            // unstructured base is a real error (below).
            0
        } else {
            return Err(crate::error::optimization_error(
                "MIR→LIR: field projection requires a struct/tuple layout",
            ));
        };

        let desired_ptr_ty = lir::LirType::Ptr(Box::new(field_lir_ty.clone()));
        let target_ptr = if offset == 0 {
            let cast_id = self.next_id();
            self.queued_instructions.push(lir::LirInstruction {
                id: cast_id,
                kind: lir::LirInstructionKind::Bitcast(
                    base_addr.ptr.clone(),
                    desired_ptr_ty.clone(),
                ),
                result: Some(lir::LirRegister {
                    id: cast_id,
                    ty: desired_ptr_ty.clone(),
                }),
                debug_info: None,
            });
            lir::LirValue::register(cast_id, desired_ptr_ty.clone())
        } else {
            let i8_ptr_ty = lir::LirType::Ptr(Box::new(lir::LirType::I8));
            let base_i8_ptr_id = self.next_id();
            self.queued_instructions.push(lir::LirInstruction {
                id: base_i8_ptr_id,
                kind: lir::LirInstructionKind::Bitcast(base_addr.ptr.clone(), i8_ptr_ty.clone()),
                result: Some(lir::LirRegister {
                    id: base_i8_ptr_id,
                    ty: i8_ptr_ty.clone(),
                }),
                debug_info: None,
            });
            let base_i8_ptr = lir::LirValue::register(base_i8_ptr_id, i8_ptr_ty.clone());

            let offset_value = lir::LirValue::constant(
                self.integer_constant(&lir::LirType::I64, offset as i64)
                    .expect("field offset must fit i64"),
            );

            let gep_id = self.next_id();
            self.queued_instructions.push(lir::LirInstruction {
                id: gep_id,
                kind: lir::LirInstructionKind::GetElementPtr {
                    ptr: base_i8_ptr,
                    indices: vec![offset_value],
                    inbounds: true,
                },
                result: Some(lir::LirRegister {
                    id: gep_id,
                    ty: i8_ptr_ty.clone(),
                }),
                debug_info: None,
            });

            let cast_id = self.next_id();
            self.queued_instructions.push(lir::LirInstruction {
                id: cast_id,
                kind: lir::LirInstructionKind::Bitcast(
                    lir::LirValue::register(gep_id, i8_ptr_ty.clone()),
                    desired_ptr_ty.clone(),
                ),
                result: Some(lir::LirRegister {
                    id: cast_id,
                    ty: desired_ptr_ty.clone(),
                }),
                debug_info: None,
            });
            lir::LirValue::register(cast_id, desired_ptr_ty)
        };

        let alignment = self.alignment_for_lir_type(&field_lir_ty);
        Ok(PlaceAccess::Address(PlaceAddress {
            ptr: target_ptr,
            ty: field_ty.clone(),
            lir_ty: field_lir_ty,
            alignment,
        }))
    }

    pub(super) fn apply_index_projection(
        &mut self,
        base_place: &mir::Place,
        access: PlaceAccess,
        index_local: mir::LocalId,
    ) -> Result<PlaceAccess> {
        let index_place = mir::Place::from_local(index_local);
        let index_operand = mir::Operand::Copy(index_place);
        let mut index_value = self.transform_operand(&index_operand)?;
        let index_lir_ty = self
            .type_of_operand(&index_operand)
            .ok_or_else(|| crate::error::optimization_error("index operand has no type"))?;
        if index_lir_ty != lir::LirType::I64 {
            let cast_id = self.next_id();
            self.queued_instructions.push(lir::LirInstruction {
                id: cast_id,
                kind: lir::LirInstructionKind::SextOrTrunc(index_value.clone(), lir::LirType::I64),
                result: Some(lir::LirRegister {
                    id: cast_id,
                    ty: lir::LirType::I64,
                }),
                debug_info: None,
            });
            index_value = lir::LirValue::register(cast_id, lir::LirType::I64);
        }

        self.apply_index_projection_value(base_place, access, index_value)
    }

    pub(super) fn apply_index_projection_value(
        &mut self,
        base_place: &mir::Place,
        access: PlaceAccess,
        index_value: lir::LirValue,
    ) -> Result<PlaceAccess> {
        let base_ty = self.lookup_place_type(base_place).ok_or_else(|| {
            crate::error::optimization_error("MIR→LIR: missing type for index projection")
        })?;

        let element_ty = match &base_ty.kind {
            TyKind::Array(elem, _) => *elem.clone(),
            TyKind::Slice(elem) => *elem.clone(),
            _ => {
                return Err(crate::error::optimization_error(
                    "MIR→LIR: index projection requires array or slice type",
                ));
            }
        };

        let element_lir_ty = self.lir_type_from_ty(&element_ty);
        let element_alignment = self.alignment_for_lir_type(&element_lir_ty);

        let slice_ptr_ty = lir::LirType::Ptr(Box::new(element_lir_ty.clone()));
        let base_ptr = match access {
            PlaceAccess::Address(addr) => match base_ty.kind {
                TyKind::Slice(_) => {
                    let load_id = self.next_id();
                    self.queued_instructions.push(lir::LirInstruction {
                        id: load_id,
                        kind: lir::LirInstructionKind::Load {
                            address: addr.ptr.clone(),
                            alignment: Some(addr.alignment),
                            volatile: false,
                        },
                        result: Some(lir::LirRegister {
                            id: load_id,
                            ty: addr.lir_ty.clone(),
                        }),
                        debug_info: None,
                    });
                    let extract_id = self.next_id();
                    self.queued_instructions.push(lir::LirInstruction {
                        id: extract_id,
                        kind: lir::LirInstructionKind::ExtractValue {
                            aggregate: lir::LirValue::register(load_id, addr.lir_ty.clone()),
                            indices: vec![0],
                        },
                        result: Some(lir::LirRegister {
                            id: extract_id,
                            ty: slice_ptr_ty.clone(),
                        }),
                        debug_info: None,
                    });
                    lir::LirValue::register(extract_id, slice_ptr_ty.clone())
                }
                _ => addr.ptr,
            },
            PlaceAccess::Value { value, lir_ty, .. } => match base_ty.kind {
                TyKind::Slice(_) => {
                    let extract_id = self.next_id();
                    self.queued_instructions.push(lir::LirInstruction {
                        id: extract_id,
                        kind: lir::LirInstructionKind::ExtractValue {
                            aggregate: value,
                            indices: vec![0],
                        },
                        result: Some(lir::LirRegister {
                            id: extract_id,
                            ty: slice_ptr_ty.clone(),
                        }),
                        debug_info: None,
                    });
                    lir::LirValue::register(extract_id, slice_ptr_ty.clone())
                }
                _ => {
                    let alignment = self.alignment_for_lir_type(&lir_ty).max(1);
                    let pointer_type = lir::LirType::Ptr(Box::new(lir_ty.clone()));
                    let size_value = lir::LirValue::constant(
                        self.integer_constant(&lir::LirType::I32, 1)
                            .expect("one must fit i32"),
                    );
                    let alloca_id = self.next_id();
                    self.queued_instructions.push(lir::LirInstruction {
                        id: alloca_id,
                        kind: lir::LirInstructionKind::Alloca {
                            size: size_value,
                            alignment,
                        },
                        result: Some(lir::LirRegister {
                            id: alloca_id,
                            ty: pointer_type.clone(),
                        }),
                        debug_info: None,
                    });
                    let ptr_value = lir::LirValue::register(alloca_id, pointer_type);

                    let store_id = self.next_id();
                    self.queued_instructions.push(lir::LirInstruction {
                        id: store_id,
                        kind: lir::LirInstructionKind::Store {
                            value,
                            address: ptr_value.clone(),
                            alignment: Some(alignment),
                            volatile: false,
                        },
                        result: None,
                        debug_info: None,
                    });

                    ptr_value
                }
            },
        };

        let element_size = self.size_of_lir_type(&element_lir_ty);
        let offset_value = if element_size == 1 {
            index_value
        } else {
            let scale = lir::LirValue::constant(
                self.integer_constant(&lir::LirType::I64, element_size as i64)
                    .expect("element size must fit i64"),
            );
            let mul_id = self.next_id();
            self.queued_instructions.push(lir::LirInstruction {
                id: mul_id,
                kind: lir::LirInstructionKind::Mul(index_value, scale),
                result: Some(lir::LirRegister {
                    id: mul_id,
                    ty: lir::LirType::I64,
                }),
                debug_info: None,
            });
            lir::LirValue::register(mul_id, lir::LirType::I64)
        };

        let i8_ptr_ty = lir::LirType::Ptr(Box::new(lir::LirType::I8));
        let base_i8_ptr_id = self.next_id();
        self.queued_instructions.push(lir::LirInstruction {
            id: base_i8_ptr_id,
            kind: lir::LirInstructionKind::Bitcast(base_ptr.clone(), i8_ptr_ty.clone()),
            result: Some(lir::LirRegister {
                id: base_i8_ptr_id,
                ty: i8_ptr_ty.clone(),
            }),
            debug_info: None,
        });
        let base_i8_ptr = lir::LirValue::register(base_i8_ptr_id, i8_ptr_ty.clone());

        let gep_id = self.next_id();
        self.queued_instructions.push(lir::LirInstruction {
            id: gep_id,
            kind: lir::LirInstructionKind::GetElementPtr {
                ptr: base_i8_ptr,
                indices: vec![offset_value],
                inbounds: true,
            },
            result: Some(lir::LirRegister {
                id: gep_id,
                ty: i8_ptr_ty.clone(),
            }),
            debug_info: None,
        });

        let target_ptr_ty = lir::LirType::Ptr(Box::new(element_lir_ty.clone()));
        let cast_id = self.next_id();
        self.queued_instructions.push(lir::LirInstruction {
            id: cast_id,
            kind: lir::LirInstructionKind::Bitcast(
                lir::LirValue::register(gep_id, i8_ptr_ty.clone()),
                target_ptr_ty.clone(),
            ),
            result: Some(lir::LirRegister {
                id: cast_id,
                ty: target_ptr_ty.clone(),
            }),
            debug_info: None,
        });

        Ok(PlaceAccess::Address(PlaceAddress {
            ptr: lir::LirValue::register(cast_id, target_ptr_ty),
            ty: element_ty.clone(),
            lir_ty: element_lir_ty,
            alignment: element_alignment,
        }))
    }
}
