use super::*;
use fp_core::error::Result;
use fp_core::{lir, mir};

impl MirToLirLowerer {
    pub(super) fn handle_aggregate(
        &mut self,
        place: &mir::Place,
        kind: &mir::AggregateKind,
        fields: &[mir::Operand],
    ) -> Result<(Vec<lir::LirInstruction>, Option<lir::LirValue>)> {
        let mut instructions = Vec::new();
        let mut raw_values = Vec::with_capacity(fields.len());
        let mut constants = Vec::with_capacity(fields.len());
        let mut operand_types = Vec::with_capacity(fields.len());
        let mut all_constants = true;

        for operand in fields {
            let value = self.transform_operand(operand)?;
            instructions.extend(self.take_queued_instructions());
            operand_types.push(self.type_of_operand(operand));
            let is_constant = matches!(value.kind, lir::LirValueKind::Constant(_));
            if let lir::LirValueKind::Constant(ref constant_kind) = value.kind {
                constants.push(lir::LirConstant {
                    ty: value.ty.clone(),
                    kind: constant_kind.clone(),
                });
            }
            if !is_constant {
                all_constants = false;
            }
            raw_values.push(value);
        }

        let place_ty = self.lookup_place_type(place);
        let aggregate_ty = place_ty.as_ref().map(|ty| self.lir_type_from_ty(ty));
        let mut expected_field_tys = self.expected_aggregate_element_types(
            place_ty.as_ref(),
            aggregate_ty.as_ref(),
            raw_values.len(),
        );
        if expected_field_tys.is_empty()
            || (matches!(aggregate_ty, Some(lir::LirType::Ptr(_)))
                && raw_values.len() == expected_field_tys.len()
                && expected_field_tys
                    .iter()
                    .all(|t| matches!(t, lir::LirType::Ptr(_))))
        {
            expected_field_tys = operand_types
                .iter()
                .zip(raw_values.iter())
                .map(|(operand_ty, value)| operand_ty.clone().unwrap_or_else(|| value.ty.clone()))
                .collect();
        }
        for (idx, ty) in expected_field_tys.iter_mut().enumerate() {
            if matches!(ty, lir::LirType::Void) {
                if let Some(operand) = fields.get(idx) {
                    if let Some(operand_ty) = self.type_of_operand(operand) {
                        *ty = operand_ty;
                    }
                }
            }
        }
        if fields.is_empty() {
            if let Some(lir_ty) = aggregate_ty {
                let value = match &lir_ty {
                    lir::LirType::Struct { .. } => {
                        lir::LirValue::constant(lir::LirConstant::aggregate(
                            lir_ty.clone(),
                            lir::LirConstantAggregate::Struct(Vec::new()),
                        ))
                    }
                    lir::LirType::Array(_, _) => {
                        lir::LirValue::constant(lir::LirConstant::aggregate(
                            lir_ty.clone(),
                            lir::LirConstantAggregate::Array(Vec::new()),
                        ))
                    }
                    _ => return Ok((instructions, None)),
                };
                return Ok((instructions, Some(value)));
            }
            return Ok((instructions, None));
        }
        if all_constants {
            let adjusted_consts =
                self.adjust_constants_for_aggregate(constants, &expected_field_tys)?;
            if let Some(place_ty) = place_ty.as_ref() {
                if let Some(constant) =
                    self.constant_from_aggregate(kind, adjusted_consts, place_ty)
                {
                    return Ok((instructions, Some(lir::LirValue::constant(constant))));
                }
            }
        }
        let agg_construction_ty = if matches!(kind, mir::AggregateKind::Array(_)) {
            match aggregate_ty.clone() {
                Some(lir::LirType::Array(elem, _)) => {
                    Some(lir::LirType::Array(elem, raw_values.len() as u64))
                }
                _ => Some(lir::LirType::Array(
                    Box::new(
                        expected_field_tys
                            .first()
                            .cloned()
                            .unwrap_or(lir::LirType::I64),
                    ),
                    raw_values.len() as u64,
                )),
            }
        } else {
            match aggregate_ty.clone() {
                Some(lir::LirType::Struct {
                    fields,
                    packed,
                    name,
                }) if fields.len() == raw_values.len() => Some(lir::LirType::Struct {
                    fields,
                    packed,
                    name,
                }),
                Some(lir::LirType::Array(elem, _)) => {
                    Some(lir::LirType::Array(elem, raw_values.len() as u64))
                }
                Some(_) if raw_values.len() > 1 => Some(lir::LirType::Struct {
                    fields: expected_field_tys.clone(),
                    packed: false,
                    name: None,
                }),
                None if raw_values.len() > 1 => Some(lir::LirType::Struct {
                    fields: expected_field_tys.clone(),
                    packed: false,
                    name: None,
                }),
                _ => None,
            }
        };
        if let Some(agg_ty) = agg_construction_ty {
            let mut current_value =
                lir::LirValue::constant(lir::LirConstant::undef(agg_ty.clone()));
            for (index, value) in raw_values.into_iter().enumerate() {
                let mut element = value;
                if let Some(field_ty) = expected_field_tys.get(index) {
                    let source_ty = operand_types.get(index).and_then(|ty| ty.clone());
                    element = self.coerce_aggregate_value_with_source(
                        element,
                        source_ty.as_ref(),
                        field_ty,
                        &mut instructions,
                    )?;
                }
                let instr_id = self.next_id();
                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind: lir::LirInstructionKind::InsertValue {
                        aggregate: current_value.clone(),
                        element,
                        indices: vec![index as u32],
                    },
                    result: Some(lir::LirRegister {
                        id: instr_id,
                        ty: agg_ty.clone(),
                    }),
                    debug_info: None,
                });
                current_value = lir::LirValue::register(instr_id, agg_ty.clone());
            }
            return Ok((instructions, Some(current_value)));
        }
        if raw_values.len() == 1 {
            return Ok((instructions, raw_values.into_iter().next()));
        }
        Ok((instructions, None))
    }
}
