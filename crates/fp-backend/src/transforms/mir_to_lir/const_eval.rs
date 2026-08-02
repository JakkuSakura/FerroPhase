use fp_core::error::Result;
use fp_core::{lir, mir};

use super::LirGenerator;

impl LirGenerator {
    /// Analyze MIR body to extract const values assigned to locals
    pub(crate) fn analyze_const_values(&mut self, mir_body: &mir::Body) -> Result<()> {
        // Iterate to propagate simple aliases like x = y where y is const-evaluated
        let mut changed = true;
        while changed {
            changed = false;
            for basic_block in &mir_body.basic_blocks {
                for stmt in &basic_block.statements {
                    if let mir::StatementKind::Assign(place, rvalue) = &stmt.kind {
                        if let Some(const_value) = self.extract_const_from_rvalue(rvalue)? {
                            if self.const_values.insert(place.local, const_value).is_none() {
                                changed = true;
                            }
                        } else if let mir::Rvalue::Use(op) = rvalue {
                            match op {
                                mir::Operand::Move(from) | mir::Operand::Copy(from) => {
                                    if let Some(cv) = self.const_values.get(&from.local).cloned() {
                                        if self.const_values.insert(place.local, cv).is_none() {
                                            changed = true;
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// Extract a const value from an rvalue if it represents a constant
    pub(crate) fn extract_const_from_rvalue(
        &self,
        rvalue: &mir::Rvalue,
    ) -> Result<Option<lir::LirConstant>> {
        match rvalue {
            mir::Rvalue::Query(_) => Ok(None),
            mir::Rvalue::Use(operand) => {
                if let mir::Operand::Constant(constant) = operand {
                    match &constant.literal {
                        mir::ConstantKind::Int(value) => {
                            let value = i32::try_from(*value).map_err(|_| {
                                fp_core::error::Error::from("constant does not fit i32")
                            })?;
                            Ok(Some(
                                lir::LirConstant::integer(
                                    lir::LirType::I32,
                                    lir::LirInteger::I32(u32::from_ne_bytes(value.to_ne_bytes())),
                                )
                                .map_err(|error| fp_core::error::Error::from(error.to_string()))?,
                            ))
                        }
                        mir::ConstantKind::UInt(value) => {
                            let value = u32::try_from(*value).map_err(|_| {
                                fp_core::error::Error::from("constant does not fit i32")
                            })?;
                            Ok(Some(
                                lir::LirConstant::integer(
                                    lir::LirType::I32,
                                    lir::LirInteger::I32(value),
                                )
                                .map_err(|error| fp_core::error::Error::from(error.to_string()))?,
                            ))
                        }
                        mir::ConstantKind::Float(value) => Ok(Some(
                            lir::LirConstant::float(
                                lir::LirType::F64,
                                lir::LirFloat::F64(value.to_bits()),
                            )
                            .map_err(|error| fp_core::error::Error::from(error.to_string()))?,
                        )),
                        mir::ConstantKind::Bool(value) => Ok(Some(
                            lir::LirConstant::integer(
                                lir::LirType::I1,
                                lir::LirInteger::I1(*value),
                            )
                            .map_err(|error| fp_core::error::Error::from(error.to_string()))?,
                        )),
                        // LIR strings are data globals plus an address constant. This
                        // local-only const evaluator cannot create that global.
                        mir::ConstantKind::Str(_) => Ok(None),
                        _ => Ok(None),
                    }
                } else {
                    Ok(None)
                }
            }
            // Handle binary operations that can be const-folded (simple ints)
            mir::Rvalue::BinaryOp(bin_op, lhs, rhs) => {
                if let (mir::Operand::Constant(lhs_const), mir::Operand::Constant(rhs_const)) =
                    (lhs, rhs)
                {
                    if let (mir::ConstantKind::Int(lhs_val), mir::ConstantKind::Int(rhs_val)) =
                        (&lhs_const.literal, &rhs_const.literal)
                    {
                        let result = match bin_op {
                            mir::BinOp::Add => lhs_val + rhs_val,
                            mir::BinOp::Sub => lhs_val - rhs_val,
                            mir::BinOp::Mul => lhs_val * rhs_val,
                            mir::BinOp::Div => {
                                if *rhs_val != 0 {
                                    lhs_val / rhs_val
                                } else {
                                    return Ok(None);
                                }
                            }
                            _ => return Ok(None),
                        };
                        let result = i32::try_from(result).map_err(|_| {
                            fp_core::error::Error::from("constant result does not fit i32")
                        })?;
                        Ok(Some(
                            lir::LirConstant::integer(
                                lir::LirType::I32,
                                lir::LirInteger::I32(u32::from_ne_bytes(result.to_ne_bytes())),
                            )
                            .map_err(|error| fp_core::error::Error::from(error.to_string()))?,
                        ))
                    } else {
                        Ok(None)
                    }
                } else {
                    Ok(None)
                }
            }
            mir::Rvalue::IntrinsicCall { .. } => Ok(None),
            _ => Ok(None),
        }
    }
}
