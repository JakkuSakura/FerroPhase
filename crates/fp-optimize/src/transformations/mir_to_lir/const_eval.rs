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
            mir::Rvalue::Use(operand) => {
                if let mir::Operand::Constant(constant) = operand {
                    match &constant.literal {
                        mir::ConstantKind::Int(value) => {
                            Ok(Some(lir::LirConstant::Int(*value, lir::LirType::I32)))
                        }
                        mir::ConstantKind::UInt(value) => {
                            Ok(Some(lir::LirConstant::UInt(*value, lir::LirType::I32)))
                        }
                        mir::ConstantKind::Float(value) => {
                            Ok(Some(lir::LirConstant::Float(*value, lir::LirType::F64)))
                        }
                        mir::ConstantKind::Bool(b) => Ok(Some(lir::LirConstant::Bool(*b))),
                        mir::ConstantKind::Str(s) => Ok(Some(lir::LirConstant::String(s.clone()))),
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
                        Ok(Some(lir::LirConstant::Int(result, lir::LirType::I32)))
                    } else {
                        Ok(None)
                    }
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }
}
