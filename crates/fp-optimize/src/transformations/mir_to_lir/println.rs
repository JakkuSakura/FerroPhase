use fp_core::error::Result;
use fp_core::{lir, mir};

use super::LirGenerator;

impl LirGenerator {
    /// Transform single argument println calls
    pub(crate) fn transform_println_single_arg(
        &mut self,
        arg: &mir::Operand,
        fmt: &mut String,
        call_args: &mut Vec<lir::LirValue>,
    ) -> Result<()> {
        // Helper to select printf format for a constant
        fn select_format_for_const(c: &lir::LirConstant) -> &'static str {
            match c {
                lir::LirConstant::String(_) => "%s\n",
                lir::LirConstant::Int(_, _) => "%d\n",
                lir::LirConstant::UInt(_, _) => "%u\n",
                lir::LirConstant::Float(_, _) => "%f\n",
                lir::LirConstant::Bool(_) => "%d\n",
                _ => "%d\n",
            }
        }

        match arg {
            // Direct string literal: print it as data, not a format string
            mir::Operand::Constant(mir::Constant {
                literal: mir::ConstantKind::Str(s),
                ..
            }) => {
                *fmt = "%s\n".to_string();
                call_args.push(lir::LirValue::Constant(lir::LirConstant::String(s.clone())));
            }
            // Any other direct constant
            mir::Operand::Constant(constant) => {
                let lir_const = match &constant.literal {
                    mir::ConstantKind::Int(v) => lir::LirConstant::Int(*v, lir::LirType::I32),
                    mir::ConstantKind::UInt(v) => lir::LirConstant::UInt(*v, lir::LirType::I32),
                    mir::ConstantKind::Float(v) => lir::LirConstant::Float(*v, lir::LirType::F64),
                    mir::ConstantKind::Bool(b) => lir::LirConstant::Bool(*b),
                    mir::ConstantKind::Str(s) => lir::LirConstant::String(s.clone()),
                    _ => {
                        return Err(crate::error::optimization_error(
                            "Unsupported constant in println single arg",
                        ))
                    }
                };
                *fmt = select_format_for_const(&lir_const).to_string();
                call_args.push(lir::LirValue::Constant(lir_const));
            }
            // Value moved or copied from a place: try const-prop map first
            mir::Operand::Move(place) | mir::Operand::Copy(place) => {
                if let Some(const_value) = self.const_values.get(&place.local) {
                    *fmt = select_format_for_const(const_value).to_string();
                    call_args.push(lir::LirValue::Constant(const_value.clone()));
                } else if let Some(val) = self.register_map.get(&place.local) {
                    match val {
                        lir::LirValue::Constant(c) => {
                            *fmt = select_format_for_const(c).to_string();
                            call_args.push(lir::LirValue::Constant(c.clone()));
                        }
                        _ => {
                            return Err(crate::error::optimization_error(format!(
                                "Cannot determine printf format for println single arg; unknown const for local {}",
                                place.local
                            )));
                        }
                    }
                } else {
                    return Err(crate::error::optimization_error(format!(
                        "Cannot determine printf format for println single arg; unknown const for local {}",
                        place.local
                    )));
                }
            }
        }
        Ok(())
    }

    /// Transform multi-argument println calls
    pub(crate) fn transform_println_multi_arg(
        &mut self,
        args: &[mir::Operand],
        fmt: &mut String,
        call_args: &mut Vec<lir::LirValue>,
    ) -> Result<()> {
        if let mir::Operand::Constant(mir::Constant {
            literal: mir::ConstantKind::Str(s),
            ..
        }) = &args[0]
        {
            // Replace all occurrences of {} with %d for now
            let mut replaced = s.clone();
            while let Some(pos) = replaced.find("{}") {
                replaced.replace_range(pos..pos + 2, "%d");
            }
            *fmt = replaced + "\n";
            for arg in &args[1..] {
                match arg {
                    mir::Operand::Constant(c) => {
                        let lv = match &c.literal {
                            mir::ConstantKind::Int(v) => lir::LirValue::Constant(
                                lir::LirConstant::Int(*v, lir::LirType::I32),
                            ),
                            mir::ConstantKind::UInt(v) => lir::LirValue::Constant(
                                lir::LirConstant::UInt(*v, lir::LirType::I32),
                            ),
                            mir::ConstantKind::Bool(b) => {
                                lir::LirValue::Constant(lir::LirConstant::Bool(*b))
                            }
                            mir::ConstantKind::Str(s) => {
                                lir::LirValue::Constant(lir::LirConstant::String(s.clone()))
                            }
                            _ => {
                                return Err(crate::error::optimization_error(
                                    "Unsupported constant in println multi arg",
                                ))
                            }
                        };
                        call_args.push(lv);
                    }
                    mir::Operand::Move(place) | mir::Operand::Copy(place) => {
                        if let Some(cv) = self.const_values.get(&place.local) {
                            call_args.push(lir::LirValue::Constant(cv.clone()));
                        } else if let Some(val) = self.register_map.get(&place.local) {
                            call_args.push(val.clone());
                        } else {
                            return Err(crate::error::optimization_error(format!(
                                "Unknown printf arg from local {}",
                                place.local
                            )));
                        }
                    }
                }
            }
        } else {
            return Err(crate::error::optimization_error(
                "println with multiple arguments requires a format string literal as the first argument"
                    .to_string(),
            ));
        }
        Ok(())
    }
}
