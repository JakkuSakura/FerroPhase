use super::*;

impl InterpretationOrchestrator {
    pub fn builtin_bitand(&self) -> BuiltinFn {
        BuiltinFn::new_with_ident("&".into(), move |args, _ctx| {
            if args.len() != 2 {
                return Err(optimization_error(format!(
                    "BitAnd expects 2 arguments, got: {}",
                    args.len()
                )));
            }

            match (&args[0], &args[1]) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::int(a.value & b.value)),
                _ => Err(optimization_error(format!(
                    "BitAnd operation not supported for types: {:?} & {:?}",
                    args[0], args[1]
                ))),
            }
        })
    }

    // Logical AND operation

    pub fn builtin_logical_and(&self) -> BuiltinFn {
        BuiltinFn::new_with_ident("&&".into(), move |args, _ctx| {
            if args.len() != 2 {
                return Err(optimization_error(format!(
                    "LogicalAnd expects 2 arguments, got: {}",
                    args.len()
                )));
            }

            match (&args[0], &args[1]) {
                (Value::Bool(a), Value::Bool(b)) => Ok(Value::bool(a.value && b.value)),
                _ => Err(optimization_error(format!(
                    "LogicalAnd operation not supported for types: {:?} && {:?}",
                    args[0], args[1]
                ))),
            }
        })
    }

    // Logical OR operation

    pub fn builtin_logical_or(&self) -> BuiltinFn {
        BuiltinFn::new_with_ident("||".into(), move |args, _ctx| {
            if args.len() != 2 {
                return Err(optimization_error(format!(
                    "LogicalOr expects 2 arguments, got: {}",
                    args.len()
                )));
            }

            match (&args[0], &args[1]) {
                (Value::Bool(a), Value::Bool(b)) => Ok(Value::bool(a.value || b.value)),
                _ => Err(optimization_error(format!(
                    "LogicalOr operation not supported for types: {:?} || {:?}",
                    args[0], args[1]
                ))),
            }
        })
    }

    // Division operation

    pub fn builtin_div(&self) -> BuiltinFn {
        BuiltinFn::new_with_ident("/".into(), move |args, _ctx| {
            if args.len() != 2 {
                return Err(optimization_error(format!(
                    "Division expects 2 arguments, got: {}",
                    args.len()
                )));
            }

            match (&args[0], &args[1]) {
                (Value::Int(a), Value::Int(b)) => {
                    if b.value == 0 {
                        return Err(optimization_error("Division by zero".to_string()));
                    }
                    Ok(Value::int(a.value / b.value))
                }
                _ => Err(optimization_error(format!(
                    "Division operation not supported for types: {:?} / {:?}",
                    args[0], args[1]
                ))),
            }
        })
    }

    // Format string expression handler - evaluate structured template parts with args

    pub fn lookup_bin_op_kind(&self, op: BinOpKind) -> Result<BuiltinFn> {
        match op {
            BinOpKind::Add => Ok(builtin_add()),
            BinOpKind::AddTrait => {
                let this = self.clone();
                Ok(BuiltinFn::new(op, move |args, value| {
                    let args: Vec<_> = args
                        .into_iter()
                        .map(|x| {
                            let value = this.interpret_value(x, value, true)?;
                            match value {
                                Value::Type(Ty::ImplTraits(impls)) => Ok(impls.bounds),
                                _ => opt_bail!(format!("Expected impl Traits, got {:?}", value)),
                            }
                        })
                        .try_collect()?;
                    Ok(Ty::ImplTraits(ImplTraits {
                        bounds: TypeBounds {
                            bounds: args.into_iter().flat_map(|x| x.bounds).collect(),
                        },
                    })
                    .into())
                }))
            }
            BinOpKind::Sub => Ok(builtin_sub()),
            BinOpKind::Mul => Ok(builtin_mul()),
            BinOpKind::Div => Ok(self.builtin_div()),
            // BinOpKind::Mod => Ok(builtin_mod()),
            BinOpKind::Gt => Ok(builtin_gt()),
            BinOpKind::Lt => Ok(builtin_lt()),
            BinOpKind::Ge => Ok(builtin_ge()),
            BinOpKind::Le => Ok(builtin_le()),
            BinOpKind::Eq => Ok(builtin_eq()),
            BinOpKind::Ne => Ok(builtin_ne()),
            BinOpKind::BitAnd => Ok(self.builtin_bitand()),
            BinOpKind::Or => Ok(self.builtin_logical_or()),
            BinOpKind::And => Ok(self.builtin_logical_and()),
            // BinOpKind::BitOr => {}
            // BinOpKind::BitXor => {}
            // BinOpKind::Any(_) => {}
            _ => opt_bail!(format!("Could not process {:?}", op)),
        }
    }
}
