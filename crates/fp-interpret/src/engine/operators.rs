use super::*;
use num_traits::Zero;

impl<'ctx> AstInterpreter<'ctx> {
    pub(super) fn evaluate_binop(&self, op: BinOpKind, lhs: Value, rhs: Value) -> Result<Value> {
        match op {
            BinOpKind::Add | BinOpKind::AddTrait => self.binop_add(lhs, rhs),
            BinOpKind::Sub => self.binop_sub(lhs, rhs),
            BinOpKind::Mul => self.binop_mul(lhs, rhs),
            BinOpKind::Div => self.binop_div(lhs, rhs),
            BinOpKind::Mod => self.binop_mod(lhs, rhs),
            BinOpKind::Shl | BinOpKind::Shr => self.binop_shift(op, lhs, rhs),
            BinOpKind::Gt | BinOpKind::Ge | BinOpKind::Lt | BinOpKind::Le => {
                self.binop_ordering(op, lhs, rhs)
            }
            BinOpKind::Eq | BinOpKind::Ne => self.binop_equality(op, lhs, rhs),
            BinOpKind::Or | BinOpKind::And => self.binop_logical(op, lhs, rhs),
            BinOpKind::BitAnd | BinOpKind::BitOr | BinOpKind::BitXor => {
                self.binop_bitwise(op, lhs, rhs)
            }
        }
    }

    fn binop_add(&self, lhs: Value, rhs: Value) -> Result<Value> {
        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::int(l.value + r.value)),
            (Value::BigInt(l), Value::BigInt(r)) => Ok(Value::big_int(l.value + r.value)),
            (Value::Decimal(l), Value::Decimal(r)) => Ok(Value::decimal(l.value + r.value)),
            (Value::BigDecimal(l), Value::BigDecimal(r)) => {
                Ok(Value::big_decimal(l.value + r.value))
            }
            (Value::String(l), Value::String(r)) => {
                Ok(Value::string(format!("{}{}", l.value, r.value)))
            }
            (Value::Type(lhs_ty), Value::Type(rhs_ty)) => Ok(Value::Type(Ty::TypeBinaryOp(
                fp_core::ast::TypeBinaryOp {
                    kind: TypeBinaryOpKind::Add,
                    lhs: Box::new(lhs_ty),
                    rhs: Box::new(rhs_ty),
                }
                .into(),
            ))),
            other => Err(interpretation_error(format!(
                "unsupported operands for '+': {:?}",
                other
            ))),
        }
    }
    fn binop_sub(&self, lhs: Value, rhs: Value) -> Result<Value> {
        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::int(l.value - r.value)),
            (Value::BigInt(l), Value::BigInt(r)) => Ok(Value::big_int(l.value - r.value)),
            (Value::Decimal(l), Value::Decimal(r)) => Ok(Value::decimal(l.value - r.value)),
            (Value::BigDecimal(l), Value::BigDecimal(r)) => {
                Ok(Value::big_decimal(l.value - r.value))
            }
            (Value::Type(lhs_ty), Value::Type(rhs_ty)) => Ok(Value::Type(Ty::TypeBinaryOp(
                fp_core::ast::TypeBinaryOp {
                    kind: TypeBinaryOpKind::Subtract,
                    lhs: Box::new(lhs_ty),
                    rhs: Box::new(rhs_ty),
                }
                .into(),
            ))),
            other => Err(interpretation_error(format!(
                "unsupported operands for '-': {:?}",
                other
            ))),
        }
    }
    fn binop_mul(&self, lhs: Value, rhs: Value) -> Result<Value> {
        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::int(l.value * r.value)),
            (Value::BigInt(l), Value::BigInt(r)) => Ok(Value::big_int(l.value * r.value)),
            (Value::Decimal(l), Value::Decimal(r)) => Ok(Value::decimal(l.value * r.value)),
            (Value::BigDecimal(l), Value::BigDecimal(r)) => {
                Ok(Value::big_decimal(l.value * r.value))
            }
            other => Err(interpretation_error(format!(
                "unsupported operands for '*': {:?}",
                other
            ))),
        }
    }
    fn binop_div(&self, lhs: Value, rhs: Value) -> Result<Value> {
        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => {
                if r.value == 0 {
                    Err(interpretation_error("division by zero".to_string()))
                } else if l.value % r.value == 0 {
                    Ok(Value::int(l.value / r.value))
                } else {
                    Ok(Value::decimal(l.value as f64 / r.value as f64))
                }
            }
            (Value::BigInt(l), Value::BigInt(r)) => {
                if r.value == 0.into() {
                    Err(interpretation_error("division by zero".to_string()))
                } else {
                    Ok(Value::big_int(l.value / r.value))
                }
            }
            (Value::Decimal(l), Value::Decimal(r)) => {
                if r.value == 0.0 {
                    Err(interpretation_error("division by zero".to_string()))
                } else {
                    Ok(Value::decimal(l.value / r.value))
                }
            }
            (Value::BigDecimal(l), Value::BigDecimal(r)) => {
                if r.value.is_zero() {
                    Err(interpretation_error("division by zero".to_string()))
                } else {
                    Ok(Value::big_decimal(l.value / r.value))
                }
            }
            other => Err(interpretation_error(format!(
                "unsupported operands for '/': {:?}",
                other
            ))),
        }
    }
    fn binop_mod(&self, lhs: Value, rhs: Value) -> Result<Value> {
        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::int(l.value % r.value)),
            (Value::BigInt(l), Value::BigInt(r)) => Ok(Value::big_int(l.value % r.value)),
            other => Err(interpretation_error(format!(
                "unsupported operands for '%': {:?}",
                other
            ))),
        }
    }
    fn binop_ordering(&self, op: BinOpKind, lhs: Value, rhs: Value) -> Result<Value> {
        use std::cmp::Ordering;
        let ordering = match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => l.value.cmp(&r.value),
            (Value::BigInt(l), Value::BigInt(r)) => l.value.cmp(&r.value),
            (Value::Decimal(l), Value::Decimal(r)) => l.value.total_cmp(&r.value),
            (Value::BigDecimal(l), Value::BigDecimal(r)) => l.value.cmp(&r.value),
            (Value::String(l), Value::String(r)) => l.value.cmp(&r.value),
            other => {
                return Err(interpretation_error(format!(
                    "unsupported operands for ordering comparison: {:?}",
                    other
                )))
            }
        };
        let result = match op {
            BinOpKind::Gt => ordering == Ordering::Greater,
            BinOpKind::Ge => ordering != Ordering::Less,
            BinOpKind::Lt => ordering == Ordering::Less,
            BinOpKind::Le => ordering != Ordering::Greater,
            _ => unreachable!(),
        };
        Ok(Value::bool(result))
    }
    fn binop_equality(&self, op: BinOpKind, lhs: Value, rhs: Value) -> Result<Value> {
        let eq = lhs == rhs;
        let result = match op {
            BinOpKind::Eq => eq,
            BinOpKind::Ne => !eq,
            _ => unreachable!(),
        };
        Ok(Value::bool(result))
    }
    fn binop_logical(&self, op: BinOpKind, lhs: Value, rhs: Value) -> Result<Value> {
        let (l, r) = match (lhs, rhs) {
            (Value::Bool(l), Value::Bool(r)) => (l.value, r.value),
            other => {
                return Err(interpretation_error(format!(
                    "logical operators require booleans, found {:?}",
                    other
                )))
            }
        };
        let result = match op {
            BinOpKind::Or => l || r,
            BinOpKind::And => l && r,
            _ => unreachable!(),
        };
        Ok(Value::bool(result))
    }
    fn binop_bitwise(&self, op: BinOpKind, lhs: Value, rhs: Value) -> Result<Value> {
        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => {
                let result = match op {
                    BinOpKind::BitAnd => l.value & r.value,
                    BinOpKind::BitOr => l.value | r.value,
                    BinOpKind::BitXor => l.value ^ r.value,
                    _ => unreachable!(),
                };
                Ok(Value::int(result))
            }
            (Value::BigInt(l), Value::BigInt(r)) => {
                let result = match op {
                    BinOpKind::BitAnd => l.value & r.value,
                    BinOpKind::BitOr => l.value | r.value,
                    BinOpKind::BitXor => l.value ^ r.value,
                    _ => unreachable!(),
                };
                Ok(Value::big_int(result))
            }
            (Value::Type(lhs_ty), Value::Type(rhs_ty)) => {
                let kind = match op {
                    BinOpKind::BitAnd => TypeBinaryOpKind::Intersect,
                    BinOpKind::BitOr => TypeBinaryOpKind::Union,
                    _ => {
                        return Err(interpretation_error(format!(
                            "unsupported operands for bitwise: {:?}",
                            (op, Value::Type(lhs_ty), Value::Type(rhs_ty))
                        )))
                    }
                };
                Ok(Value::Type(Ty::TypeBinaryOp(
                    fp_core::ast::TypeBinaryOp {
                        kind,
                        lhs: Box::new(lhs_ty),
                        rhs: Box::new(rhs_ty),
                    }
                    .into(),
                )))
            }
            other => Err(interpretation_error(format!(
                "bitwise operators require integers, found {:?}",
                other
            ))),
        }
    }

    fn binop_shift(&self, op: BinOpKind, lhs: Value, rhs: Value) -> Result<Value> {
        // Shifts are only defined for integers; match the bitwise operator constraints.
        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => {
                let result = match op {
                    BinOpKind::Shl => l.value << r.value,
                    BinOpKind::Shr => l.value >> r.value,
                    _ => unreachable!(),
                };
                Ok(Value::int(result))
            }
            (Value::BigInt(l), Value::BigInt(r)) => {
                let shift = usize::try_from(&r.value).map_err(|_| {
                    interpretation_error("shift amount must be a non-negative integer".to_string())
                })?;
                let result = match op {
                    BinOpKind::Shl => l.value << shift,
                    BinOpKind::Shr => l.value >> shift,
                    _ => unreachable!(),
                };
                Ok(Value::big_int(result))
            }
            other => Err(interpretation_error(format!(
                "shift operators require integers, found {:?}",
                other
            ))),
        }
    }

    pub(super) fn evaluate_unary(&self, op: UnOpKind, value: Value) -> Result<Value> {
        match op {
            UnOpKind::Not => match value {
                Value::Bool(b) => Ok(Value::bool(!b.value)),
                other => Err(interpretation_error(format!(
                    "operator '!' requires boolean, found {}",
                    other
                ))),
            },
            UnOpKind::Neg => match value {
                Value::Int(i) => Ok(Value::int(-i.value)),
                Value::BigInt(i) => Ok(Value::big_int(-i.value)),
                Value::Decimal(d) => Ok(Value::decimal(-d.value)),
                Value::BigDecimal(d) => Ok(Value::big_decimal(-d.value)),
                other => Err(interpretation_error(format!(
                    "operator '-' requires numeric operand, found {}",
                    other
                ))),
            },
            UnOpKind::Deref => Err(interpretation_error(
                "unsupported unary operator in AST interpretation".to_string(),
            )),
            UnOpKind::Any(kind) => {
                if kind.as_str() == "box" {
                    return Ok(Value::Any(AnyBox::new(RuntimeBox { value })));
                }
                Err(interpretation_error(
                    "unsupported unary operator in AST interpretation".to_string(),
                ))
            }
        }
    }
}
