use eyre::{bail, Result};

use lang_core::ast::{Expr, ExprBinOp, ExprLoop};
use lang_core::ast::{Type, TypeInt, TypePrimitive, Value};
use lang_core::context::SharedScopedContext;
use lang_core::ops::BinOpKind;

use crate::emitter::MipsEmitter;
use crate::instruction::{MipsInstruction, MipsOpcode};
use crate::register::MipsRegister;

#[derive(Debug)]
pub struct EmitExprResult {
    pub ret: MipsRegister,
    pub instructions: Vec<MipsInstruction>,
}
impl EmitExprResult {
    pub fn new(ret: MipsRegister, instructions: Vec<MipsInstruction>) -> Self {
        Self { ret, instructions }
    }
}

impl MipsEmitter {
    pub fn emit_binop_int(
        &mut self,
        op: BinOpKind,
        ret: MipsRegister,
        lhs: MipsRegister,
        rhs: MipsRegister,
        _ctx: &SharedScopedContext,
    ) -> Result<Vec<MipsInstruction>> {
        let opcode = MipsOpcode::from_binop(op)?;
        if opcode.is_r_type() {
            Ok(vec![MipsInstruction::from_opcode_r(opcode, ret, lhs, rhs)])
        } else if opcode.followed_by_mflo() {
            let ins = MipsInstruction::from_opcode_mult_div_mod(opcode, lhs, rhs);
            let ins_mflo = MipsInstruction::Mflo { rd: ret };
            Ok(vec![ins, ins_mflo])
        } else {
            bail!("Unsupported binop {}", op);
        }
    }
    pub fn emit_binop_impl(
        &mut self,
        op: BinOpKind,
        lhs: MipsRegister,
        rhs: MipsRegister,
        ret: MipsRegister,
        ty: Type,
        _ctx: &SharedScopedContext,
    ) -> Result<Vec<MipsInstruction>> {
        match ty {
            Type::Primitive(TypePrimitive::Int(TypeInt::I32)) => {
                self.emit_binop_int(op, lhs, rhs, ret, _ctx)
            }
            _ => bail!("Unsupported type {}", ty),
        }
    }
    pub fn emit_value(
        &mut self,
        value: &Value,
        _ctx: &SharedScopedContext,
    ) -> Result<EmitExprResult> {
        match value {
            Value::Int(i) => {
                if i.value > i16::MAX as i64 {
                    bail!("Value {} is too large for MIPS", i.value);
                }
                let ret = self.get_register_t();
                let ins = self.emit_load_immediate(ret, i.value as i16);
                Ok(ins)
            }
            Value::Unit(_) => Ok(EmitExprResult::new(MipsRegister::Zero, vec![])),
            _ => bail!("Unsupported value {}", value),
        }
    }
    pub fn emit_binop(
        &mut self,
        binop: &ExprBinOp,
        ctx: &SharedScopedContext,
    ) -> Result<EmitExprResult> {
        let lhs = self.emit_expr(&binop.lhs, ctx)?;
        let rhs = self.emit_expr(&binop.rhs, ctx)?;
        let dst = self.get_register_t();
        let op = self.emit_binop_impl(
            binop.kind.clone(),
            lhs.ret,
            rhs.ret,
            dst,
            Type::Primitive(TypePrimitive::Int(TypeInt::I32)),
            ctx,
        )?;
        let result =
            EmitExprResult::new(dst, vec![lhs.instructions, rhs.instructions, op].concat());
        Ok(result)
    }
    pub fn emit_loop(
        &mut self,
        l: &ExprLoop,
        _ctx: &SharedScopedContext,
    ) -> Result<EmitExprResult> {
        let lbl = self.get_label();
        let mut ins = vec![];
        let label = MipsInstruction::Label { name: lbl.clone() };
        ins.push(label);
        ins.extend(self.emit_expr(&l.body, _ctx)?.instructions);
        let jump = MipsInstruction::J { label: lbl.clone() };
        ins.push(jump);
        Ok(EmitExprResult::new(MipsRegister::Zero, ins))
    }
    pub fn emit_expr(&mut self, expr: &Expr, ctx: &SharedScopedContext) -> Result<EmitExprResult> {
        match expr {
            Expr::BinOp(op) => self.emit_binop(op, ctx),
            Expr::Loop(l) => self.emit_loop(l, ctx),
            Expr::Value(value) => self.emit_value(value, ctx),

            _ => bail!("Unsupported expr {}", expr),
        }
    }
}
