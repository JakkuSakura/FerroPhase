use eyre::{bail, Result};

use lang_core::ast::{AstExpr, AstType, AstValue, TypeInt, TypePrimitive};
use lang_core::ast::{ExprBinOp, ExprBlock, ExprIf, ExprInvoke, ExprInvokeTarget, ExprLoop};
use lang_core::context::SharedScopedContext;
use lang_core::ops::BinOpKind;

use crate::emitter::MipsEmitter;
use crate::instruction::{MipsInstruction, MipsOpcode};
use crate::storage::register::{MipsRegister, MipsRegisterOwned};

#[derive(Debug)]
pub struct MipsEmitExprResult {
    pub ret: MipsRegisterOwned,
    pub instructions: Vec<MipsInstruction>,
}
impl MipsEmitExprResult {
    pub fn new(ret: MipsRegisterOwned, instructions: Vec<MipsInstruction>) -> Self {
        Self { ret, instructions }
    }
}

impl MipsEmitter {
    pub fn emit_binop_int(
        &mut self,
        op: BinOpKind,
        mut lhs: MipsRegisterOwned,
        mut rhs: MipsRegisterOwned,
        _ctx: &SharedScopedContext,
    ) -> Result<MipsEmitExprResult> {
        let opcode = MipsOpcode::from_binop(op)?;
        if opcode.is_r_type() {
            let ret = self.stack.register_manager().acquire_t().unwrap();
            let ins = MipsInstruction::from_opcode_r(opcode, ret.get(), lhs.get(), rhs.get());
            Ok(MipsEmitExprResult {
                ret,
                instructions: vec![ins],
            })
        } else if opcode.followed_by_mflo() {
            let ret = self.stack.register_manager().acquire_t().unwrap();

            let ins = MipsInstruction::from_opcode_mult_div_mod(opcode, lhs.get(), rhs.get());
            let ins_mflo = MipsInstruction::Mflo { rd: ret.get() };
            Ok(MipsEmitExprResult {
                ret,
                instructions: vec![ins, ins_mflo],
            })
        } else if opcode == MipsOpcode::Slt {
            let ret = self.stack.register_manager().acquire_t().unwrap();
            match op {
                BinOpKind::Lt => {}
                BinOpKind::Ge => {
                    // swap lhs and rhs
                    let tmp = lhs;
                    lhs = rhs;
                    rhs = tmp;
                }
                _ => bail!("Unsupported binop {}", op),
            }
            let ins = MipsInstruction::Slt {
                rd: ret.get(),
                rs: lhs.get(),
                rt: rhs.get(),
            };
            Ok(MipsEmitExprResult {
                ret,
                instructions: vec![ins],
            })
        } else {
            bail!("Unsupported binop {}", op);
        }
    }
    pub fn emit_binop_impl(
        &mut self,
        op: BinOpKind,
        lhs: MipsRegisterOwned,
        rhs: MipsRegisterOwned,
        ty: AstType,
        _ctx: &SharedScopedContext,
    ) -> Result<MipsEmitExprResult> {
        match ty {
            AstType::Primitive(TypePrimitive::Int(TypeInt::I32)) => {
                self.emit_binop_int(op, lhs, rhs, _ctx)
            }
            _ => bail!("Unsupported type {}", ty),
        }
    }
    pub fn emit_value(
        &mut self,
        value: &AstValue,
        _ctx: &SharedScopedContext,
    ) -> Result<MipsEmitExprResult> {
        match value {
            AstValue::Int(i) => {
                if i.value > i16::MAX as i64 {
                    bail!("Value {} is too large for MIPS", i.value);
                }
                let ins = self.emit_load_immediate(i.value as i16);
                Ok(ins)
            }
            AstValue::Unit(_) => Ok(MipsEmitExprResult::new(MipsRegisterOwned::zero(), vec![])),
            _ => bail!("Unsupported value {}", value),
        }
    }
    pub fn emit_binop(
        &mut self,
        binop: &ExprBinOp,
        ctx: &SharedScopedContext,
    ) -> Result<MipsEmitExprResult> {
        let lhs = self.emit_expr(&binop.lhs, ctx)?;
        let rhs = self.emit_expr(&binop.rhs, ctx)?;

        let op = self.emit_binop_impl(
            binop.kind.clone(),
            lhs.ret,
            rhs.ret,
            AstType::Primitive(TypePrimitive::Int(TypeInt::I32)),
            ctx,
        )?;
        let result = MipsEmitExprResult::new(
            op.ret,
            vec![lhs.instructions, rhs.instructions, op.instructions].concat(),
        );
        Ok(result)
    }
    pub fn emit_loop(
        &mut self,
        l: &ExprLoop,
        _ctx: &SharedScopedContext,
    ) -> Result<MipsEmitExprResult> {
        let lbl = self.get_label();
        let mut ins = vec![];
        let label = MipsInstruction::Label { name: lbl.clone() };
        ins.push(label);
        ins.extend(self.emit_expr(&l.body, _ctx)?.instructions);
        let jump = MipsInstruction::J { label: lbl.clone() };
        ins.push(jump);
        Ok(MipsEmitExprResult::new(MipsRegisterOwned::zero(), ins))
    }
    pub fn emit_if(
        &mut self,
        if_: &ExprIf,
        _ctx: &SharedScopedContext,
    ) -> Result<MipsEmitExprResult> {
        let label_endif = self.get_label();
        let label_else = self.get_label();
        let cond = self.emit_expr(&if_.cond, _ctx)?;
        let mut ins = vec![];
        ins.extend(cond.instructions);
        ins.push(MipsInstruction::Beq {
            rs: cond.ret.get(),
            rt: MipsRegister::Zero,
            label: if if_.elze.is_some() {
                label_else.clone()
            } else {
                label_endif.clone()
            },
        });
        let then = self.emit_expr(&if_.then, _ctx)?;
        ins.extend(then.instructions);
        if let Some(else_) = &if_.elze {
            ins.push(MipsInstruction::J {
                label: label_endif.clone(),
            });
            ins.push(MipsInstruction::Label { name: label_else });
            let else_ = self.emit_expr(else_, _ctx)?;
            ins.extend(else_.instructions);

            // copy the result of then to the result of else
            // Move is a pseudo instruction
            ins.push(MipsInstruction::Add {
                rd: then.ret.get(),
                rs: else_.ret.get(),
                rt: MipsRegister::Zero,
            });
        }
        ins.push(MipsInstruction::Label { name: label_endif });

        Ok(MipsEmitExprResult {
            ret: then.ret,
            instructions: ins,
        })
    }
    pub fn emit_block(
        &mut self,
        block: &ExprBlock,
        ctx: &SharedScopedContext,
    ) -> Result<MipsEmitExprResult> {
        let mut ins = vec![];
        for stmt in block.first_stmts() {
            ins.extend(self.emit_statement(stmt, ctx)?.instructions);
        }
        let ret = if let Some(expr) = &block.last_expr() {
            self.emit_expr(&*expr, ctx)?
        } else {
            MipsEmitExprResult::new(MipsRegisterOwned::zero(), vec![])
        };
        ins.extend(ret.instructions);
        Ok(MipsEmitExprResult::new(ret.ret, ins))
    }
    pub fn emit_invoke(
        &mut self,
        invoke: &ExprInvoke,
        _ctx: &SharedScopedContext,
    ) -> Result<MipsEmitExprResult> {
        match &invoke.target {
            ExprInvokeTarget::Function(ident) => Ok(self.emit_invoke_function(&ident.to_string())),
            _ => bail!("Unsupported invoke {:?}", invoke.target),
        }
    }

    pub fn emit_expr(
        &mut self,
        expr: &AstExpr,
        ctx: &SharedScopedContext,
    ) -> Result<MipsEmitExprResult> {
        match expr {
            AstExpr::Value(value) => self.emit_value(value, ctx),
            AstExpr::BinOp(op) => self.emit_binop(op, ctx),
            AstExpr::Loop(l) => self.emit_loop(l, ctx),
            AstExpr::If(if_) => self.emit_if(if_, ctx),
            AstExpr::Block(b) => self.emit_block(b, ctx),
            AstExpr::Invoke(x) => self.emit_invoke(x, ctx),
            _ => bail!("Unsupported expr {}", expr),
        }
    }
}
