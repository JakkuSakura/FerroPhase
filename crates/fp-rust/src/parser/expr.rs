use super::RustParser;

use crate::{parser, RawExpr, RawExprMacro, RawStmtMacro};
use fp_core::ast::Ident;
use fp_core::ast::*;
use fp_core::error::Result;
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};
use fp_core::ops::{BinOpKind, UnOpKind};
use itertools::Itertools;
use proc_macro2::Span;
use quote::ToTokens;
use std::sync::atomic::{AtomicUsize, Ordering};

static FOR_LOOP_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub(super) struct ExprParser<'a> {
    parser: &'a RustParser,
}

impl<'a> ExprParser<'a> {
    pub fn new(parser: &'a RustParser) -> Self {
        Self { parser }
    }

    fn err<T>(&self, message: impl Into<String>, fallback: T) -> Result<T> {
        self.parser.error(message, fallback)
    }

    pub fn parse_expr(&self, expr: syn::Expr) -> Result<Expr> {
        let expr = match expr {
            syn::Expr::Binary(b) => self.parse_expr_binary(b)?,
            syn::Expr::Unary(u) => self.parse_unary(u)?.into(),
            syn::Expr::Block(b) if b.label.is_none() => Expr::block(self.parse_block(b.block)?),
            syn::Expr::Call(c) => self.parse_expr_call(c)?.into(),
            syn::Expr::If(i) => self.parse_expr_if(i)?.into(),
            syn::Expr::Loop(l) => self.parse_expr_loop(l)?.into(),
            syn::Expr::Lit(l) => Expr::value(self.parse_literal(l.lit)?),
            syn::Expr::Macro(m) => self.parse_expr_macro(m)?,
            syn::Expr::MethodCall(c) => self.parse_expr_method_call(c)?.into(),
            syn::Expr::Index(i) => self.parse_expr_index(i)?.into(),
            syn::Expr::Path(p) => Expr::path(parser::parse_path(p.path)?),
            syn::Expr::Reference(r) => self.parse_expr_reference(r)?.into(),
            syn::Expr::Tuple(t) if t.elems.is_empty() => Expr::unit(),
            syn::Expr::Tuple(t) => self.parse_expr_tuple(t)?.into(),
            syn::Expr::Struct(s) => self.parse_expr_struct(s)?.into(),
            syn::Expr::Const(c) => self.parse_expr_const(c)?,
            syn::Expr::Paren(p) => self.parse_expr_paren(p)?.into(),
            syn::Expr::Range(r) => self.parse_expr_range(r)?.into(),
            syn::Expr::ForLoop(f) => self.parse_expr_for(f)?,
            syn::Expr::Field(f) => self.parse_expr_field(f)?.into(),
            syn::Expr::Try(t) => self.parse_expr_try(t)?.into(),
            syn::Expr::While(w) => self.parse_expr_while(w)?.into(),
            syn::Expr::Let(l) => self.parse_expr_let(l)?.into(),
            syn::Expr::Closure(c) => self.parse_expr_closure(c)?.into(),
            syn::Expr::Array(a) => self.parse_expr_array(a)?.into(),
            syn::Expr::Repeat(r) => self.parse_expr_repeat(r)?.into(),
            syn::Expr::Assign(a) => self.parse_expr_assign(a)?.into(),
            syn::Expr::Break(b) => self.parse_expr_break(b)?,
            syn::Expr::Continue(_) => self.parse_expr_continue()?,
            syn::Expr::Return(r) => self.parse_expr_return(r)?,
            raw => {
                tracing::debug!("RawExpr {:?}", raw);
                Expr::any(RawExpr { raw })
            }
        };
        Ok(expr)
    }

    fn parse_expr_for(&self, f: syn::ExprForLoop) -> Result<Expr> {
        let iter_expr_clone = (*f.expr).clone();
        let range = match iter_expr_clone {
            syn::Expr::Range(range) => range,
            _ => {
                return Ok(Expr::any(RawExpr {
                    raw: syn::Expr::ForLoop(f),
                }));
            }
        };

        let end_expr = match range.end {
            Some(expr) => *expr,
            None => {
                return Ok(Expr::any(RawExpr {
                    raw: syn::Expr::ForLoop(f),
                }));
            }
        };

        let start_expr = range
            .start
            .map(|expr| *expr)
            .unwrap_or_else(|| syn::parse_quote!(0));

        let idx = FOR_LOOP_COUNTER.fetch_add(1, Ordering::Relaxed);
        let iter_ident = syn::Ident::new(&format!("__fp_for_iter_{}", idx), Span::call_site());
        let end_ident = syn::Ident::new(&format!("__fp_for_end_{}", idx), Span::call_site());
        let pat = *f.pat;
        let body = f.body;

        let comparison: syn::Expr = match range.limits {
            syn::RangeLimits::Closed(_) => syn::parse_quote!(#iter_ident > #end_ident),
            syn::RangeLimits::HalfOpen(_) => syn::parse_quote!(#iter_ident >= #end_ident),
        };

        let desugared: syn::Expr = if let Some(label) = f.label {
            let loop_label = label.name;
            let break_label = loop_label.clone();
            syn::parse_quote!({
                let mut #iter_ident = #start_expr;
                let #end_ident = #end_expr;
                #loop_label: loop {
                    if #comparison {
                        break #break_label;
                    }
                    let #pat = #iter_ident;
                    #iter_ident += 1;
                    #body
                }
            })
        } else {
            syn::parse_quote!({
                let mut #iter_ident = #start_expr;
                let #end_ident = #end_expr;
                loop {
                    if #comparison {
                        break;
                    }
                    let #pat = #iter_ident;
                    #iter_ident += 1;
                    #body
                }
            })
        };

        self.parse_expr(desugared)
    }

    fn parse_expr_array(&self, a: syn::ExprArray) -> Result<ExprArray> {
        Ok(ExprArray {
            values: a
                .elems
                .into_iter()
                .map(|expr| self.parse_expr(expr))
                .try_collect()?,
        })
    }

    fn parse_expr_repeat(&self, r: syn::ExprRepeat) -> Result<ExprArrayRepeat> {
        Ok(ExprArrayRepeat {
            elem: self.parse_expr(*r.expr)?.into(),
            len: self.parse_expr(*r.len)?.into(),
        })
    }

    fn parse_expr_closure(&self, c: syn::ExprClosure) -> Result<ExprClosure> {
        let movability = c.movability.is_some();
        let params: Vec<_> = c
            .inputs
            .into_iter()
            .map(|x| self.parser.parse_pat(x))
            .try_collect()?;
        let ret_ty = match c.output {
            syn::ReturnType::Default => None,
            syn::ReturnType::Type(_, ty) => Some(self.parser.parse_type(*ty)?.into()),
        };
        let body = self.parse_expr(*c.body)?.into();
        Ok(ExprClosure {
            movability: Some(movability),
            params,
            ret_ty,
            body,
        })
    }

    fn parse_expr_let(&self, l: syn::ExprLet) -> Result<ExprLet> {
        Ok(ExprLet {
            pat: self.parser.parse_pat(*l.pat)?.into(),
            expr: self.parse_expr(*l.expr)?.into(),
        })
    }

    fn parse_expr_while(&self, w: syn::ExprWhile) -> Result<ExprWhile> {
        Ok(ExprWhile {
            cond: self.parse_expr(*w.cond)?.into(),
            body: Expr::block(self.parse_block(w.body)?).into(),
        })
    }

    fn parse_expr_try(&self, t: syn::ExprTry) -> Result<ExprTry> {
        Ok(ExprTry {
            expr: self.parse_expr(*t.expr)?.into(),
        })
    }

    fn parse_expr_field(&self, f: syn::ExprField) -> Result<ExprSelect> {
        let obj = self.parse_expr(*f.base)?.into();
        let field = self.parse_field_member(f.member);
        Ok(ExprSelect {
            obj,
            field,
            select: ExprSelectType::Field,
        })
    }

    fn parse_field_member(&self, f: syn::Member) -> Ident {
        match f {
            syn::Member::Named(n) => parser::parse_ident(n),
            syn::Member::Unnamed(n) => Ident::new(n.index.to_string()),
        }
    }

    fn parse_literal(&self, lit: syn::Lit) -> Result<Value> {
        Ok(match lit {
            syn::Lit::Int(i) => Value::Int(ValueInt::new(
                i.base10_parse().map_err(|e| eyre::eyre!(e.to_string()))?,
            )),
            syn::Lit::Float(i) => Value::Decimal(ValueDecimal::new(
                i.base10_parse().map_err(|e| eyre::eyre!(e.to_string()))?,
            )),
            syn::Lit::Str(s) => Value::String(ValueString::new_ref(s.value())),
            syn::Lit::Bool(b) => Value::Bool(ValueBool::new(b.value)),
            other => {
                tracing::debug!("Literal not supported: {:?}", other.to_token_stream());
                return self.err(
                    format!("Lit not supported: {:?}", other.to_token_stream()),
                    Value::Unit(ValueUnit),
                );
            }
        })
    }

    fn parse_unary(&self, u: syn::ExprUnary) -> Result<ExprUnOp> {
        let expr = self.parse_expr(*u.expr)?;
        let op = match u.op {
            syn::UnOp::Neg(_) => UnOpKind::Neg,
            syn::UnOp::Not(_) => UnOpKind::Not,
            syn::UnOp::Deref(_) => UnOpKind::Deref,
            other => {
                tracing::debug!("Unary not supported: {:?}", other);
                return self.err(
                    format!("Unary op not supported: {:?}", other),
                    ExprUnOp {
                        op: UnOpKind::Not,
                        val: Expr::unit().into(),
                    },
                );
            }
        };
        Ok(ExprUnOp {
            op,
            val: expr.into(),
        })
    }

    /// returns: statement, with_semicolon
    pub fn parse_stmt(&self, stmt: syn::Stmt) -> Result<(BlockStmt, bool)> {
        Ok(match stmt {
            syn::Stmt::Local(l) => {
                let pat = self.parser.parse_pat(l.pat)?;
                let (init, diverge) = match l.init {
                    Some(init) => {
                        let init_expr = self.parse_expr(*init.expr)?;
                        let diverge = init.diverge.map(|x| self.parse_expr(*x.1)).transpose()?;
                        (Some(init_expr), diverge)
                    }
                    None => (None, None),
                };
                (BlockStmt::Let(StmtLet { pat, init, diverge }), true)
            }
            syn::Stmt::Item(item) => (self.parser.parse_item(item).map(BlockStmt::item)?, true),
            syn::Stmt::Expr(e, semicolon) => {
                if let syn::Expr::Verbatim(v) = &e {
                    if v.is_empty() {
                        return Ok((BlockStmt::noop().into(), semicolon.is_some()));
                    }
                }
                (
                    BlockStmt::Expr(
                        BlockStmtExpr::new(self.parse_expr(e)?).with_semicolon(semicolon.is_some()),
                    ),
                    semicolon.is_some(),
                )
            }
            syn::Stmt::Macro(raw) => (self.parse_stmt_macro(raw)?, true),
        })
    }

    pub fn parse_block(&self, block: syn::Block) -> Result<ExprBlock> {
        let mut stmts = vec![];
        for stmt in block.stmts.into_iter() {
            let (stmt, _) = self.parse_stmt(stmt)?;
            stmts.push(stmt);
        }
        Ok(ExprBlock::new_stmts(stmts))
    }

    fn parse_expr_reference(&self, item: syn::ExprReference) -> Result<ExprReference> {
        Ok(ExprReference {
            referee: self.parse_expr(*item.expr)?.into(),
            mutable: Some(item.mutability.is_some()),
        })
    }

    fn parse_expr_call(&self, call: syn::ExprCall) -> Result<ExprInvoke> {
        let fun = self.parse_expr(*call.func)?;
        let args: Vec<_> = call
            .args
            .into_iter()
            .map(|arg| self.parse_expr(arg))
            .try_collect()?;

        Ok(ExprInvoke {
            target: ExprInvokeTarget::expr(fun),
            args,
        })
    }

    fn parse_expr_method_call(&self, call: syn::ExprMethodCall) -> Result<ExprInvoke> {
        Ok(ExprInvoke {
            target: ExprInvokeTarget::Method(
                ExprSelect {
                    obj: self.parse_expr(*call.receiver)?.into(),
                    field: parser::parse_ident(call.method),
                    select: ExprSelectType::Method,
                }
                .into(),
            )
            .into(),
            args: call
                .args
                .into_iter()
                .map(|arg| self.parse_expr(arg))
                .try_collect()?,
        })
    }

    fn parse_expr_index(&self, index: syn::ExprIndex) -> Result<ExprIndex> {
        Ok(ExprIndex {
            obj: self.parse_expr(*index.expr)?.into(),
            index: self.parse_expr(*index.index)?.into(),
        })
    }

    fn parse_expr_if(&self, i: syn::ExprIf) -> Result<ExprIf> {
        let cond = self.parse_expr(*i.cond)?.into();
        let then = self.parse_block(i.then_branch)?;
        let elze = if let Some((_, e)) = i.else_branch {
            Some(self.parse_expr(*e)?.into())
        } else {
            None
        };
        Ok(ExprIf {
            cond,
            then: Expr::block(then).into(),
            elze,
        })
    }

    fn parse_expr_loop(&self, l: syn::ExprLoop) -> Result<ExprLoop> {
        Ok(ExprLoop {
            label: l.label.map(|x| parser::parse_ident(x.name.ident)),
            body: Expr::block(self.parse_block(l.body)?).into(),
        })
    }

    fn parse_expr_binary(&self, b: syn::ExprBinary) -> Result<Expr> {
        let lhs_expr = self.parse_expr(*b.left)?;
        let rhs_expr = self.parse_expr(*b.right)?;

        let make_assign = |kind: BinOpKind, lhs_expr: Expr, rhs_expr: Expr| -> Expr {
            ExprAssign {
                target: lhs_expr.clone().into(),
                value: Expr::from(ExprKind::BinOp(ExprBinOp {
                    kind,
                    lhs: lhs_expr.into(),
                    rhs: rhs_expr.into(),
                }))
                .into(),
            }
            .into()
        };

        let kind = match b.op {
            syn::BinOp::AddAssign(_) => {
                return Ok(make_assign(BinOpKind::Add, lhs_expr, rhs_expr));
            }
            syn::BinOp::SubAssign(_) => {
                return Ok(make_assign(BinOpKind::Sub, lhs_expr, rhs_expr));
            }
            syn::BinOp::MulAssign(_) => {
                return Ok(make_assign(BinOpKind::Mul, lhs_expr, rhs_expr));
            }
            syn::BinOp::RemAssign(_) => {
                return Ok(make_assign(BinOpKind::Mod, lhs_expr, rhs_expr));
            }
            syn::BinOp::DivAssign(_) => {
                return Ok(make_assign(BinOpKind::Div, lhs_expr, rhs_expr));
            }
            syn::BinOp::Add(_) => BinOpKind::Add,
            syn::BinOp::Mul(_) => BinOpKind::Mul,
            syn::BinOp::Sub(_) => BinOpKind::Sub,
            syn::BinOp::Div(_) => BinOpKind::Div,
            syn::BinOp::Rem(_) => BinOpKind::Mod,
            syn::BinOp::Gt(_) => BinOpKind::Gt,
            syn::BinOp::Ge(_) => BinOpKind::Ge,
            syn::BinOp::Le(_) => BinOpKind::Le,
            syn::BinOp::Lt(_) => BinOpKind::Lt,
            syn::BinOp::Eq(_) => BinOpKind::Eq,
            syn::BinOp::Ne(_) => BinOpKind::Ne,
            syn::BinOp::BitOr(_) => BinOpKind::BitOr,
            syn::BinOp::BitAnd(_) => BinOpKind::BitAnd,
            syn::BinOp::BitXor(_) => BinOpKind::BitXor,
            syn::BinOp::Or(_) => BinOpKind::Or,
            syn::BinOp::And(_) => BinOpKind::And,
            other => {
                tracing::debug!("Binary op not supported: {:?}", other);
                return self.err(format!("Op not supported {:?}", other), Expr::unit());
            }
        };

        Ok(Expr::from(ExprKind::BinOp(ExprBinOp {
            kind,
            lhs: lhs_expr.into(),
            rhs: rhs_expr.into(),
        })))
    }

    fn parse_expr_tuple(&self, t: syn::ExprTuple) -> Result<ExprTuple> {
        let mut values = Vec::new();
        for e in t.elems {
            values.push(self.parse_expr(e)?);
        }
        Ok(ExprTuple { values })
    }

    fn parse_expr_field_value(&self, fv: syn::FieldValue) -> Result<ExprField> {
        Ok(ExprField {
            name: crate::parser::ty::parse_member(fv.member)?,
            value: self.parse_expr(fv.expr)?.into(),
        })
    }

    fn parse_expr_struct(&self, s: syn::ExprStruct) -> Result<ExprStruct> {
        Ok(ExprStruct {
            name: Expr::path(parser::parse_path(s.path)?).into(),
            fields: s
                .fields
                .into_iter()
                .map(|field| self.parse_expr_field_value(field))
                .try_collect()?,
        })
    }

    fn parse_expr_paren(&self, p: syn::ExprParen) -> Result<ExprParen> {
        Ok(ExprParen {
            expr: self.parse_expr(*p.expr)?.into(),
        })
    }

    fn parse_expr_range(&self, r: syn::ExprRange) -> Result<ExprRange> {
        let start = r
            .start
            .map(|x| self.parse_expr(*x))
            .transpose()?
            .map(|x| x.into());
        let limit = match r.limits {
            syn::RangeLimits::HalfOpen(_) => ExprRangeLimit::Exclusive,
            syn::RangeLimits::Closed(_) => ExprRangeLimit::Inclusive,
        };
        let end = r
            .end
            .map(|x| self.parse_expr(*x))
            .transpose()?
            .map(|x| x.into());
        Ok(ExprRange {
            start,
            limit,
            end,
            step: None,
        })
    }

    fn parse_expr_assign(&self, a: syn::ExprAssign) -> Result<ExprAssign> {
        Ok(ExprAssign {
            target: self.parse_expr(*a.left)?.into(),
            value: self.parse_expr(*a.right)?.into(),
        })
    }

    fn parse_expr_break(&self, b: syn::ExprBreak) -> Result<Expr> {
        let value = b.expr.map(|e| self.parse_expr(*e)).transpose()?;
        let args = if let Some(val) = value {
            vec![val]
        } else {
            Vec::new()
        };
        Ok(ExprIntrinsicCall::new(
            IntrinsicCallKind::Break,
            IntrinsicCallPayload::Args { args },
        )
        .into())
    }

    fn parse_expr_continue(&self) -> Result<Expr> {
        Ok(ExprIntrinsicCall::new(
            IntrinsicCallKind::Continue,
            IntrinsicCallPayload::Args { args: Vec::new() },
        )
        .into())
    }

    fn parse_expr_return(&self, r: syn::ExprReturn) -> Result<Expr> {
        let value = r.expr.map(|e| self.parse_expr(*e)).transpose()?;
        let args = if let Some(val) = value {
            vec![val]
        } else {
            Vec::new()
        };
        Ok(ExprIntrinsicCall::new(
            IntrinsicCallKind::Return,
            IntrinsicCallPayload::Args { args },
        )
        .into())
    }

    fn parse_expr_macro(&self, m: syn::ExprMacro) -> Result<Expr> {
        if Self::is_println_macro(&m.mac) {
            return self.parse_io_macro_to_function_call(&m.mac, IntrinsicCallKind::Println);
        }

        if Self::is_print_macro(&m.mac) {
            return self.parse_io_macro_to_function_call(&m.mac, IntrinsicCallKind::Print);
        }

        if Self::is_cfg_macro(&m.mac) {
            if let Some(expr) = self.parse_cfg_macro(&m.mac)? {
                return Ok(expr);
            }
        }

        if Self::is_input_macro(&m.mac) {
            return self.parse_input_macro(&m.mac);
        }

        if Self::is_fp_macro(&m.mac) {
            return self.parse_fp_macro(&m.mac);
        }

        if let Some(intrinsic_kind) = get_metaprogramming_intrinsic(&m.mac) {
            return self.parse_metaprogramming_intrinsic(&m.mac, intrinsic_kind);
        }

        Ok(Expr::any(RawExprMacro { raw: m }))
    }

    fn parse_stmt_macro(&self, raw: syn::StmtMacro) -> Result<BlockStmt> {
        if Self::is_println_macro(&raw.mac) {
            let call_expr =
                self.parse_io_macro_to_function_call(&raw.mac, IntrinsicCallKind::Println)?;
            tracing::debug!("parsed println! macro to function call");
            return Ok(BlockStmt::Expr(
                BlockStmtExpr::new(call_expr).with_semicolon(raw.semi_token.is_some()),
            ));
        }

        if Self::is_print_macro(&raw.mac) {
            let call_expr =
                self.parse_io_macro_to_function_call(&raw.mac, IntrinsicCallKind::Print)?;
            tracing::debug!("parsed print! macro to function call");
            return Ok(BlockStmt::Expr(
                BlockStmtExpr::new(call_expr).with_semicolon(raw.semi_token.is_some()),
            ));
        }

        if Self::is_cfg_macro(&raw.mac) {
            if let Some(expr) = self.parse_cfg_macro(&raw.mac)? {
                return Ok(BlockStmt::Expr(
                    BlockStmtExpr::new(expr).with_semicolon(raw.semi_token.is_some()),
                ));
            }
        }

        if Self::is_input_macro(&raw.mac) {
            let call_expr = self.parse_input_macro(&raw.mac)?;
            return Ok(BlockStmt::Expr(
                BlockStmtExpr::new(call_expr).with_semicolon(raw.semi_token.is_some()),
            ));
        }

        if Self::is_fp_macro(&raw.mac) {
            let call_expr = self.parse_fp_macro(&raw.mac)?;
            tracing::debug!("parsed fp! macro");
            return Ok(BlockStmt::Expr(
                BlockStmtExpr::new(call_expr).with_semicolon(raw.semi_token.is_some()),
            ));
        }

        Ok(BlockStmt::any(RawStmtMacro { raw }))
    }

    fn parse_cfg_macro(&self, mac: &syn::Macro) -> Result<Option<Expr>> {
        let tokens = mac
            .tokens
            .to_string()
            .chars()
            .filter(|ch| !ch.is_whitespace())
            .collect::<String>();

        if tokens == "debug_assertions" {
            return Ok(Some(self.debug_assertions_intrinsic()));
        }

        if tokens == "not(debug_assertions)" {
            let expr = self.debug_assertions_intrinsic();
            return Ok(Some(
                ExprUnOp {
                    op: UnOpKind::Not,
                    val: Box::new(expr),
                }
                .into(),
            ));
        }

        Ok(None)
    }

    fn debug_assertions_intrinsic(&self) -> Expr {
        ExprIntrinsicCall::new(
            IntrinsicCallKind::DebugAssertions,
            IntrinsicCallPayload::Args { args: Vec::new() },
        )
        .into()
    }

    fn parse_input_macro(&self, mac: &syn::Macro) -> Result<Expr> {
        let tokens_str = mac.tokens.to_string();
        let args = if tokens_str.trim().is_empty() {
            Vec::new()
        } else {
            let wrapped = format!("dummy({})", tokens_str);
            let call = match syn::parse_str::<syn::ExprCall>(&wrapped) {
                Ok(call) => call,
                Err(err) => {
                    return self.err(
                        format!("Failed to parse input! arguments: {}", err),
                        Expr::unit(),
                    );
                }
            };
            call.args
                .into_iter()
                .map(|arg| self.parse_expr(arg))
                .collect::<Result<Vec<_>>>()?
        };

        Ok(ExprIntrinsicCall::new(
            IntrinsicCallKind::Input,
            IntrinsicCallPayload::Args { args },
        )
        .into())
    }

    fn parse_expr_const_block(&self, block: syn::Block) -> Result<Expr> {
        let block = self.parse_block(block)?;
        Ok(ExprIntrinsicCall::new(
            IntrinsicCallKind::ConstBlock,
            IntrinsicCallPayload::Args {
                args: vec![Expr::block(block)],
            },
        )
        .into())
    }

    fn parse_fp_macro(&self, mac: &syn::Macro) -> Result<Expr> {
        let tokens_str = mac.tokens.to_string();
        tracing::debug!("fp! macro found with tokens: {}", tokens_str);

        if tokens_str.trim().is_empty() {
            return Ok(Expr::unit());
        }

        let parser = RustParser::new();
        parser.parse_fp_content(&tokens_str)
    }

    fn parse_io_macro_to_function_call(
        &self,
        mac: &syn::Macro,
        kind: IntrinsicCallKind,
    ) -> Result<Expr> {
        let tokens_str = mac.tokens.to_string();

        if tokens_str.trim().is_empty() {
            let println_expr = ExprIntrinsicCall::new(
                kind,
                IntrinsicCallPayload::Format {
                    template: ExprFormatString {
                        parts: vec![FormatTemplatePart::Literal(String::new())],
                        args: Vec::new(),
                        kwargs: Vec::new(),
                    },
                },
            )
            .into();
            return Ok(println_expr);
        }

        let wrapped_tokens = format!("dummy({})", tokens_str);

        match syn::parse_str::<syn::ExprCall>(&wrapped_tokens) {
            Ok(call_expr) => {
                let args: Vec<_> = call_expr
                    .args
                    .into_iter()
                    .map(|arg| self.parse_expr(arg))
                    .collect::<Result<Vec<_>>>()?;

                if !args.is_empty() {
                    if let ExprKind::Value(value) = args[0].kind() {
                        if let Value::String(format_str) = &**value {
                            let format_args = args[1..].to_vec();
                            let parts = parse_format_template(&format_str.value)?;
                            let format = ExprFormatString {
                                parts,
                                args: format_args,
                                kwargs: Vec::new(),
                            };

                            return Ok(ExprIntrinsicCall::new(
                                kind,
                                IntrinsicCallPayload::Format { template: format },
                            )
                            .into());
                        }
                    }
                }

                let fn_name = match kind {
                    IntrinsicCallKind::Println => "println",
                    IntrinsicCallKind::Print => "print",
                    _ => "println",
                };

                Ok(ExprInvoke {
                    target: ExprInvokeTarget::expr(Expr::path(fp_core::ast::Path::from(
                        Ident::new(fn_name),
                    ))),
                    args,
                }
                .into())
            }
            Err(e) => self.err(
                format!(
                    "Failed to parse println! macro arguments '{}': {}",
                    tokens_str, e
                ),
                Expr::unit(),
            ),
        }
    }

    fn parse_metaprogramming_intrinsic(
        &self,
        mac: &syn::Macro,
        kind: IntrinsicCallKind,
    ) -> Result<Expr> {
        let tokens_str = mac.tokens.to_string();

        let args = if tokens_str.trim().is_empty() {
            Vec::new()
        } else {
            let wrapped = format!("dummy({})", tokens_str);
            let call = match syn::parse_str::<syn::ExprCall>(&wrapped) {
                Ok(call) => call,
                Err(e) => {
                    return self.err(
                        format!("Failed to parse intrinsic arguments: {}", e),
                        Expr::unit(),
                    );
                }
            };
            call.args
                .into_iter()
                .map(|arg| self.parse_expr(arg))
                .collect::<Result<Vec<_>>>()?
        };

        Ok(ExprIntrinsicCall::new(kind, IntrinsicCallPayload::Args { args }).into())
    }

    fn parse_expr_const(&self, expr: syn::ExprConst) -> Result<Expr> {
        self.parse_expr_const_block(expr.block)
    }

    fn is_println_macro(mac: &syn::Macro) -> bool {
        mac.path.segments.len() == 1 && mac.path.segments[0].ident == "println"
    }

    fn is_print_macro(mac: &syn::Macro) -> bool {
        mac.path.segments.len() == 1 && mac.path.segments[0].ident == "print"
    }

    fn is_cfg_macro(mac: &syn::Macro) -> bool {
        mac.path.segments.len() == 1 && mac.path.segments[0].ident == "cfg"
    }

    fn is_fp_macro(mac: &syn::Macro) -> bool {
        mac.path.segments.len() == 1 && mac.path.segments[0].ident == "fp"
    }

    fn is_input_macro(mac: &syn::Macro) -> bool {
        mac.path.segments.len() == 1 && mac.path.segments[0].ident == "input"
    }
}

impl RustParser {
    pub(super) fn expr_parser(&self) -> ExprParser<'_> {
        ExprParser::new(self)
    }
}

fn parse_format_template(template: &str) -> Result<Vec<fp_core::ast::FormatTemplatePart>> {
    let mut parts = Vec::new();
    let mut current_literal = String::new();
    let mut chars = template.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '{' {
            if let Some(&next_ch) = chars.peek() {
                if next_ch == '}' {
                    chars.next();
                    if !current_literal.is_empty() {
                        parts.push(fp_core::ast::FormatTemplatePart::Literal(
                            current_literal.clone(),
                        ));
                        current_literal.clear();
                    }
                    parts.push(fp_core::ast::FormatTemplatePart::Placeholder(
                        fp_core::ast::FormatPlaceholder {
                            arg_ref: fp_core::ast::FormatArgRef::Implicit,
                            format_spec: None,
                        },
                    ));
                } else {
                    let mut placeholder_content = String::new();
                    chars.next();
                    while let Some(inner_ch) = chars.next() {
                        if inner_ch == '}' {
                            break;
                        }
                        placeholder_content.push(inner_ch);
                    }

                    if !current_literal.is_empty() {
                        parts.push(fp_core::ast::FormatTemplatePart::Literal(
                            current_literal.clone(),
                        ));
                        current_literal.clear();
                    }

                    let placeholder = parse_placeholder_content(&placeholder_content)?;
                    parts.push(fp_core::ast::FormatTemplatePart::Placeholder(placeholder));
                }
            } else {
                current_literal.push(ch);
            }
        } else if ch == '%' {
            if let Some(&next_ch) = chars.peek() {
                if next_ch == '%' {
                    chars.next();
                    current_literal.push('%');
                    continue;
                }
            }

            if !current_literal.is_empty() {
                parts.push(fp_core::ast::FormatTemplatePart::Literal(
                    current_literal.clone(),
                ));
                current_literal.clear();
            }

            let mut spec = String::new();
            while let Some(&next) = chars.peek() {
                spec.push(next);
                chars.next();
                if next.is_ascii_alphabetic() {
                    break;
                }
            }

            if spec.is_empty() {
                spec.push('s');
            }

            parts.push(fp_core::ast::FormatTemplatePart::Placeholder(
                fp_core::ast::FormatPlaceholder {
                    arg_ref: fp_core::ast::FormatArgRef::Implicit,
                    format_spec: Some(format!("%{}", spec)),
                },
            ));
        } else {
            current_literal.push(ch);
        }
    }

    if !current_literal.is_empty() {
        parts.push(fp_core::ast::FormatTemplatePart::Literal(current_literal));
    }

    Ok(parts)
}

fn parse_placeholder_content(content: &str) -> Result<fp_core::ast::FormatPlaceholder> {
    if content.is_empty() {
        return Ok(fp_core::ast::FormatPlaceholder {
            arg_ref: fp_core::ast::FormatArgRef::Implicit,
            format_spec: None,
        });
    }

    if let Some(colon_pos) = content.find(':') {
        let arg_part = &content[..colon_pos];
        let format_spec = &content[colon_pos + 1..];

        let arg_ref = if arg_part.is_empty() {
            fp_core::ast::FormatArgRef::Implicit
        } else if let Ok(index) = arg_part.parse::<usize>() {
            fp_core::ast::FormatArgRef::Positional(index)
        } else {
            fp_core::ast::FormatArgRef::Named(arg_part.to_string())
        };

        Ok(fp_core::ast::FormatPlaceholder {
            arg_ref,
            format_spec: Some(format_spec.to_string()),
        })
    } else {
        let arg_ref = if let Ok(index) = content.parse::<usize>() {
            fp_core::ast::FormatArgRef::Positional(index)
        } else {
            fp_core::ast::FormatArgRef::Named(content.to_string())
        };

        Ok(fp_core::ast::FormatPlaceholder {
            arg_ref,
            format_spec: None,
        })
    }
}

fn get_metaprogramming_intrinsic(mac: &syn::Macro) -> Option<IntrinsicCallKind> {
    if mac.path.segments.len() != 1 {
        return None;
    }
    let name = mac.path.segments[0].ident.to_string();
    match name.as_str() {
        "sizeof" => Some(IntrinsicCallKind::SizeOf),
        "reflect_fields" => Some(IntrinsicCallKind::ReflectFields),
        "hasmethod" => Some(IntrinsicCallKind::HasMethod),
        "type_name" => Some(IntrinsicCallKind::TypeName),
        "create_struct" => Some(IntrinsicCallKind::CreateStruct),
        "clone_struct" => Some(IntrinsicCallKind::CloneStruct),
        "addfield" => Some(IntrinsicCallKind::AddField),
        "hasfield" => Some(IntrinsicCallKind::HasField),
        "field_count" => Some(IntrinsicCallKind::FieldCount),
        "method_count" => Some(IntrinsicCallKind::MethodCount),
        "field_type" => Some(IntrinsicCallKind::FieldType),
        "struct_size" => Some(IntrinsicCallKind::StructSize),
        "generate_method" => Some(IntrinsicCallKind::GenerateMethod),
        "compile_error" => Some(IntrinsicCallKind::CompileError),
        "compile_warning" => Some(IntrinsicCallKind::CompileWarning),
        _ => None,
    }
}
