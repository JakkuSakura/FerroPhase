use super::RustParser;

use crate::{parser, RawExpr, RawExprMacro};
use fp_core::ast::Ident;
use fp_core::ast::*;
use fp_core::diagnostics::DiagnosticLevel;
use fp_core::error::Result;
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::ops::{BinOpKind, UnOpKind};
use itertools::Itertools;
use proc_macro2::{Span, TokenStream, TokenTree};
use quote::{quote, ToTokens};
use std::sync::atomic::{AtomicUsize, Ordering};
use syn::parse::{Parse, ParseStream, Parser};
use syn::parse_quote;
use syn::spanned::Spanned;
use syn::punctuated::Punctuated;
use syn::LitStr;
use syn::Token;

mod assert;
mod logging;

static FOR_LOOP_COUNTER: AtomicUsize = AtomicUsize::new(0);
static MATCH_EXPR_COUNTER: AtomicUsize = AtomicUsize::new(0);
static ASSERT_COUNTER: AtomicUsize = AtomicUsize::new(0);

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
            // Record macro invocation as AST; lowering happens in normalization stage.
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
            syn::Expr::Match(m) => self.parse_expr(self.desugar_match(m)?)?,
            syn::Expr::Field(f) => self.parse_expr_field(f)?.into(),
            syn::Expr::Try(t) => self.parse_expr_try(t)?.into(),
            syn::Expr::While(w) => self.parse_expr_while(w)?.into(),
            syn::Expr::Let(l) => self.parse_expr_let(l)?.into(),
            syn::Expr::Closure(c) => self.parse_expr_closure(c)?.into(),
            syn::Expr::Array(a) => self.parse_expr_array(a)?.into(),
            syn::Expr::Repeat(r) => self.parse_expr_repeat(r)?.into(),
            syn::Expr::Async(a) => self.parse_expr_async(a)?,
            syn::Expr::Assign(a) => self.parse_expr_assign(a)?.into(),
            syn::Expr::Cast(c) => self.parse_expr_cast(c)?.into(),
            syn::Expr::Await(a) => self.parse_expr_await(a)?.into(),
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
                return self.parse_expr(self.desugar_general_for_loop(f)?);
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

    fn desugar_general_for_loop(&self, f: syn::ExprForLoop) -> Result<syn::Expr> {
        let iter_expr = *f.expr;
        let pat = *f.pat;
        let body = f.body;
        let idx = FOR_LOOP_COUNTER.fetch_add(1, Ordering::Relaxed);
        let iter_ident = syn::Ident::new(&format!("__fp_for_iter_{idx}"), Span::call_site());

        let loop_expr: syn::Expr = if let Some(label) = f.label {
            let loop_label = label.name;
            let break_label = loop_label.clone();
            syn::parse_quote!({
                let mut #iter_ident = (#iter_expr).into_iter();
                #loop_label: loop {
                    if let Some(#pat) = #iter_ident.next() {
                        #body
                    } else {
                        break #break_label;
                    }
                }
            })
        } else {
            syn::parse_quote!({
                let mut #iter_ident = (#iter_expr).into_iter();
                loop {
                    if let Some(#pat) = #iter_ident.next() {
                        #body
                    } else {
                        break;
                    }
                }
            })
        };

        Ok(loop_expr)
    }

    fn desugar_match(&self, m: syn::ExprMatch) -> Result<syn::Expr> {
        let scrutinee = *m.expr;
        let idx = MATCH_EXPR_COUNTER.fetch_add(1, Ordering::Relaxed);
        let value_ident = syn::Ident::new(&format!("__fp_match_value_{idx}"), Span::call_site());
        let result_ident = syn::Ident::new(&format!("__fp_match_result_{idx}"), Span::call_site());

        let mut cases = Vec::new();
        let mut default_case = None;

        for arm in m.arms {
            let pat = arm.pat;
            let guard = arm.guard.map(|(_, expr)| *expr);
            let body = *arm.body;

            let is_wildcard = matches!(pat, syn::Pat::Wild(_));
            if is_wildcard && guard.is_none() {
                default_case = Some(quote! {
                    break { #body };
                });
                continue;
            }

            let guard_tokens = guard.map(|g| quote! { if #g { break { #body }; }});

            let case_tokens = if let Some(guard_block) = guard_tokens {
                quote! {
                    if let #pat = &#value_ident {
                        #guard_block
                    }
                }
            } else {
                quote! {
                    if let #pat = &#value_ident {
                        break { #body };
                    }
                }
            };

            cases.push(case_tokens);
        }

        let default_tokens = default_case.unwrap_or_else(|| {
            quote! {
                break { { ::std::process::abort() } };
            }
        });

        let desugared: syn::Expr = syn::parse_quote!({
            let #value_ident = (#scrutinee);
            let #result_ident = loop {
                #(#cases)*
                #default_tokens
            };
            #result_ident
        });

        Ok(desugared)
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

    fn parse_expr_await(&self, a: syn::ExprAwait) -> Result<Expr> {
        let base = self.parse_expr(*a.base)?;
        Ok(ExprKind::Await(ExprAwait { base: base.into() }).into())
    }

    fn parse_expr_async(&self, expr: syn::ExprAsync) -> Result<Expr> {
        // Parse `async { .. }` into an AST-level ExprAsync node so that
        // later stages can decide how to lower async semantics.
        let block = self.parse_block(expr.block)?;
        let inner = Expr::block(block);
        Ok(ExprKind::Async(ExprAsync { expr: inner.into() }).into())
    }

    fn unsupported_expr_macro(&self, mac: &syn::ExprMacro) -> Result<Expr> {
        let name = mac.mac.path.to_token_stream().to_string();
        let message = format!("Unsupported expression macro `{}`", name);
        self.warn_or_error_expr(message, Expr::unit())
    }

    fn warn_or_error_expr(&self, message: impl Into<String>, fallback: Expr) -> Result<Expr> {
        if self.parser.lossy_mode() {
            self.parser
                .record_diagnostic(DiagnosticLevel::Warning, message.into());
            Ok(fallback)
        } else {
            self.parser.error(message, fallback)
        }
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
            syn::BinOp::Shl(_) => BinOpKind::Shl,
            syn::BinOp::Shr(_) => BinOpKind::Shr,
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
            update: None,
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

    fn parse_expr_cast(&self, c: syn::ExprCast) -> Result<ExprCast> {
        Ok(ExprCast {
            expr: self.parse_expr(*c.expr)?.into(),
            ty: self.parser.parse_type(*c.ty)?,
        })
    }

    fn parse_expr_break(&self, b: syn::ExprBreak) -> Result<Expr> {
        let value = b.expr.map(|e| self.parse_expr(*e)).transpose()?;
        Ok(ExprKind::Break(ExprBreak {
            value: value.map(Box::new),
        })
        .into())
    }

    fn parse_expr_continue(&self) -> Result<Expr> {
        Ok(ExprKind::Continue(ExprContinue {}).into())
    }

    fn parse_expr_return(&self, r: syn::ExprReturn) -> Result<Expr> {
        let value = r.expr.map(|e| self.parse_expr(*e)).transpose()?;
        Ok(ExprKind::Return(ExprReturn {
            value: value.map(Box::new),
        })
        .into())
    }

    fn parse_expr_macro(&self, m: syn::ExprMacro) -> Result<Expr> {
        match self.build_macro_invocation(&m.mac) {
            Ok(invocation) => Ok(Expr::macro_invocation(invocation)),
            Err(err) => self.err(
                format!(
                    "failed to record macro invocation `{}`: {}",
                    m.mac.path.to_token_stream(),
                    err
                ),
                Expr::any(RawExprMacro { raw: m }),
            ),
        }
    }

    fn build_macro_invocation(&self, mac: &syn::Macro) -> Result<MacroInvocation> {
        let path = parser::parse_path(mac.path.clone())?;
        let delimiter = match &mac.delimiter {
            syn::MacroDelimiter::Paren(_) => MacroDelimiter::Parenthesis,
            syn::MacroDelimiter::Brace(_) => MacroDelimiter::Brace,
            syn::MacroDelimiter::Bracket(_) => MacroDelimiter::Bracket,
        };
        let mut invocation = MacroInvocation::new(path, delimiter, mac.tokens.to_string());
        if let Some(span) = self.parser.span_for_proc_macro(mac.span()) {
            invocation = invocation.with_span(span);
        }
        Ok(invocation)
    }

    pub(crate) fn lower_expr_macro(&self, m: syn::ExprMacro) -> Result<Expr> {
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

        if Self::is_vec_macro(&m.mac) {
            return self.parse_vec_macro(&m);
        }

        if Self::is_matches_macro(&m.mac) {
            return self.parse_matches_macro(&m);
        }

        if Self::is_format_macro(&m.mac) {
            return self.parse_format_macro(&m);
        }

        if let Some(level) = Self::log_macro_level(&m.mac) {
            return self.parse_logging_macro(&m.mac, level);
        }

        if Self::is_panic_macro(&m.mac)
            || Self::is_unreachable_macro(&m.mac)
            || Self::is_unimplemented_macro(&m.mac)
            || Self::is_todo_macro(&m.mac)
        {
            return self.parse_panic_like_macro(&m);
        }

        if Self::is_assert_macro(&m.mac) {
            return self.parse_assert_macro(&m.mac, AssertKind::Assert);
        }

        if Self::is_debug_assert_macro(&m.mac) {
            return self.parse_assert_macro(&m.mac, AssertKind::DebugAssert);
        }

        if Self::is_assert_eq_macro(&m.mac) {
            return self.parse_assert_macro(&m.mac, AssertKind::AssertEq);
        }

        if Self::is_assert_ne_macro(&m.mac) {
            return self.parse_assert_macro(&m.mac, AssertKind::AssertNe);
        }

        if Self::is_debug_assert_eq_macro(&m.mac) {
            return self.parse_assert_macro(&m.mac, AssertKind::DebugAssertEq);
        }

        if Self::is_debug_assert_ne_macro(&m.mac) {
            return self.parse_assert_macro(&m.mac, AssertKind::DebugAssertNe);
        }

        if Self::is_env_macro(&m.mac) {
            return self.parse_env_macro(&m.mac, false);
        }

        if Self::is_option_env_macro(&m.mac) {
            return self.parse_env_macro(&m.mac, true);
        }

        if let Some(intrinsic_kind) = get_metaprogramming_intrinsic(&m.mac) {
            return self.parse_metaprogramming_intrinsic(&m.mac, intrinsic_kind);
        }

        self.unsupported_expr_macro(&m)
    }

    fn parse_vec_macro(&self, m: &syn::ExprMacro) -> Result<Expr> {
        let args = match syn::parse2::<VecMacroKind>(m.mac.tokens.clone()) {
            Ok(args) => args,
            Err(err) => {
                return self.err(
                    format!("failed to parse vec! macro: {}", err),
                    Expr::any(RawExprMacro { raw: m.clone() }),
                );
            }
        };

        match args {
            VecMacroKind::Elements(elements) => self.make_vec_from_elements(elements),
            VecMacroKind::Repeat { elem, len } => self.make_vec_from_repeat(elem, len),
        }
    }

    fn parse_matches_macro(&self, m: &syn::ExprMacro) -> Result<Expr> {
        let args = match syn::parse2::<MatchesMacroArgs>(m.mac.tokens.clone()) {
            Ok(args) => args,
            Err(err) => {
                return self.err(
                    format!("failed to parse matches! macro: {}", err),
                    Expr::unit(),
                );
            }
        };

        let scrutinee = args.expr;
        let pat = args.pat;
        let guard = args.guard;

        let desugared: syn::Expr = if let Some(guard_expr) = guard {
            parse_quote!({
                match #scrutinee {
                    #pat if #guard_expr => true,
                    _ => false,
                }
            })
        } else {
            parse_quote!({
                match #scrutinee {
                    #pat => true,
                    _ => false,
                }
            })
        };

        self.parse_expr(desugared)
    }

    fn parse_format_macro(&self, m: &syn::ExprMacro) -> Result<Expr> {
        let tokens_str = m.mac.tokens.to_string();
        let wrapped_tokens = format!("format({})", tokens_str);

        let call_expr = match syn::parse_str::<syn::ExprCall>(&wrapped_tokens) {
            Ok(expr) => expr,
            Err(err) => {
                return self.err(
                    format!(
                        "Failed to parse format! macro arguments '{}': {}",
                        tokens_str, err
                    ),
                    Expr::unit(),
                );
            }
        };

        let args: Vec<_> = call_expr
            .args
            .into_iter()
            .map(|arg| self.parse_expr(arg))
            .collect::<Result<Vec<_>>>()?;

        if !args.is_empty() {
            if let ExprKind::Value(value) = args[0].kind() {
                if let Value::String(format_str) = &**value {
                    let parts = parse_format_template(&format_str.value)?;
                    let format = ExprStringTemplate { parts };
                    let mut args_out = Vec::with_capacity(args.len());
                    args_out.push(Expr::new(ExprKind::FormatString(format)));
                    args_out.extend(args[1..].iter().cloned());
                    return Ok(ExprIntrinsicCall::new(
                        IntrinsicCallKind::Format,
                        args_out,
                        Vec::new(),
                    )
                    .into());
                }
            }
        }

        Ok(ExprIntrinsicCall::new(IntrinsicCallKind::Format, args, Vec::new()).into())
    }

    // moved to parser/expr_logging.rs

    fn make_logging_literal(&self, level: LogLevel, message: Option<String>) -> Expr {
        let mut literal = String::new();
        literal.push_str(level.prefix());
        match message {
            Some(msg) => {
                let trimmed = msg.trim();
                if trimmed.is_empty() {
                    literal.push_str(level.default_message());
                } else {
                    literal.push_str(trimmed);
                }
            }
            None => literal.push_str(level.default_message()),
        }

        ExprIntrinsicCall::new(
            IntrinsicCallKind::Println,
            vec![Expr::new(ExprKind::FormatString(ExprStringTemplate {
                parts: vec![FormatTemplatePart::Literal(literal)],
            }))],
            Vec::new(),
        )
        .into()
    }

    fn parse_stmt_macro(&self, raw: syn::StmtMacro) -> Result<BlockStmt> {
        let expr_macro = syn::ExprMacro {
            attrs: raw.attrs.clone(),
            mac: raw.mac.clone(),
        };
        // Record macro invocation as AST; lowering happens later in normalization stage.
        let expr = self.parse_expr_macro(expr_macro)?;
        Ok(BlockStmt::Expr(
            BlockStmtExpr::new(expr).with_semicolon(raw.semi_token.is_some()),
        ))
    }

    fn parse_panic_like_macro(&self, m: &syn::ExprMacro) -> Result<Expr> {
        let args = parse_expr_arguments(m.mac.tokens.clone())?;
        let macro_name = m
            .mac
            .path
            .segments
            .last()
            .map(|segment| segment.ident.to_string())
            .unwrap_or_else(|| "panic".to_owned());

        let default_message = match macro_name.as_str() {
            "panic" => "panic! macro triggered",
            "panic_any" => "panic_any! macro triggered",
            "unreachable" => "entered unreachable! macro branch",
            "unimplemented" => "unimplemented! macro invoked",
            "todo" => "todo! macro invoked",
            _ => "panic macro triggered",
        };

        let stmts = self.make_failure_stmts(args, Vec::new(), default_message)?;
        Ok(Expr::block(ExprBlock::new_stmts(stmts)))
    }

    // Parser no longer performs in-file assert macro lowering. Macro lowering is handled during normalization.

    pub(super) fn make_failure_stmts(
        &self,
        message_args: Vec<syn::Expr>,
        default_print_args: Vec<syn::Expr>,
        default_message: &str,
    ) -> Result<Vec<BlockStmt>> {
        let mut default_print_args = default_print_args;
        if default_print_args.is_empty() {
            let lit = LitStr::new(default_message, Span::call_site());
            default_print_args.push(syn::parse_quote!(#lit));
        }

        let mut stmts = Vec::new();
        let use_user_format = matches!(
            message_args.first(),
            Some(syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(_),
                ..
            }))
        );

        let print_args = if message_args.is_empty() {
            default_print_args
        } else if use_user_format {
            message_args
        } else {
            for expr in message_args {
                let parsed = self.parse_expr(expr)?;
                stmts.push(BlockStmt::Expr(
                    BlockStmtExpr::new(parsed).with_semicolon(true),
                ));
            }
            default_print_args
        };

        let println_syn: syn::Expr = syn::parse_quote!(println!(#(#print_args),*));
        let println_expr = self.parse_expr(println_syn)?;
        stmts.push(BlockStmt::Expr(
            BlockStmtExpr::new(println_expr).with_semicolon(true),
        ));
        stmts.push(BlockStmt::Expr(
            BlockStmtExpr::new(make_abort_expr()).with_semicolon(true),
        ));
        Ok(stmts)
    }

    fn parse_env_macro(&self, mac: &syn::Macro, optional: bool) -> Result<Expr> {
        let args: EnvMacroArgs = match syn::parse2(mac.tokens.clone()) {
            Ok(parsed) => parsed,
            Err(err) => {
                return self.err(
                    format!("Failed to parse env! macro arguments: {}", err),
                    Expr::unit(),
                )
            }
        };

        let key = args.key.value();
        let mut value = std::env::var(&key).ok();
        if value.is_none() {
            if let Some(default_expr) = args.default.as_ref() {
                if let Some(default) = extract_string_literal(default_expr) {
                    value = Some(default);
                }
            }
        }

        if optional {
            let option = ValueOption::new(value.map(Value::string));
            Ok(Expr::value(Value::Option(option)))
        } else {
            let resolved = value.unwrap_or_default();
            Ok(Expr::value(Value::string(resolved)))
        }
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

    pub(super) fn debug_assertions_intrinsic(&self) -> Expr {
        ExprIntrinsicCall::new(IntrinsicCallKind::DebugAssertions, Vec::new(), Vec::new()).into()
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

        Ok(ExprIntrinsicCall::new(IntrinsicCallKind::Input, args, Vec::new()).into())
    }

    fn parse_expr_const_block(&self, block: syn::Block) -> Result<Expr> {
        let block = self.parse_block(block)?;
        Ok(ExprKind::ConstBlock(ExprConstBlock {
            expr: Expr::block(block).into(),
        })
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
                vec![Expr::new(ExprKind::FormatString(ExprStringTemplate {
                    parts: vec![FormatTemplatePart::Literal(String::new())],
                }))],
                Vec::new(),
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
                            let format = ExprStringTemplate { parts };
                            let mut args_out = Vec::with_capacity(1 + format_args.len());
                            args_out.push(Expr::new(ExprKind::FormatString(format)));
                            args_out.extend(format_args);

                            return Ok(ExprIntrinsicCall::new(kind, args_out, Vec::new()).into());
                        }
                    }
                }

                Ok(ExprIntrinsicCall::new(kind, args, Vec::new()).into())
            }
            Err(e) => self.err(
                format!(
                    "Failed to parse {}! macro arguments '{}': {}",
                    match kind {
                        IntrinsicCallKind::Println => "println",
                        IntrinsicCallKind::Print => "print",
                        _ => "print",
                    },
                    tokens_str,
                    e
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
        let args = self.parse_intrinsic_args(mac)?;

        Ok(ExprIntrinsicCall::new(kind, args, Vec::new()).into())
    }

    fn parse_intrinsic_args(&self, mac: &syn::Macro) -> Result<Vec<Expr>> {
        if mac.tokens.is_empty() {
            return Ok(Vec::new());
        }

        let mut args = Vec::new();
        for tokens in split_macro_args(mac.tokens.clone()) {
            if tokens.is_empty() {
                return self.err(
                    "Failed to parse intrinsic arguments: empty argument",
                    Vec::new(),
                );
            }

            match syn::parse2::<syn::Expr>(tokens.clone()) {
                Ok(expr) => args.push(self.parse_expr(expr)?),
                Err(_) => {
                    // Allow type arguments (e.g., &'static str) in metaprogramming intrinsics.
                    if let Some(expr) = self.try_parse_quote_expr(tokens.clone()) {
                        args.push(expr);
                        continue;
                    }
                    let ty = match syn::parse2::<syn::Type>(tokens.clone()) {
                        Ok(ty) => ty,
                        Err(err) => {
                            return self.err(
                                format!(
                                    "Failed to parse intrinsic argument as expr or type: {}",
                                    err
                                ),
                                Vec::new(),
                            );
                        }
                    };
                    let ast_ty = self.parser.parse_type(ty)?;
                    args.push(Expr::value(Value::Type(ast_ty)));
                }
            }
        }

        Ok(args)
    }

    fn try_parse_quote_expr(&self, tokens: TokenStream) -> Option<Expr> {
        let mut iter = tokens.into_iter().peekable();
        let TokenTree::Ident(ident) = iter.next()? else {
            return None;
        };
        if ident.to_string() != "quote" {
            return None;
        }

        let mut kind: Option<QuoteFragmentKind> = None;
        if let Some(TokenTree::Punct(punct)) = iter.peek() {
            if punct.as_char() == '<' {
                iter.next();
                let TokenTree::Ident(kind_ident) = iter.next()? else {
                    return None;
                };
                let kind_name = kind_ident.to_string();
                kind = match kind_name.as_str() {
                    "expr" => Some(QuoteFragmentKind::Expr),
                    "stmt" => Some(QuoteFragmentKind::Stmt),
                    "item" => Some(QuoteFragmentKind::Item),
                    "type" => Some(QuoteFragmentKind::Type),
                    "items" | "fns" | "structs" | "enums" | "traits" | "impls" | "consts"
                    | "statics" | "mods" | "uses" | "macros" | "exprs" | "stmts" | "types" => {
                        tracing::warn!("deprecated plural quote fragment kind: {}", kind_name);
                        Some(QuoteFragmentKind::Item)
                    }
                    _ => return None,
                };
                let TokenTree::Punct(close) = iter.next()? else {
                    return None;
                };
                if close.as_char() != '>' {
                    return None;
                }
            }
        }

        let TokenTree::Group(group) = iter.next()? else {
            return None;
        };
        if !matches!(group.delimiter(), proc_macro2::Delimiter::Brace) {
            return None;
        }
        if iter.next().is_some() {
            return None;
        }

        let block_tokens = TokenStream::from(TokenTree::Group(group));
        let syn_block = syn::parse2::<syn::Block>(block_tokens).ok()?;
        let ast_block = self.parser.parse_block(syn_block).ok()?;
        Some(Expr::from(ExprKind::Quote(ExprQuote {
            block: ast_block,
            kind,
        })))
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

    fn macro_matches(mac: &syn::Macro, names: &[&str]) -> bool {
        mac.path
            .segments
            .last()
            .map(|segment| names.iter().any(|name| segment.ident == *name))
            .unwrap_or(false)
    }

    fn is_vec_macro(mac: &syn::Macro) -> bool {
        Self::macro_matches(mac, &["vec"])
    }

    fn is_matches_macro(mac: &syn::Macro) -> bool {
        Self::macro_matches(mac, &["matches"])
    }

    fn is_format_macro(mac: &syn::Macro) -> bool {
        Self::macro_matches(mac, &["format"])
    }

    fn is_panic_macro(mac: &syn::Macro) -> bool {
        Self::macro_matches(mac, &["panic", "panic_any"])
    }

    fn is_unreachable_macro(mac: &syn::Macro) -> bool {
        Self::macro_matches(mac, &["unreachable"])
    }

    fn is_unimplemented_macro(mac: &syn::Macro) -> bool {
        Self::macro_matches(mac, &["unimplemented"])
    }

    fn is_todo_macro(mac: &syn::Macro) -> bool {
        Self::macro_matches(mac, &["todo"])
    }

    fn is_assert_macro(mac: &syn::Macro) -> bool {
        Self::macro_matches(mac, &["assert"])
    }

    fn is_debug_assert_macro(mac: &syn::Macro) -> bool {
        Self::macro_matches(mac, &["debug_assert"])
    }

    fn is_assert_eq_macro(mac: &syn::Macro) -> bool {
        Self::macro_matches(mac, &["assert_eq"])
    }

    fn is_assert_ne_macro(mac: &syn::Macro) -> bool {
        Self::macro_matches(mac, &["assert_ne"])
    }

    fn is_debug_assert_eq_macro(mac: &syn::Macro) -> bool {
        Self::macro_matches(mac, &["debug_assert_eq"])
    }

    fn is_debug_assert_ne_macro(mac: &syn::Macro) -> bool {
        Self::macro_matches(mac, &["debug_assert_ne"])
    }

    fn is_env_macro(mac: &syn::Macro) -> bool {
        Self::macro_matches(mac, &["env"])
    }

    fn is_option_env_macro(mac: &syn::Macro) -> bool {
        Self::macro_matches(mac, &["option_env"])
    }

    fn log_macro_level(mac: &syn::Macro) -> Option<LogLevel> {
        mac.path
            .segments
            .last()
            .and_then(|segment| match segment.ident.to_string().as_str() {
                "trace" => Some(LogLevel::Trace),
                "debug" => Some(LogLevel::Debug),
                "info" => Some(LogLevel::Info),
                "warn" => Some(LogLevel::Warn),
                "error" => Some(LogLevel::Error),
                _ => None,
            })
    }
}

fn split_macro_args(tokens: TokenStream) -> Vec<TokenStream> {
    let mut args = Vec::new();
    let mut current = TokenStream::new();

    for token in tokens {
        if let TokenTree::Punct(punct) = &token {
            if punct.as_char() == ',' {
                if !current.is_empty() {
                    args.push(current);
                    current = TokenStream::new();
                }
                continue;
            }
        }
        current.extend(std::iter::once(token));
    }

    if !current.is_empty() {
        args.push(current);
    }

    args
}

impl RustParser {
    pub(super) fn expr_parser(&self) -> ExprParser<'_> {
        ExprParser::new(self)
    }
}

impl<'a> ExprParser<'a> {
    fn make_vec_from_elements(
        &self,
        elements: Punctuated<syn::Expr, syn::Token![,]>,
    ) -> Result<Expr> {
        let parsed_elements = elements
            .into_iter()
            .map(|element| self.parse_expr(element))
            .collect::<Result<Vec<_>>>()?;
        Ok(
            ExprKind::IntrinsicContainer(ExprIntrinsicContainer::VecElements {
                elements: parsed_elements,
            })
            .into(),
        )
    }

    fn make_vec_from_repeat(&self, elem: syn::Expr, len: syn::Expr) -> Result<Expr> {
        let elem_expr = self.parse_expr(elem)?;
        let len_expr = self.parse_expr(len)?;
        Ok(
            ExprKind::IntrinsicContainer(ExprIntrinsicContainer::VecRepeat {
                elem: elem_expr.into(),
                len: len_expr.into(),
            })
            .into(),
        )
    }
}

enum VecMacroKind {
    Elements(Punctuated<syn::Expr, syn::Token![,]>),
    Repeat { elem: syn::Expr, len: syn::Expr },
}

enum AssertKind {
    Assert,
    DebugAssert,
    AssertEq,
    DebugAssertEq,
    AssertNe,
    DebugAssertNe,
}

impl AssertKind {
    fn is_debug(&self) -> bool {
        matches!(
            self,
            AssertKind::DebugAssert | AssertKind::DebugAssertEq | AssertKind::DebugAssertNe
        )
    }
}

#[derive(Copy, Clone)]
enum LogLevel {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

impl LogLevel {
    fn prefix(&self) -> &'static str {
        match self {
            LogLevel::Trace => "[TRACE] ",
            LogLevel::Debug => "[DEBUG] ",
            LogLevel::Info => "[INFO] ",
            LogLevel::Warn => "[WARN] ",
            LogLevel::Error => "[ERROR] ",
        }
    }

    fn default_message(&self) -> &'static str {
        match self {
            LogLevel::Trace => "trace event",
            LogLevel::Debug => "debug event",
            LogLevel::Info => "info event",
            LogLevel::Warn => "warning event",
            LogLevel::Error => "error event",
        }
    }
}

struct EnvMacroArgs {
    key: syn::LitStr,
    default: Option<syn::Expr>,
}

impl Parse for EnvMacroArgs {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let key: syn::LitStr = input.parse()?;
        let mut default = None;
        if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
            if !input.is_empty() {
                default = Some(input.parse::<syn::Expr>()?);
            }
        }
        if input.peek(Token![,]) {
            let _ = input.parse::<Token![,]>();
        }
        Ok(Self { key, default })
    }
}

struct MatchesMacroArgs {
    expr: syn::Expr,
    pat: syn::Pat,
    guard: Option<syn::Expr>,
}

impl Parse for MatchesMacroArgs {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let expr: syn::Expr = input.parse()?;
        input.parse::<Token![,]>()?;
        let pat = syn::Pat::parse_single(input)?;

        let guard = if input.peek(Token![if]) {
            input.parse::<Token![if]>()?;
            Some(input.parse::<syn::Expr>()?)
        } else {
            None
        };

        if input.peek(Token![,]) {
            let _ = input.parse::<Token![,]>();
        }

        Ok(Self { expr, pat, guard })
    }
}

fn parse_expr_arguments(tokens: proc_macro2::TokenStream) -> Result<Vec<syn::Expr>> {
    if tokens.is_empty() {
        return Ok(Vec::new());
    }

    Punctuated::<syn::Expr, Token![,]>::parse_terminated
        .parse2(tokens)
        .map(|punct| punct.into_iter().collect())
        .map_err(|err| err.to_string().into())
}

fn extract_string_literal(expr: &syn::Expr) -> Option<String> {
    match expr {
        syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Str(lit),
            ..
        }) => Some(lit.value()),
        _ => None,
    }
}

pub(super) fn make_abort_expr() -> Expr {
    let path = Path::new(vec![
        Ident::new("std"),
        Ident::new("process"),
        Ident::new("abort"),
    ]);

    ExprInvoke {
        target: ExprInvokeTarget::expr(Expr::path(path)),
        args: Vec::new(),
    }
    .into()
}

impl Parse for VecMacroKind {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.is_empty() {
            return Ok(VecMacroKind::Elements(Punctuated::new()));
        }

        let fork = input.fork();
        let _: syn::Expr = fork.parse()?;
        if fork.peek(syn::Token![;]) {
            let elem: syn::Expr = input.parse()?;
            input.parse::<syn::Token![;]>()?;
            let len: syn::Expr = input.parse()?;
            if !input.is_empty() {
                return Err(input.error("unexpected tokens after vec! repeat form"));
            }
            return Ok(VecMacroKind::Repeat { elem, len });
        }

        let elements = Punctuated::<syn::Expr, syn::Token![,]>::parse_terminated(input)?;
        Ok(VecMacroKind::Elements(elements))
    }
}

fn parse_format_template(template: &str) -> Result<Vec<fp_core::ast::FormatTemplatePart>> {
    let mut parts = Vec::new();
    let mut current_literal = String::new();
    let mut chars = template.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '{' {
            if let Some(&next_ch) = chars.peek() {
                if next_ch == '{' {
                    chars.next();
                    current_literal.push('{');
                    continue;
                }
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
        } else if ch == '}' {
            if let Some(&next_ch) = chars.peek() {
                if next_ch == '}' {
                    chars.next();
                    current_literal.push('}');
                    continue;
                }
            }
            current_literal.push('}');
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
                    format_spec: Some(FormatSpec::parse(&format!("%{}", spec)).map_err(|err| {
                        fp_core::error::Error::from(format!(
                            "invalid format spec (%{}): {err}",
                            spec
                        ))
                    })?),
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
            format_spec: Some(FormatSpec::parse(format_spec).map_err(|err| {
                fp_core::error::Error::from(format!("invalid format spec: {err}"))
            })?),
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
