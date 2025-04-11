use eyre::bail;
use eyre::Result;
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use lang_core::ast::{
    AstExpr, BlockStmt, ExprArray, ExprAssign, ExprBinOp, ExprBlock, ExprClosure, ExprField,
    ExprIf, ExprIndex, ExprInvoke, ExprInvokeTarget, ExprLet, ExprLoop, ExprMatch, ExprParen,
    ExprRange, ExprRangeLimit, ExprReference, ExprSelect, ExprSelectType, ExprStruct, ExprTuple,
    ExprUnOp, ExprWhile, StmtLet,
};
use lang_core::ops::{BinOpKind, UnOpKind};

use crate::printer::RustPrinter;

impl RustPrinter {
    pub fn print_expr_no_braces(&self, node: &AstExpr) -> Result<TokenStream> {
        match node {
            AstExpr::Block(n) => self.print_block_no_braces(&n),
            AstExpr::Value(v) if v.is_unit() => Ok(quote!()),
            _ => self.print_expr(node),
        }
    }
    pub fn print_expr_id(&self, id: u64) -> Result<TokenStream> {
        let ident = format_ident!("_expr_{}", id);
        Ok(quote!(#ident))
    }
    pub fn print_expr(&self, node: &AstExpr) -> Result<TokenStream> {
        match node {
            AstExpr::Id(id) => self.print_expr_id(*id),
            AstExpr::Locator(loc) => self.print_locator(loc),
            AstExpr::Value(n) => self.print_value(n),
            AstExpr::Invoke(n) => self.print_invoke_expr(n),
            AstExpr::UnOp(op) => self.print_un_op(op),
            AstExpr::BinOp(op) => self.print_bin_op(op),
            AstExpr::Any(n) => self.print_any(n),
            AstExpr::Match(n) => self.print_match(n),
            AstExpr::If(n) => self.print_if(n),
            AstExpr::Block(n) => self.print_block(n),
            AstExpr::Struct(n) => self.print_struct_expr(n),
            AstExpr::Select(n) => self.print_select(n),
            AstExpr::Reference(n) => self.print_ref(n),
            AstExpr::Assign(n) => self.print_assign(n),
            AstExpr::Index(n) => self.print_index(n),
            AstExpr::Closured(n) => self.print_expr(&n.expr),
            AstExpr::Paren(n) => self.print_paren(n),
            AstExpr::Loop(n) => self.print_loop(n),
            AstExpr::Range(n) => self.print_range(n),
            AstExpr::Tuple(n) => self.print_expr_tuple(n),
            AstExpr::Try(n) => self.print_expr_try(&n.expr),
            AstExpr::While(n) => self.print_while(n),
            AstExpr::Let(n) => self.print_expr_let(n),
            AstExpr::Closure(n) => self.print_expr_closure(n),
            AstExpr::Array(n) => self.print_expr_array(n),

            _ => bail!("Unable to serialize {:?}", node),
        }
    }

    fn print_bin_op(&self, binop: &ExprBinOp) -> Result<TokenStream> {
        let lhs = self.print_expr(&binop.lhs.get())?;
        let rhs = self.print_expr(&binop.rhs.get())?;
        let op = self.print_bin_op_kind(&binop.kind);
        Ok(quote!(#lhs #op #rhs))
    }

    fn print_invoke_expr(&self, invoke: &ExprInvoke) -> Result<TokenStream> {
        let fun = self.print_invoke_target(&invoke.target)?;
        let args: Vec<_> = invoke
            .args
            .iter()
            .map(|x| self.print_expr(&x.get()))
            .try_collect()?;
        Ok(quote!(
            #fun(#(#args), *)
        ))
    }
    fn print_expr_let(&self, let_: &ExprLet) -> Result<TokenStream> {
        let pat = self.print_pattern(&let_.pat)?;
        let init = self.print_expr(&let_.expr)?;
        Ok(quote!(
            let #pat = #init
        ))
    }
    pub fn print_stmt_let(&self, let_: &StmtLet) -> Result<TokenStream> {
        let pat = self.print_pattern(&let_.pat)?;

        if let Some(init) = &let_.init {
            let init = self.print_expr(&init)?;
            let elze = if let Some(elze) = &let_.diverge {
                let elze = self.print_expr(elze)?;
                quote!(else #elze)
            } else {
                quote!()
            };
            Ok(quote!(
                let #pat = #init #elze;
            ))
        } else {
            Ok(quote!(
                let #pat;
            ))
        }
    }
    pub fn print_assign(&self, assign: &ExprAssign) -> Result<TokenStream> {
        let target = self.print_expr(&assign.target)?;
        let value = self.print_expr(&assign.value)?;
        Ok(quote!(
            #target = #value;
        ))
    }
    pub fn print_index(&self, index: &ExprIndex) -> Result<TokenStream> {
        let expr = self.print_expr(&index.obj.get())?;
        let index = self.print_expr(&index.index.get())?;
        Ok(quote!(
            #expr[#index]
        ))
    }
    pub fn print_paren(&self, paren: &ExprParen) -> Result<TokenStream> {
        let expr = self.print_expr(&paren.expr.get())?;
        Ok(quote!(
            (#expr)
        ))
    }

    pub fn print_loop(&self, loop_: &ExprLoop) -> Result<TokenStream> {
        let body = self.print_expr_no_braces(&loop_.body)?;
        Ok(quote!(
            loop {
                #body
            }
        ))
    }
    // pub fn print_for_each(&self, for_each: &ExprForEach) -> Result<TokenStream> {
    //     let name = self.print_ident(&for_each.variable);
    //     let iter = self.print_expr(&for_each.iterable)?;
    //     let body = self.print_block(&for_each.body)?;
    //     Ok(quote!(
    //         for #name in #iter
    //             #body
    //     ))
    // }
    fn print_while(&self, while_: &ExprWhile) -> Result<TokenStream> {
        let cond = self.print_expr(&while_.cond)?;
        let body = self.print_expr_no_braces(&while_.body)?;
        Ok(quote!(
            while #cond {
                #body
            }
        ))
    }
    pub fn print_statement(&self, stmt: &BlockStmt) -> Result<TokenStream> {
        match stmt {
            BlockStmt::Item(item) => self.print_item(item),
            BlockStmt::Let(let_) => self.print_stmt_let(let_),
            BlockStmt::Expr(expr0) => {
                let expr = self.print_expr(&expr0.expr)?;
                let with_semicolon;

                if expr0.semicolon == Some(true) {
                    with_semicolon = true;
                } else if expr0.semicolon == Some(false) {
                    with_semicolon = false;
                } else {
                    match &*expr0.expr {
                        AstExpr::Block(_) | AstExpr::If(_) => with_semicolon = false,
                        _ => with_semicolon = true,
                    }
                }
                if with_semicolon {
                    Ok(quote!(#expr;))
                } else {
                    Ok(quote!(#expr))
                }
            }
            BlockStmt::Any(any) => {
                let expr = self.print_any(any)?;
                Ok(quote!(#expr;))
            }
            BlockStmt::Noop => Ok(quote!(;)),
        }
    }
    pub fn print_stmt_chunk(&self, items: &[BlockStmt]) -> Result<TokenStream> {
        let mut stmts = vec![];
        for item in items {
            let item = self.print_statement(item)?;
            stmts.push(item);
        }
        Ok(quote!(#(#stmts) *))
    }
    pub fn print_block(&self, n: &ExprBlock) -> Result<TokenStream> {
        let inner = self.print_block_no_braces(n)?;
        Ok(quote!({
            #inner
        }))
    }
    pub fn print_block_no_braces(&self, n: &ExprBlock) -> Result<TokenStream> {
        let chunk = self.print_stmt_chunk(&n.stmts)?;
        Ok(quote!(
            #chunk
        ))
    }
    fn print_if(&self, if_: &ExprIf) -> Result<TokenStream> {
        let cond = self.print_expr(&if_.cond)?;
        let then = self.print_expr_no_braces(&if_.then)?;
        let elze = if let Some(elze) = &if_.elze {
            let elze = self.print_expr(elze)?;
            quote!(else #elze)
        } else {
            quote!()
        };
        Ok(quote!(
            if #cond {
                #then
            }
            #elze
        ))
    }

    pub fn print_match(&self, m: &ExprMatch) -> Result<TokenStream> {
        let mut ts = vec![];
        for (_i, c) in m.cases.iter().enumerate() {
            let node = &c.cond;
            let co = self.print_expr(node)?;
            let node = &c.body;
            let ex = self.print_expr_no_braces(node)?;
            ts.push(quote!(
                if #co => { #ex }
            ))
        }
        Ok(quote!(match () {
            () #(#ts)*
            _ => {}
        }))
    }

    pub fn print_invoke(&self, node: &ExprInvoke) -> Result<TokenStream> {
        let fun = self.print_invoke_target(&node.target)?;
        let args: Vec<_> = node
            .args
            .iter()
            .map(|x| self.print_expr(&x.get()))
            .try_collect()?;
        match &node.target {
            ExprInvokeTarget::Function(_) => {
                return Ok(quote!(
                    #fun(#(#args), *)
                ));
            }
            ExprInvokeTarget::Type(_) => {
                return Ok(quote!(
                    <#fun>::<#(#args), *>
                ));
            }
            ExprInvokeTarget::BinOp(op) => {
                let ret = match op {
                    BinOpKind::Add => quote!(#(#args) + *),
                    BinOpKind::AddTrait => quote!(#(#args) + *),
                    BinOpKind::Sub => quote!(#(#args) - *),
                    BinOpKind::Div => quote!(#(#args) / *),
                    BinOpKind::Mul => {
                        let mut result = vec![];
                        for (i, a) in args.into_iter().enumerate() {
                            if i != 0 {
                                result.push(quote!(*));
                            }
                            result.push(a);
                        }
                        quote!(#(#result)*)
                    }
                    BinOpKind::Mod => quote!(#(#args) % *),
                    BinOpKind::Gt => quote!(#(#args) > *),
                    BinOpKind::Lt => quote!(#(#args) < *),
                    BinOpKind::Ge => quote!(#(#args) >= *),
                    BinOpKind::Le => quote!(#(#args) <= *),
                    BinOpKind::Eq => quote!(#(#args) == *),
                    BinOpKind::Ne => quote!(#(#args) != *),
                    BinOpKind::Or => quote!(#(#args) || *),
                    BinOpKind::And => quote!(#(#args) && *),
                    BinOpKind::BitOr => quote!(#(#args) | *),
                    BinOpKind::BitAnd => quote!(#(#args) & *),
                    BinOpKind::BitXor => quote!(#(#args) ^ *),
                };
                return Ok(ret);
            }

            ExprInvokeTarget::Method(select) => match select.select {
                ExprSelectType::Field => {
                    return Ok(quote!(
                        (#fun)(#(#args), *)
                    ));
                }
                _ => {}
            },
            _ => {}
        }

        let fun_str = fun.to_string();

        // TODO: deprecate it
        let code = match fun_str.as_str() {
            "tuple" => quote!(
                (#(#args), *)
            ),
            _ => quote!(
                #fun(#(#args), *)
            ),
        };
        // if true {
        //     return Ok(quote!((#code)));
        // }
        Ok(code)
    }

    pub fn print_ref(&self, n: &ExprReference) -> Result<TokenStream> {
        let referee = self.print_expr(&n.referee.get())?;
        if n.mutable == Some(true) {
            Ok(quote!(&mut #referee))
        } else {
            Ok(quote!(&#referee))
        }
    }

    fn print_select(&self, select: &ExprSelect) -> Result<TokenStream> {
        let obj = self.print_expr(&select.obj.get())?;
        let field = self.print_ident(&select.field);
        match select.select {
            ExprSelectType::Const => Ok(quote!(
                #obj::#field
            )),
            _ => Ok(quote!(
                #obj.#field
            )),
        }
    }
    fn print_expr_try(&self, node: &AstExpr) -> Result<TokenStream> {
        let expr = self.print_expr(node)?;
        Ok(quote!(#expr?))
    }

    fn print_range(&self, range: &ExprRange) -> Result<TokenStream> {
        let start = range
            .start
            .as_ref()
            .map(|x| self.print_expr(x))
            .transpose()?;
        let end = range.end.as_ref().map(|x| self.print_expr(x)).transpose()?;
        let dots = match range.limit {
            ExprRangeLimit::Inclusive => quote!(..=),
            ExprRangeLimit::Exclusive => quote!(..),
        };
        Ok(quote!(#start #dots #end))
    }

    pub fn print_invoke_target(&self, target: &ExprInvokeTarget) -> Result<TokenStream> {
        match target {
            ExprInvokeTarget::Function(locator) => self.print_locator(locator),
            ExprInvokeTarget::Type(t) => self.print_type(t),
            ExprInvokeTarget::Method(select) => self.print_select(select),
            ExprInvokeTarget::Closure(fun) => self.print_func_value(fun),
            ExprInvokeTarget::BinOp(op) => Ok(self.print_bin_op_kind(op)),
            ExprInvokeTarget::Expr(expr) => self.print_expr(expr),
        }
    }
    fn print_expr_field_value(&self, field: &ExprField) -> Result<TokenStream> {
        let name = self.print_ident(&field.name);
        if let Some(value) = &field.value {
            let value = self.print_expr(value)?;
            Ok(quote!(#name: #value))
        } else {
            Ok(quote!(#name))
        }
    }
    pub fn print_struct_expr(&self, s: &ExprStruct) -> Result<TokenStream> {
        let name = self.print_expr(&s.name.get())?;
        let kwargs: Vec<_> = s
            .fields
            .iter()
            .map(|x| self.print_expr_field_value(x))
            .try_collect()?;
        Ok(quote!(#name { #(#kwargs), * }))
    }
    pub fn print_expr_tuple(&self, tuple: &ExprTuple) -> Result<TokenStream> {
        let args: Vec<_> = tuple
            .values
            .iter()
            .map(|x| self.print_expr(x))
            .try_collect()?;
        Ok(quote!((#(#args),*)))
    }
    pub fn print_bin_op_kind(&self, op: &BinOpKind) -> TokenStream {
        match op {
            BinOpKind::Add => quote!(+),
            BinOpKind::AddTrait => quote!(+),
            BinOpKind::Sub => quote!(-),
            BinOpKind::Mul => quote!(*),
            BinOpKind::Div => quote!(/),
            BinOpKind::Mod => quote!(%),
            BinOpKind::Gt => quote!(>),
            BinOpKind::Lt => quote!(<),
            BinOpKind::Ge => quote!(>=),
            BinOpKind::Le => quote!(<=),
            BinOpKind::Eq => quote!(==),
            BinOpKind::Ne => quote!(!=),
            BinOpKind::Or => quote!(||),
            BinOpKind::And => quote!(&&),
            BinOpKind::BitOr => quote!(|),
            BinOpKind::BitAnd => quote!(&),
            BinOpKind::BitXor => quote!(^),
        }
    }
    pub fn print_un_op_kind(&self, op: &UnOpKind) -> TokenStream {
        match op {
            UnOpKind::Neg => quote!(-),
            UnOpKind::Not => quote!(!),
            UnOpKind::Deref => quote!(*),
            UnOpKind::Any(any) => self.print_ident(any),
        }
    }
    pub fn print_un_op(&self, expr: &ExprUnOp) -> Result<TokenStream> {
        let op = self.print_un_op_kind(&expr.op);
        let value = self.print_expr(&expr.val)?;
        Ok(quote!(#op #value))
    }
    fn print_expr_closure(&self, closure: &ExprClosure) -> Result<TokenStream> {
        let movability = if closure.movability == Some(true) {
            quote!(move)
        } else {
            quote!()
        };
        let params: Vec<_> = closure
            .params
            .iter()
            .map(|x| self.print_pattern(x))
            .try_collect()?;
        let ret = self.print_return_type(closure.ret_ty.as_deref())?;
        let body = self.print_expr(&closure.body)?;
        Ok(quote!(#movability |#(#params),*| #ret #body))
    }
    fn print_expr_array(&self, array: &ExprArray) -> Result<TokenStream> {
        let values: Vec<_> = array
            .values
            .iter()
            .map(|x| self.print_expr(x))
            .try_collect()?;
        Ok(quote!([#(#values),*]))
    }
}
