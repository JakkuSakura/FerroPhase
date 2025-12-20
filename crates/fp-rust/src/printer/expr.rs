use fp_core::{bail, Result};
use itertools::Itertools;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use std::str::FromStr;
use syn::LitStr;

use fp_core::ast::{
    BlockStmt, Expr, ExprArray, ExprArrayRepeat, ExprAssign, ExprAwait, ExprBinOp, ExprBlock,
    ExprCast, ExprClosure, ExprDereference, ExprField, ExprFor, ExprFormatString, ExprIf,
    ExprIndex, ExprIntrinsicCall, ExprIntrinsicContainer, ExprInvoke, ExprInvokeTarget, ExprKind,
    ExprLet, ExprLoop, ExprMacro, ExprMatch, ExprParen, ExprRange, ExprRangeLimit, ExprReference,
    ExprSelect, ExprSelectType, ExprSplat, ExprSplatDict, ExprStruct, ExprStructural, ExprTuple,
    ExprUnOp, ExprWhile, FormatArgRef, FormatTemplatePart, Item, MacroDelimiter, StmtLet, Ty,
    Value, ValueList, Visibility,
};
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};
use fp_core::ops::{BinOpKind, UnOpKind};

use crate::printer::RustPrinter;

impl RustPrinter {
    pub fn print_expr_no_braces(&self, node: &Expr) -> Result<TokenStream> {
        match node.kind() {
            ExprKind::Block(n) => self.print_block_no_braces(&n),
            ExprKind::Value(v) if v.is_unit() => Ok(quote!()),
            _ => self.print_expr(node),
        }
    }
    pub fn print_expr_id(&self, id: u64) -> Result<TokenStream> {
        let ident = format_ident!("_expr_{}", id);
        Ok(quote!(#ident))
    }

    pub fn print_expr(&self, node: &Expr) -> Result<TokenStream> {
        match node.kind() {
            ExprKind::Id(id) => self.print_expr_id(*id),
            ExprKind::Locator(loc) => self.print_locator(loc),
            ExprKind::Value(v) => {
                if let Some(ty) = node.ty() {
                    if let (Ty::Array(arr), Value::List(ValueList { values })) = (ty, v.as_ref()) {
                        if let Some(first) = values.first() {
                            let all_same = values.iter().all(|val| val == first);
                            if all_same {
                                let elem = self.print_value(first)?;
                                let len = self.print_expr(arr.len.as_ref())?;
                                return Ok(quote!([#elem; #len]));
                            }
                        }
                        let elems: Vec<_> =
                            values.iter().map(|x| self.print_value(x)).try_collect()?;
                        return Ok(quote!([#(#elems),*]));
                    }
                }
                self.print_value(v)
            }
            ExprKind::Invoke(n) => self.print_invoke_expr(n),
            ExprKind::UnOp(op) => self.print_un_op(op),
            ExprKind::BinOp(op) => self.print_bin_op(op),
            ExprKind::Any(n) => self.print_any(n),
            ExprKind::Match(n) => self.print_match(n),
            ExprKind::If(n) => self.print_if(n),
            ExprKind::Block(n) => self.print_block(n),
            ExprKind::Struct(n) => self.print_struct_expr(n),
            ExprKind::Select(n) => self.print_select(n),
            ExprKind::Reference(n) => self.print_ref(n),
            ExprKind::Assign(n) => self.print_assign(n),
            ExprKind::Index(n) => self.print_index(n),
            ExprKind::Closured(n) => self.print_expr(&n.expr),
            ExprKind::Paren(n) => self.print_paren(n),
            ExprKind::Loop(n) => self.print_loop(n),
            ExprKind::Range(n) => self.print_range(n),
            ExprKind::Tuple(n) => self.print_expr_tuple(n),
            ExprKind::Try(n) => self.print_expr_try(&n.expr),
            ExprKind::While(n) => self.print_while(n),
            ExprKind::Let(n) => self.print_expr_let(n),
            ExprKind::Closure(n) => self.print_expr_closure(n),
            ExprKind::Array(n) => self.print_expr_array(n),
            ExprKind::ArrayRepeat(n) => self.print_expr_array_repeat(n),
            ExprKind::Await(n) => self.print_expr_await(n),
            ExprKind::Async(n) => self.print_expr(&n.expr),
            ExprKind::For(n) => self.print_expr_for(n),
            ExprKind::IntrinsicContainer(n) => self.print_intrinsic_container(n),
            ExprKind::IntrinsicCall(n) => self.print_intrinsic_call(n),
            ExprKind::Quote(_n) => Ok(quote!({ /* quote */ })),
            ExprKind::Splice(_n) => Ok(quote!({ /* splice */ })),
            ExprKind::Structural(n) => self.print_structural_expr(n),
            ExprKind::Dereference(n) => self.print_expr_dereference(n),
            ExprKind::FormatString(n) => self.print_expr_format_string(n),
            ExprKind::Splat(n) => self.print_expr_splat(n),
            ExprKind::SplatDict(n) => self.print_expr_splat_dict(n),
            ExprKind::Macro(n) => self.print_macro_expr(n),
            ExprKind::Item(item) => self.print_expr_item(item),
            ExprKind::Cast(n) => self.print_expr_cast(n),
        }
    }

    fn print_expr_for(&self, for_expr: &ExprFor) -> Result<TokenStream> {
        // Emit a straightforward Rust-style `for` loop.
        //
        // Heuristic: `xs.iter().enumerate()` yields `(usize, &T)`. When the
        // pattern is `(i, x)`, bind the element by value as `(i, &x)`.
        let pat_ts = match for_expr.pat.kind() {
            fp_core::ast::PatternKind::Tuple(tuple)
                if tuple.patterns.len() == 2
                    && matches!(
                        for_expr.iter.kind(),
                        ExprKind::Invoke(fp_core::ast::ExprInvoke {
                            target: fp_core::ast::ExprInvokeTarget::Method(_),
                            ..
                        })
                    )
                    && is_iter_enumerate_over_iter(for_expr.iter.as_ref()) =>
            {
                let first = self.print_pattern(&tuple.patterns[0])?;
                let second = match tuple.patterns[1].kind() {
                    fp_core::ast::PatternKind::Ident(ident) => {
                        let id = self.print_pat_ident(ident)?;
                        quote!(&#id)
                    }
                    _ => self.print_pattern(&tuple.patterns[1])?,
                };
                quote!((#first, #second))
            }
            _ => self.print_pattern(&for_expr.pat)?,
        };
        let iter = self.print_expr(&for_expr.iter)?;
        let body = self.print_expr_no_braces(&for_expr.body)?;
        Ok(quote!(for #pat_ts in #iter { #body }))
    }

    fn print_block_item(&self, item: &Item) -> Result<TokenStream> {
        use fp_core::ast::ItemKind;

        match item.kind() {
            ItemKind::DefConst(def) => {
                let name = self.print_ident(&def.name);
                let ty = def
                    .ty
                    .as_ref()
                    .or_else(|| def.ty_annotation())
                    .map(|ty| self.print_type(ty))
                    .transpose()?;
                let value = self.print_expr(def.value.as_ref())?;

                // Prefer `const` when the initializer is const-like, so that nested
                // items (impls, enums) can refer to it and `const { ... }` blocks can
                // use it.
                if ty.is_some() && is_const_like_expr(def.value.as_ref()) {
                    let ty = ty.expect("ty checked");
                    Ok(quote!(const #name: #ty = #value;))
                } else if let Some(ty) = ty {
                    let initializer_is_callable = matches!(
                        def.value.kind(),
                        ExprKind::Invoke(_) | ExprKind::Closure(_) | ExprKind::Closured(_)
                    );
                    let annotated_as_fn = matches!(def.ty.as_ref(), Some(Ty::Function(_)));
                    if annotated_as_fn && initializer_is_callable {
                        Ok(quote!(let #name = #value;))
                    } else {
                        Ok(quote!(let #name: #ty = #value;))
                    }
                } else {
                    Ok(quote!(let #name = #value;))
                }
            }
            ItemKind::DefStatic(def) => {
                let name = self.print_ident(&def.name);
                let ty = def.ty_annotation().unwrap_or(&def.ty);
                let ty = self.print_type(ty)?;
                let value = self.print_expr(def.value.as_ref())?;
                Ok(quote!(let #name: #ty = #value;))
            }
            ItemKind::DefFunction(def) => {
                let func = self.print_function(&def.sig, &def.body, &Visibility::Inherited)?;
                Ok(quote!(#func))
            }
            ItemKind::DefStruct(def) => {
                let mut def = def.clone();
                def.visibility = Visibility::Inherited;
                self.print_def_struct(&def)
            }
            ItemKind::DefStructural(def) => {
                let mut def = def.clone();
                def.visibility = Visibility::Inherited;
                self.print_def_structural(&def)
            }
            ItemKind::DefEnum(def) => {
                let mut def = def.clone();
                def.visibility = Visibility::Inherited;
                self.print_def_enum(&def)
            }
            ItemKind::DefTrait(def) => {
                let mut def = def.clone();
                def.visibility = Visibility::Inherited;
                self.print_def_trait(&def)
            }
            ItemKind::DefType(def) => {
                let mut def = def.clone();
                def.visibility = Visibility::Inherited;
                self.print_def_type(&def)
            }
            ItemKind::Import(def) => {
                let mut def = def.clone();
                def.visibility = Visibility::Inherited;
                self.print_import(&def)
            }
            // impl blocks and modules don't carry visibility on the item itself.
            _ => self.print_item(item),
        }
    }

    fn print_intrinsic_container(
        &self,
        collection: &ExprIntrinsicContainer,
    ) -> fp_core::Result<TokenStream> {
        self.print_expr(&collection.clone().into_const_expr())
    }

    fn print_expr_cast(&self, cast: &ExprCast) -> Result<TokenStream> {
        let expr = self.print_expr(&cast.expr)?;
        let ty = self.print_type(&cast.ty)?;
        Ok(quote!(#expr as #ty))
    }

    fn print_macro_expr(&self, mac: &ExprMacro) -> Result<TokenStream> {
        let path = self.print_path(&mac.invocation.path);
        let tokens =
            TokenStream::from_str(&mac.invocation.tokens).unwrap_or_else(|_| TokenStream::new());
        let expanded = match mac.invocation.delimiter {
            MacroDelimiter::Parenthesis => quote!(#path!(#tokens)),
            MacroDelimiter::Bracket => quote!(#path![#tokens]),
            MacroDelimiter::Brace => quote!(#path!{#tokens}),
        };
        Ok(expanded)
    }

    fn print_expr_await(&self, await_expr: &ExprAwait) -> Result<TokenStream> {
        let base = self.print_expr(&await_expr.base)?;
        Ok(quote!((#base).await))
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
            BlockStmt::Item(item) => self.print_block_item(item),
            BlockStmt::Let(let_) => self.print_stmt_let(let_),
            BlockStmt::Expr(expr0) => {
                let expr = self.print_expr(&expr0.expr)?;
                let with_semicolon;

                if expr0.semicolon == Some(true) {
                    with_semicolon = true;
                } else if expr0.semicolon == Some(false) {
                    with_semicolon = false;
                } else {
                    match expr0.expr.kind() {
                        ExprKind::Block(_) | ExprKind::If(_) => with_semicolon = false,
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
        if let Some(scrutinee) = m.scrutinee.as_ref() {
            let scrutinee = self.print_expr(scrutinee)?;
            let mut arms = Vec::new();
            for case in &m.cases {
                let pat = case
                    .pat
                    .as_ref()
                    .ok_or_else(|| fp_core::error::Error::from("match arm missing pattern"))?;
                let pat_tokens = self.print_pattern(pat)?;
                let guard_tokens = if let Some(guard) = case.guard.as_ref() {
                    let guard = self.print_expr(guard)?;
                    quote!(if #guard)
                } else {
                    quote!()
                };
                let body = self.print_expr_no_braces(&case.body)?;
                arms.push(quote!(#pat_tokens #guard_tokens => { #body }));
            }
            return Ok(quote!(match #scrutinee { #(#arms,)* }));
        }

        // Legacy lowering: boolean conditions.
        let mut ts = vec![];
        for c in m.cases.iter() {
            let co = self.print_expr(&c.cond)?;
            let ex = self.print_expr_no_braces(&c.body)?;
            ts.push(quote!(if #co => { #ex }));
        }
        Ok(quote!(match () { () #(#ts)* _ => {} }))
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
                    BinOpKind::Shl => quote!(#(#args) << *),
                    BinOpKind::Shr => quote!(#(#args) >> *),
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
    fn print_expr_try(&self, node: &Expr) -> Result<TokenStream> {
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
            BinOpKind::Shl => quote!(<<),
            BinOpKind::Shr => quote!(>>),
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
        // Default to `move` closures: it's the most robust translation for FP
        // closures, especially when they escape the defining scope.
        let movability = match closure.movability {
            Some(true) | None => quote!(move),
            Some(false) => quote!(),
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
        // FP slice types map to `&[T]` in Rust; borrowing array literals keeps
        // call sites ergonomic (`[1,2,3]` coerces to a slice when borrowed).
        Ok(quote!(&[#(#values),*]))
    }

    fn print_expr_array_repeat(&self, array: &ExprArrayRepeat) -> Result<TokenStream> {
        let elem = self.print_expr(array.elem.as_ref())?;
        let len = self.print_expr(array.len.as_ref())?;
        Ok(quote!(&[#elem; #len]))
    }

    fn print_expr_dereference(&self, deref: &ExprDereference) -> Result<TokenStream> {
        let value = self.print_expr(deref.referee.as_ref())?;
        Ok(quote!(*#value))
    }

    fn print_structural_expr(&self, structural: &ExprStructural) -> Result<TokenStream> {
        let fields: Vec<_> = structural
            .fields
            .iter()
            .map(|field| self.print_expr_field_value(field))
            .try_collect()?;
        Ok(quote!({ #(#fields),* }))
    }

    fn print_expr_splat(&self, splat: &ExprSplat) -> Result<TokenStream> {
        let iter = self.print_expr(splat.iter.as_ref())?;
        Ok(quote!(*#iter))
    }

    fn print_expr_splat_dict(&self, splat: &ExprSplatDict) -> Result<TokenStream> {
        let dict = self.print_expr(splat.dict.as_ref())?;
        Ok(quote!(**#dict))
    }

    fn print_expr_item(&self, item: &Item) -> Result<TokenStream> {
        let tokens = self.print_item(item)?;
        Ok(quote!({ #tokens }))
    }

    fn print_intrinsic_call(&self, call: &ExprIntrinsicCall) -> Result<TokenStream> {
        match call.kind {
            IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                self.print_print_intrinsic(call)
            }
            IntrinsicCallKind::SizeOf
            | IntrinsicCallKind::FieldCount
            | IntrinsicCallKind::MethodCount
            | IntrinsicCallKind::TypeName => {
                let args = match &call.payload {
                    IntrinsicCallPayload::Args { args } => args,
                    IntrinsicCallPayload::Format { .. } => {
                        bail!("intrinsic expects args payload")
                    }
                };
                if args.len() != 1 {
                    bail!("intrinsic {:?} expects exactly 1 argument", call.kind)
                }
                let ty = match args[0].kind() {
                    ExprKind::Locator(locator) => self.print_locator(locator)?,
                    _ => bail!("intrinsic {:?} expects a type locator", call.kind),
                };

                match call.kind {
                    IntrinsicCallKind::SizeOf => Ok(quote!(::core::mem::size_of::<#ty>())),
                    IntrinsicCallKind::FieldCount => {
                        Ok(quote!(::fp_rust::intrinsic_field_count::<#ty>()))
                    }
                    IntrinsicCallKind::MethodCount => {
                        Ok(quote!(::fp_rust::intrinsic_method_count::<#ty>()))
                    }
                    IntrinsicCallKind::TypeName => {
                        Ok(quote!(::fp_rust::intrinsic_type_name::<#ty>()))
                    }
                    _ => unreachable!(),
                }
            }
            IntrinsicCallKind::HasField | IntrinsicCallKind::HasMethod => {
                let args = match &call.payload {
                    IntrinsicCallPayload::Args { args } => args,
                    IntrinsicCallPayload::Format { .. } => {
                        bail!("intrinsic expects args payload")
                    }
                };
                if args.len() != 2 {
                    bail!("intrinsic {:?} expects exactly 2 arguments", call.kind)
                }
                let ty = match args[0].kind() {
                    ExprKind::Locator(locator) => self.print_locator(locator)?,
                    _ => bail!("intrinsic {:?} expects a type locator", call.kind),
                };
                let name = self.print_expr(&args[1])?;
                match call.kind {
                    IntrinsicCallKind::HasField => Ok(quote!(
                        ::fp_rust::intrinsic_has_field::<#ty>(#name)
                    )),
                    IntrinsicCallKind::HasMethod => Ok(quote!(
                        ::fp_rust::intrinsic_has_method::<#ty>(#name)
                    )),
                    _ => unreachable!(),
                }
            }
            IntrinsicCallKind::ConstBlock => {
                let args = match &call.payload {
                    IntrinsicCallPayload::Args { args } => args,
                    IntrinsicCallPayload::Format { .. } => {
                        bail!("const_block intrinsic expects args payload")
                    }
                };
                if args.len() != 1 {
                    bail!("const_block intrinsic expects exactly 1 argument")
                }
                let arg = &args[0];
                if matches!(arg.kind(), ExprKind::Block(_)) {
                    let block = self.print_expr(arg)?;
                    Ok(quote!(const #block))
                } else {
                    let expr = self.print_expr(arg)?;
                    Ok(quote!(const { #expr }))
                }
            }
            IntrinsicCallKind::Break => {
                let args = match &call.payload {
                    IntrinsicCallPayload::Args { args } => args,
                    IntrinsicCallPayload::Format { .. } => {
                        bail!("break intrinsic expects args payload")
                    }
                };
                match args.as_slice() {
                    [] => Ok(quote!(break)),
                    [value] => {
                        let value = self.print_expr(value)?;
                        Ok(quote!(break #value))
                    }
                    _ => bail!("break intrinsic accepts at most one argument"),
                }
            }
            IntrinsicCallKind::Continue => Ok(quote!(continue)),
            IntrinsicCallKind::DebugAssertions => Ok(quote!(cfg!(debug_assertions))),
            IntrinsicCallKind::Return => {
                let args: Vec<_> = match &call.payload {
                    IntrinsicCallPayload::Args { args } => {
                        args.iter().map(|arg| self.print_expr(arg)).try_collect()?
                    }
                    IntrinsicCallPayload::Format { .. } => {
                        bail!("return intrinsic expects args payload")
                    }
                };
                match args.as_slice() {
                    [] => Ok(quote!(return)),
                    [value] => Ok(quote!(return #value)),
                    _ => bail!("return intrinsic accepts at most one argument"),
                }
            }
            _ => self.print_generic_intrinsic(call),
        }
    }

    fn print_print_intrinsic(&self, call: &ExprIntrinsicCall) -> Result<TokenStream> {
        let template = match &call.payload {
            IntrinsicCallPayload::Format { template } => template,
            IntrinsicCallPayload::Args { .. } => {
                bail!("print intrinsics expect format payloads")
            }
        };

        let (literal, args) = self.prepare_format_args(template)?;
        let macro_ident = if matches!(call.kind, IntrinsicCallKind::Println) {
            format_ident!("println")
        } else {
            format_ident!("print")
        };

        Ok({
            let args_iter = args.iter();
            quote!(#macro_ident!(#literal #(, #args_iter)* ))
        })
    }

    fn print_generic_intrinsic(&self, call: &ExprIntrinsicCall) -> Result<TokenStream> {
        let ident = self.intrinsic_function_ident(call.kind);
        let args: Vec<TokenStream> = match &call.payload {
            IntrinsicCallPayload::Args { args } => {
                args.iter().map(|arg| self.print_expr(arg)).try_collect()?
            }
            IntrinsicCallPayload::Format { template } => {
                vec![self.print_expr_format_string(template)?]
            }
        };

        if args.is_empty() {
            Ok(quote!(#ident()))
        } else {
            let args_iter = args.iter();
            Ok(quote!(#ident(#(#args_iter),*)))
        }
    }

    fn intrinsic_function_ident(&self, kind: IntrinsicCallKind) -> TokenStream {
        match kind {
            IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                unreachable!("print intrinsics handled separately")
            }
            IntrinsicCallKind::Len => quote!(len),
            IntrinsicCallKind::ConstBlock => quote!(intrinsic_const_block),
            IntrinsicCallKind::DebugAssertions => quote!(intrinsic_debug_assertions),
            IntrinsicCallKind::Input => quote!(intrinsic_input),
            IntrinsicCallKind::Break => quote!(intrinsic_break),
            IntrinsicCallKind::Continue => quote!(intrinsic_continue),
            IntrinsicCallKind::Return => quote!(intrinsic_return),
            IntrinsicCallKind::SizeOf => quote!(intrinsic_size_of),
            IntrinsicCallKind::ReflectFields => quote!(intrinsic_reflect_fields),
            IntrinsicCallKind::HasMethod => quote!(intrinsic_has_method),
            IntrinsicCallKind::TypeName => quote!(intrinsic_type_name),
            IntrinsicCallKind::CreateStruct => quote!(intrinsic_create_struct),
            IntrinsicCallKind::CloneStruct => quote!(intrinsic_clone_struct),
            IntrinsicCallKind::AddField => quote!(intrinsic_add_field),
            IntrinsicCallKind::HasField => quote!(intrinsic_has_field),
            IntrinsicCallKind::FieldCount => quote!(intrinsic_field_count),
            IntrinsicCallKind::MethodCount => quote!(intrinsic_method_count),
            IntrinsicCallKind::FieldType => quote!(intrinsic_field_type),
            IntrinsicCallKind::StructSize => quote!(intrinsic_struct_size),
            IntrinsicCallKind::GenerateMethod => quote!(intrinsic_generate_method),
            IntrinsicCallKind::CompileError => quote!(intrinsic_compile_error),
            IntrinsicCallKind::CompileWarning => quote!(intrinsic_compile_warning),
        }
    }

    fn prepare_format_args(
        &self,
        template: &ExprFormatString,
    ) -> Result<(LitStr, Vec<TokenStream>)> {
        let mut literal = String::new();
        for part in &template.parts {
            match part {
                FormatTemplatePart::Literal(text) => literal.push_str(text),
                FormatTemplatePart::Placeholder(placeholder) => {
                    literal.push('{');
                    match &placeholder.arg_ref {
                        FormatArgRef::Implicit => {}
                        FormatArgRef::Positional(idx) => literal.push_str(&idx.to_string()),
                        FormatArgRef::Named(name) => literal.push_str(name),
                    }
                    if let Some(spec) = &placeholder.format_spec {
                        literal.push(':');
                        literal.push_str(spec);
                    }
                    literal.push('}');
                }
            }
        }

        let mut args: Vec<TokenStream> = template
            .args
            .iter()
            .map(|arg| self.print_print_arg(arg))
            .try_collect()?;

        for kwarg in &template.kwargs {
            let ident = format_ident!("{}", kwarg.name);
            let value = self.print_expr(&kwarg.value)?;
            args.push(quote!(#ident = #value));
        }

        Ok((LitStr::new(&literal, Span::call_site()), args))
    }

    fn print_print_arg(&self, arg: &Expr) -> Result<TokenStream> {
        let expr = self.print_expr(arg)?;
        match arg.kind() {
            ExprKind::Value(value) if matches!(value.as_ref(), Value::Unit(_) | Value::Null(_)) => {
                Ok(quote!(format_args!("{:?}", #expr)))
            }
            _ => Ok(expr),
        }
    }

    fn print_expr_format_string(&self, template: &ExprFormatString) -> Result<TokenStream> {
        let (literal, args) = self.prepare_format_args(template)?;
        let args_iter = args.iter();
        Ok(quote!(format!(#literal #(, #args_iter)* )))
    }
}

fn is_const_like_expr(expr: &Expr) -> bool {
    match expr.kind() {
        ExprKind::Value(_) | ExprKind::Locator(_) => true,
        ExprKind::Paren(p) => is_const_like_expr(&p.expr.get()),
        ExprKind::Cast(c) => is_const_like_expr(c.expr.as_ref()),
        ExprKind::Reference(r) => is_const_like_expr(r.referee.as_ref()),
        ExprKind::Dereference(d) => is_const_like_expr(d.referee.as_ref()),
        ExprKind::BinOp(b) => {
            is_const_like_expr(b.lhs.as_ref()) && is_const_like_expr(b.rhs.as_ref())
        }
        ExprKind::UnOp(u) => is_const_like_expr(u.val.as_ref()),
        ExprKind::Tuple(t) => t.values.iter().all(is_const_like_expr),
        ExprKind::Array(a) => a.values.iter().all(is_const_like_expr),
        ExprKind::ArrayRepeat(r) => {
            is_const_like_expr(r.elem.as_ref()) && is_const_like_expr(r.len.as_ref())
        }
        ExprKind::Struct(s) => {
            is_const_like_expr(s.name.as_ref())
                && s.fields
                    .iter()
                    .all(|f| f.value.as_ref().map(is_const_like_expr).unwrap_or(true))
        }
        ExprKind::If(i) => {
            is_const_like_expr(i.cond.as_ref())
                && is_const_like_expr(i.then.as_ref())
                && i.elze
                    .as_ref()
                    .map(|e| is_const_like_expr(e.as_ref()))
                    .unwrap_or(true)
        }
        // Be conservative: closures, invokes, loops, macros, etc. are not const-like.
        _ => false,
    }
}

fn is_iter_enumerate_over_iter(expr: &Expr) -> bool {
    let ExprKind::Invoke(invoke) = expr.kind() else {
        return false;
    };
    let fp_core::ast::ExprInvokeTarget::Method(select) = &invoke.target else {
        return false;
    };
    if select.field.as_str() != "enumerate" || !invoke.args.is_empty() {
        return false;
    }

    let ExprKind::Invoke(inner_invoke) = select.obj.kind() else {
        return false;
    };
    let fp_core::ast::ExprInvokeTarget::Method(inner_select) = &inner_invoke.target else {
        return false;
    };
    inner_select.field.as_str() == "iter" && inner_invoke.args.is_empty()
}
