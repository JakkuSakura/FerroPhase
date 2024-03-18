use crate::{RawExpr, RawExprMacro, RawStmtMacro};
use common::*;
use itertools::Itertools;
use lang_core::ast::*;
use lang_core::id::{Ident, Locator, ParameterPath, ParameterPathSegment, Path};
use lang_core::ops::{BinOpKind, BuiltinFn, BuiltinFnName};
use lang_core::pat::{Pattern, PatternIdent};
use lang_core::utils::anybox::AnyBox;
use proc_macro2::{Span, TokenStream};
use quote::*;

pub struct RustPrinter;

impl RustPrinter {
    pub fn print_ident(&self, i: &Ident) -> TokenStream {
        match i.as_str() {
            "+" => quote!(+),
            "*" => quote!(*),
            ">" => quote!(>),
            ">=" => quote!(>=),
            "<" => quote!(<),
            "<=" => quote!(<=),
            "==" => quote!(==),
            "!=" => quote!(!=),
            "|" => quote!(|),
            "&Self" => quote!(&Self),
            "&mut Self" => quote!(&mut Self),
            "Self" => quote!(Self),
            "mut Self" => quote!(mut Self),
            "unit" => quote!(()),
            a => format_ident!("{}", a).into_token_stream(),
        }
    }
    pub fn print_pat_ident(&self, i: &PatternIdent) -> Result<TokenStream> {
        let mut_ = if i.mutability.unwrap_or_default() {
            quote!(mut)
        } else {
            quote!()
        };
        let name = self.print_ident(&i.ident);
        Ok(quote!(#mut_ #name))
    }
    pub fn print_trait_bound(&self, n: &DefTrait) -> Result<TokenStream> {
        let name = self.print_ident(&n.name);
        let bounds = self.print_type_bounds(&n.bounds)?;
        Ok(quote!(
            #name: #bounds
        ))
    }

    pub fn print_def_struct(&self, def: &DefStruct) -> Result<TokenStream> {
        let name = self.print_ident(&def.name);
        let fields: Vec<_> = def
            .value
            .fields
            .iter()
            .map(|x| self.print_field(&x))
            .try_collect()?;
        Ok(quote!(
            struct #name {
                #(#fields), *
            }
        ))
    }
    pub fn print_def_type(&self, def: &DefType) -> Result<TokenStream> {
        let name = self.print_ident(&def.name);
        let ty = self.print_type_value(&def.value)?;
        return Ok(quote!(
            type #name = t!{ #ty };
        ));
    }
    pub fn print_def_const(&self, def: &DefConst) -> Result<TokenStream> {
        let name = self.print_ident(&def.name);
        let ty = self.print_type_value(&def.ty.as_ref().context("No type")?.clone())?;
        let value = self.print_value(&def.value)?;
        return Ok(quote!(
            const #name: #ty = #value;
        ));
    }
    pub fn print_def_trait(&self, def: &DefTrait) -> Result<TokenStream> {
        let vis = self.print_vis(def.visibility);
        let name = self.print_ident(&def.name);
        let ty = self.print_type_bounds(&def.bounds)?;
        let items = self.print_items_chunk(&def.items)?;
        return Ok(quote!(
            #vis trait #name #ty {
                #items
            }
        ));
    }

    pub fn print_field(&self, field: &FieldTypeValue) -> Result<TokenStream> {
        let name = self.print_ident(&field.name);
        let ty = self.print_type_value(&field.value)?;
        Ok(quote!(pub #name: #ty ))
    }
    pub fn print_struct_type(&self, s: &TypeStruct) -> Result<TokenStream> {
        let name = self.print_ident(&s.name);
        let fields: Vec<_> = s
            .fields
            .iter()
            .map(|x| self.print_field(&x))
            .try_collect()?;
        Ok(quote!(struct #name {
            #(#fields), *
        }))
    }
    pub fn print_unnamed_struct_type(&self, s: &TypeStructural) -> Result<TokenStream> {
        let fields: Vec<_> = s
            .fields
            .iter()
            .map(|x| self.print_field(&x))
            .try_collect()?;
        Ok(quote!(
            struct {
                #(#fields), *
            }
        ))
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
    pub fn print_bin_op(&self, binop: &ExprBinOp) -> Result<TokenStream> {
        let lhs = self.print_expr(&binop.lhs.get())?;
        let rhs = self.print_expr(&binop.rhs.get())?;
        let op = self.print_bin_op_kind(&binop.kind);
        Ok(quote!(#lhs #op #rhs))
    }
    pub fn print_invoke_expr(&self, invoke: &ExprInvoke) -> Result<TokenStream> {
        match invoke.func.get() {
            Expr::Value(value) => match value.as_ref() {
                Value::BinOpKind(op) => {
                    let op = self.print_bin_op_kind(&op);
                    let args: Vec<_> = invoke
                        .args
                        .iter()
                        .map(|x| self.print_expr(&x.get()))
                        .try_collect()?;
                    let mut stream = quote!();
                    for (i, a) in args.into_iter().enumerate() {
                        if i != 0 {
                            stream = quote!(#stream #op);
                        }
                        stream = quote!(#stream #a);
                    }
                    return Ok(stream);
                }
                _ => {}
            },
            _ => {}
        }
        let fun = self.print_expr(&invoke.func.get())?;
        let args: Vec<_> = invoke
            .args
            .iter()
            .map(|x| self.print_expr(&x.get()))
            .try_collect()?;
        Ok(quote!(
            #fun(#(#args), *)
        ))
    }
    pub fn print_invoke_type(&self, invoke: &ExprInvoke) -> Result<TokenStream> {
        let fun = self.print_expr(&invoke.func.get())?;
        let args: Vec<_> = invoke
            .args
            .iter()
            .map(|x| self.print_expr(&x.get()))
            .try_collect()?;
        Ok(quote!(
            #fun::<#(#args), *>
        ))
    }

    pub fn print_items_chunk(&self, items: &[Item]) -> Result<TokenStream> {
        let mut stmts = vec![];
        for item in items {
            let item = self.print_item(item)?;
            stmts.push(item);
        }
        Ok(quote!(#(#stmts) *))
    }
    pub fn print_pattern(&self, pat: &Pattern) -> Result<TokenStream> {
        match pat {
            Pattern::Ident(ident) => self.print_pat_ident(ident),
            Pattern::Tuple(tuple) => {
                let tuple: Vec<_> = tuple
                    .patterns
                    .iter()
                    .map(|x| self.print_pattern(x))
                    .try_collect()?;
                Ok(quote!(#(#tuple), *))
            }
            Pattern::TupleStruct(tuple) => {
                let name = self.print_locator(&tuple.name)?;
                let tuple: Vec<_> = tuple
                    .patterns
                    .iter()
                    .map(|x| self.print_pattern(x))
                    .try_collect()?;
                Ok(quote!(#name(#(#tuple), *)))
            }
            Pattern::Struct(stru) => {
                let name = self.print_ident(&stru.name);
                let fields: Vec<_> = stru
                    .fields
                    .iter()
                    .map(|x| {
                        let name = self.print_ident(&x.name);
                        let rename = if let Some(rename) = &x.rename {
                            let rename = self.print_pattern(rename)?;
                            quote!(#name: #rename)
                        } else {
                            quote!(#name)
                        };
                        Ok::<_, Error>(rename)
                    })
                    .try_collect()?;
                Ok(quote!(#name { #(#fields), * }))
            }
            Pattern::Structural(stru) => {
                let fields: Vec<_> = stru
                    .fields
                    .iter()
                    .map(|x| {
                        let name = self.print_ident(&x.name);
                        let rename = if let Some(rename) = &x.rename {
                            let rename = self.print_pattern(rename)?;
                            quote!(#name: #rename)
                        } else {
                            quote!(#name)
                        };
                        Ok::<_, Error>(rename)
                    })
                    .try_collect()?;
                Ok(quote!(struct { #(#fields), * }))
            }
            Pattern::Box(box_) => {
                let pattern = self.print_pattern(&box_.pattern)?;
                // yet this is not stable
                Ok(quote!(box #pattern))
            }
            Pattern::Variant(variant) => {
                let name = self.print_expr(&variant.name)?;
                let pattern = if let Some(pattern) = &variant.pattern {
                    let pattern = self.print_pattern(pattern)?;
                    quote!(#pattern)
                } else {
                    quote!()
                };
                Ok(quote!(#name #pattern))
            }
            _ => todo!(),
        }
    }
    pub fn print_let(&self, let_: &StatementLet) -> Result<TokenStream> {
        let pat = self.print_pattern(&let_.pat)?;

        let value = self.print_expr(&let_.value)?;

        Ok(quote!(
            let #pat = #value;
        ))
    }
    pub fn print_assign(&self, assign: &ExprAssign) -> Result<TokenStream> {
        let target = self.print_expr(&assign.target)?;
        let value = self.print_expr(&assign.value)?;
        Ok(quote!(
            #target = #value;
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
    // pub fn print_while(&self, while_: &ExprWhile) -> Result<TokenStream> {
    //     let cond = self.print_expr(&while_.cond)?;
    //     let body = self.print_block(&while_.body)?;
    //     Ok(quote!(
    //         while #cond
    //             #body
    //     ))
    // }
    // pub fn print_loop(&self, loop_: &ExprLoop) -> Result<TokenStream> {
    //     let body = self.print_block(&loop_.body)?;
    //     Ok(quote!(
    //         loop
    //             #body
    //     ))
    // }
    pub fn print_statement(&self, stmt: &Statement) -> Result<TokenStream> {
        match stmt {
            Statement::Item(item) => self.print_item(item),
            Statement::Let(let_) => self.print_let(let_),
            Statement::Expr(expr) => self.print_expr(expr),
            Statement::Any(any) => self.print_any(any),
        }
    }
    pub fn print_statement_chunk(&self, items: &[Statement]) -> Result<TokenStream> {
        let mut stmts = vec![];
        for item in items {
            let item = self.print_statement(item)?;
            stmts.push(item);
        }
        Ok(quote!(#(#stmts) *))
    }
    pub fn print_block(&self, n: &ExprBlock) -> Result<TokenStream> {
        let chunk = self.print_statement_chunk(&n.stmts)?;
        Ok(quote!({
            #chunk
        }))
    }
    pub fn print_if(&self, if_: &ExprIf) -> Result<TokenStream> {
        let cond = self.print_expr(&if_.cond)?;
        let then = self.print_expr_optimized(&if_.then)?;
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
            let ex = self.print_expr_optimized(node)?;
            ts.push(quote!(
                if #co => { #ex }
            ))
        }
        Ok(quote!(match () {
            () #(#ts)*
            _ => {}
        }))
    }
    pub fn print_vis(&self, vis: Visibility) -> TokenStream {
        match vis {
            Visibility::Public => quote!(pub),
            Visibility::Private => quote!(),
            Visibility::Inherited => quote!(),
        }
    }
    pub fn print_function(&self, func: &ValueFunction, vis: Visibility) -> Result<TokenStream> {
        let name = if let Some(name) = &func.sig.name {
            self.print_ident(name)
        } else {
            quote!()
        };
        let ret_type = &func.sig.ret;
        let ret = self.print_return_type(ret_type)?;
        let param_names: Vec<_> = func
            .sig
            .params
            .iter()
            .map(|x| self.print_ident(&x.name))
            .collect();
        let param_types: Vec<_> = func
            .sig
            .params
            .iter()
            .map(|x| self.print_type_value(&x.ty))
            .try_collect()?;
        let stmts = self.print_expr_optimized(&func.body.get())?;
        let gg;
        if !func.sig.generics_params.is_empty() {
            let gt: Vec<_> = func
                .sig
                .generics_params
                .iter()
                .map(|x| self.print_ident(&x.name))
                .collect();
            let gb: Vec<_> = func
                .sig
                .generics_params
                .iter()
                .map(|x| self.print_type_bounds(&x.bounds))
                .try_collect()?;
            gg = quote!(<#(#gt: #gb), *>)
        } else {
            gg = quote!();
        }
        let vis = self.print_vis(vis);
        return Ok(quote!(
            #vis fn #name #gg(#(#param_names: #param_types), *) #ret {
                #stmts
            }
        ));
    }
    pub fn print_invoke(&self, node: &ExprInvoke) -> Result<TokenStream> {
        let func = node.func.get();

        let fun = self.print_expr(&node.func.get())?;
        let args: Vec<_> = node
            .args
            .iter()
            .map(|x| self.print_expr(&x.get()))
            .try_collect()?;
        match &func {
            Expr::Value(value) => match value.as_ref() {
                Value::Type(_) => {
                    return Ok(quote!(
                        <#fun>::<#(#args), *>
                    ))
                }
                Value::BinOpKind(op) => {
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
                _ => {}
            },
            Expr::Select(select) => match select.select {
                SelectType::Field => {
                    return Ok(quote!(
                        (#fun)(#(#args), *)
                    ));
                }
                _ => {}
            },
            _ => {}
        }

        let fun_str = fun.to_string();

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

    pub fn print_func_type_param(&self, param: &FunctionParam) -> Result<TokenStream> {
        let name = self.print_ident(&param.name);
        let ty = self.print_type_value(&param.ty)?;
        Ok(quote!(#name: #ty))
    }
    pub fn print_return_type(&self, node: &Type) -> Result<TokenStream> {
        if matches!(node, Type::Unit(_)) {
            return Ok(quote!());
        }
        let ty = self.print_type_value(&node)?;
        Ok(quote!(-> #ty))
    }
    pub fn print_func_value(&self, fun: &ValueFunction) -> Result<TokenStream> {
        self.print_function(fun, Visibility::Private)
    }
    pub fn print_func_type(&self, fun: &TypeFunction) -> Result<TokenStream> {
        let args: Vec<_> = fun
            .params
            .iter()
            .map(|x| self.print_type_value(x))
            .try_collect()?;
        let node = &fun.ret;
        let ret = self.print_return_type(node)?;
        Ok(quote!(
            fn(#(#args), *) #ret
        ))
    }
    pub fn print_select(&self, select: &ExprSelect) -> Result<TokenStream> {
        let obj = self.print_expr(&select.obj.get())?;
        let field = self.print_ident(&select.field);
        match select.select {
            SelectType::Const => Ok(quote!(
                #obj::#field
            )),
            _ => Ok(quote!(
                #obj.#field
            )),
        }
    }
    pub fn print_args(&self, node: &Vec<Expr>) -> Result<TokenStream> {
        let args: Vec<_> = node.iter().map(|x| self.print_expr(x)).try_collect()?;
        Ok(quote!((#(#args),*)))
    }

    pub fn print_impl(&self, impl_: &Impl) -> Result<TokenStream> {
        let name = self.print_expr(&impl_.self_ty)?;
        let methods = self.print_items_chunk(&impl_.items)?;
        Ok(quote!(
            impl #name {
                #methods
            }
        ))
    }
    pub fn print_module(&self, m: &Module) -> Result<TokenStream> {
        let stmts = self.print_items_chunk(&m.items)?;

        let mod_name = format_ident!("{}", m.name.as_str());
        Ok(quote!(
            pub mod #mod_name {
                #stmts
            }
        ))
    }
    pub fn print_import(&self, node: &Import) -> Result<TokenStream> {
        let vis = self.print_vis(node.visibility);
        let segments = node
            .path
            .segments
            .iter()
            .map(|x| self.print_ident(x))
            .collect::<Vec<_>>();
        Ok(quote!(#vis use #(#segments)::*;))
    }
    pub fn print_field_value(&self, s: &FieldValue) -> Result<TokenStream> {
        let name = self.print_ident(&s.name);
        let value = self.print_value(&s.value)?;
        Ok(quote!(#name: #value))
    }
    pub fn print_struct_value(&self, s: &ValueStruct) -> Result<TokenStream> {
        let name = self.print_ident(&s.ty.name);
        let kwargs: Vec<_> = s
            .structural
            .fields
            .iter()
            .map(|x| self.print_field_value(x))
            .try_collect()?;
        Ok(quote!(#name { #(#kwargs), * }))
    }
    pub fn print_struct_expr(&self, s: &ExprInitStruct) -> Result<TokenStream> {
        let name = self.print_expr(&s.name.get())?;
        let kwargs: Vec<_> = s
            .fields
            .iter()
            .map(|x| self.print_field_value(x))
            .try_collect()?;
        Ok(quote!(#name { #(#kwargs), * }))
    }
    pub fn print_builtin_fn(&self, bt: &BuiltinFn) -> Result<TokenStream> {
        match bt.name {
            BuiltinFnName::BinOpKind(ref op) => Ok(self.print_bin_op_kind(op)),
            BuiltinFnName::Name(ref name) => Ok(self.print_ident(name)),
        }
    }
    pub fn print_int(&self, n: &ValueInt) -> Result<TokenStream> {
        let n = syn::LitInt::new(&n.value.to_string(), Span::call_site());
        Ok(quote!(#n))
    }
    pub fn print_bool(&self, n: &ValueBool) -> Result<TokenStream> {
        let n = n.value;
        Ok(quote!(#n))
    }
    pub fn print_decimal(&self, n: &ValueDecimal) -> Result<TokenStream> {
        let n = syn::LitFloat::new(&n.value.to_string(), Span::call_site());
        Ok(quote!(#n))
    }
    pub fn print_char(&self, n: &ValueChar) -> Result<TokenStream> {
        let n = n.value;
        Ok(quote!(#n))
    }
    pub fn print_string(&self, n: &ValueString) -> Result<TokenStream> {
        let v = &n.value;
        return if n.owned {
            Ok(quote!(
                #v.to_string()
            ))
        } else {
            Ok(quote!(
                #v
            ))
        };
    }
    pub fn print_list_expr(&self, n: &[Expr]) -> Result<TokenStream> {
        let n: Vec<_> = n.iter().map(|x| self.print_expr(x)).try_collect()?;
        Ok(quote!(vec![#(#n),*]))
    }
    pub fn print_list_value(&self, n: &ValueList) -> Result<TokenStream> {
        let n: Vec<_> = n.values.iter().map(|x| self.print_value(x)).try_collect()?;
        Ok(quote!(vec![#(#n),*]))
    }
    pub fn print_unit(&self, _n: &ValueUnit) -> Result<TokenStream> {
        Ok(quote!(()))
    }

    pub fn print_any(&self, n: &AnyBox) -> Result<TokenStream> {
        if let Some(n) = n.downcast_ref::<RawExprMacro>() {
            return Ok(n.raw.to_token_stream());
        }
        if let Some(n) = n.downcast_ref::<RawExpr>() {
            return Ok(n.raw.to_token_stream());
        }
        if let Some(n) = n.downcast_ref::<RawStmtMacro>() {
            return Ok(n.raw.to_token_stream());
        }
        if let Some(f) = n.downcast_ref::<BuiltinFn>() {
            return self.print_builtin_fn(f);
        }
        bail!("Not supported {:?}", n)
    }
    pub fn print_undefined(&self, _n: &ValueUndefined) -> Result<TokenStream> {
        Ok(quote!(undefined))
    }
    pub fn print_value(&self, v: &Value) -> Result<TokenStream> {
        match v {
            Value::Function(f) => self.print_func_value(f),
            Value::Int(i) => self.print_int(i),
            Value::Bool(b) => self.print_bool(b),
            Value::Decimal(d) => self.print_decimal(d),
            Value::Char(c) => self.print_char(c),
            Value::String(s) => self.print_string(s),
            Value::List(l) => self.print_list_value(l),
            Value::Unit(u) => self.print_unit(u),
            Value::Type(t) => self.print_type_value(t),
            Value::Struct(s) => self.print_struct_value(s),
            Value::Any(n) => self.print_any(n),
            Value::BinOpKind(op) => Ok(self.print_bin_op_kind(op)),
            Value::Expr(e) => self.print_expr(&e.get()),
            Value::Undefined(u) => self.print_undefined(u),
            Value::None(_) => Ok(quote!(None)),
            Value::Some(s) => {
                let s = self.print_value(&s.value)?;
                Ok(quote!(Some(#s)))
            }
            Value::Option(o) => match o.value {
                Some(ref v) => {
                    let v = self.print_value(v)?;
                    Ok(quote!(Some(#v)))
                }
                None => Ok(quote!(None)),
            },
            _ => bail!("Not supported {:?}", v),
        }
    }
    pub fn print_primitive_type(&self, ty: TypePrimitive) -> Result<TokenStream> {
        match ty {
            TypePrimitive::Int(TypeInt::I64) => Ok(quote!(i64)),
            TypePrimitive::Int(TypeInt::U64) => Ok(quote!(u64)),
            TypePrimitive::Int(TypeInt::I32) => Ok(quote!(i32)),
            TypePrimitive::Int(TypeInt::U32) => Ok(quote!(u32)),
            TypePrimitive::Int(TypeInt::I16) => Ok(quote!(i16)),
            TypePrimitive::Int(TypeInt::U16) => Ok(quote!(u16)),
            TypePrimitive::Int(TypeInt::I8) => Ok(quote!(i8)),
            TypePrimitive::Int(TypeInt::U8) => Ok(quote!(u8)),
            TypePrimitive::Decimal(DecimalType::F64) => Ok(quote!(f64)),
            TypePrimitive::Decimal(DecimalType::F32) => Ok(quote!(f32)),
            TypePrimitive::Bool => Ok(quote!(bool)),
            TypePrimitive::String => Ok(quote!(String)),
            TypePrimitive::Char => Ok(quote!(char)),
            TypePrimitive::List => Ok(quote!(Vec)),
            _ => bail!("Not supported {:?}", ty),
        }
    }
    pub fn print_impl_traits(&self, traits: &ImplTraits) -> Result<TokenStream> {
        let bounds = self.print_type_bounds(&traits.bounds)?;
        Ok(quote!(impl #bounds))
    }
    pub fn print_type_bounds(&self, bounds: &TypeBounds) -> Result<TokenStream> {
        let bounds: Vec<_> = bounds
            .bounds
            .iter()
            .map(|x| self.print_expr(&x))
            .try_collect()?;
        Ok(quote!(#(#bounds)+ *))
    }
    pub fn print_type_value(&self, v: &Type) -> Result<TokenStream> {
        match v {
            Type::Function(f) => self.print_func_type(f),
            Type::Primitive(p) => self.print_primitive_type(*p),
            Type::Struct(s) => self.print_struct_type(s),
            Type::Structural(s) => self.print_unnamed_struct_type(s),
            Type::Expr(e) => self.print_expr(e),
            Type::ImplTraits(t) => self.print_impl_traits(t),
            Type::TypeBounds(t) => self.print_type_bounds(t),
            Type::Unit(_) => Ok(quote!(())),
            Type::Any(_) => Ok(quote!(dyn Any)),
            Type::Nothing(_) => Ok(quote!(!)),
            Type::Unknown(_) => Ok(quote!(_)),
            Type::Reference(r) => {
                let ty = self.print_type_value(&r.ty)?;
                if r.mutability == Some(true) {
                    Ok(quote!(&mut #ty))
                } else {
                    Ok(quote!(&#ty))
                }
            }
            Type::Value(v) => self.print_value(&v.value),
            _ => bail!("Not supported {:?}", v),
        }
    }

    pub fn print_path(&self, path: &Path) -> TokenStream {
        let segments: Vec<_> = path.segments.iter().map(|x| self.print_ident(x)).collect();
        quote!(#(#segments)::*)
    }
    fn print_parameter_path_segment(&self, segment: &ParameterPathSegment) -> Result<TokenStream> {
        let ident = self.print_ident(&segment.ident);
        if segment.args.is_empty() {
            return Ok(ident);
        }
        let args: Vec<_> = segment
            .args
            .iter()
            .map(|x| self.print_type_value(x))
            .try_collect()?;
        Ok(quote!(#ident::<#(#args), *>))
    }
    pub fn print_parameter_path(&self, path: &ParameterPath) -> Result<TokenStream> {
        let segments: Vec<_> = path
            .segments
            .iter()
            .map(|x| self.print_parameter_path_segment(x))
            .try_collect()?;
        Ok(quote!(#(#segments)::*))
    }
    pub fn print_locator(&self, pat: &Locator) -> Result<TokenStream> {
        Ok(match pat {
            Locator::Ident(n) => self.print_ident(n),
            Locator::Path(n) => self.print_path(n),
            Locator::ParameterPath(n) => self.print_parameter_path(n)?,
        })
    }

    pub fn print_expr_optimized(&self, node: &Expr) -> Result<TokenStream> {
        match node {
            Expr::Match(n) => self.print_match(n),
            Expr::If(n) => self.print_if(n),
            Expr::Block(n) => self.print_statement_chunk(&n.stmts),
            Expr::Value(v) if v.is_unit() => Ok(quote!()),
            _ => self.print_expr(node),
        }
    }
    pub fn print_expr(&self, node: &Expr) -> Result<TokenStream> {
        match node {
            Expr::Locator(loc) => self.print_locator(loc),
            Expr::Value(n) => self.print_value(n),
            Expr::Invoke(n) => self.print_invoke_expr(n),
            Expr::BinOp(op) => self.print_bin_op(op),
            Expr::Any(n) => self.print_any(n),
            Expr::Match(n) => self.print_match(n),
            Expr::If(n) => self.print_if(n),
            Expr::Block(n) => self.print_block(n),
            Expr::InitStruct(n) => self.print_struct_expr(n),
            Expr::Select(n) => self.print_select(n),
            Expr::Closured(n) => self.print_expr(&n.expr.get()),
            _ => bail!("Unable to serialize {:?}", node),
        }
    }
    pub fn print_item(&self, item: &Item) -> Result<TokenStream> {
        match item {
            Item::DefFunction(n) => self.print_function(&n.value, n.visibility),
            Item::DefType(n) => self.print_def_type(n),
            Item::DefStruct(n) => self.print_def_struct(n),
            Item::DefTrait(n) => self.print_def_trait(n),
            // Item::DefEnum(n) => self.print_def_enum(n),
            Item::Impl(n) => self.print_impl(n),
            Item::Module(n) => self.print_module(n),
            Item::Import(n) => self.print_import(n),
            Item::Expr(n) => self.print_expr(n),
            _ => bail!("Unable to serialize {:?}", item),
        }
    }
    pub fn print_file(&self, file: &File) -> Result<TokenStream> {
        let items = self.print_items_chunk(&file.module.items)?;
        Ok(quote!(#items))
    }
    pub fn print_tree(&self, node: &Tree) -> Result<TokenStream> {
        match node {
            Tree::Item(n) => self.print_item(n),
            Tree::Expr(n) => self.print_expr(n),
            Tree::File(n) => self.print_file(n),
        }
    }
}
