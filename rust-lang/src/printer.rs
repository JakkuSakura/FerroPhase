use common::Result;
use common::*;

use crate::{RawExpr, RawExprMacro, RawStmtMacro};
use common_lang::ast::*;
use common_lang::expr::*;
use common_lang::id::{ParameterPath, ParameterPathSegment};
use common_lang::ops::{BinOpKind, BuiltinFn, BuiltinFnName};
use common_lang::pat::{Pattern, PatternIdent};
use common_lang::ty::*;
use common_lang::value::*;
use proc_macro2::{Span, TokenStream};
use quote::*;

pub struct RustPrinter;

impl RustPrinter {
    pub fn print_ident(&self, i: &crate::id::Ident) -> TokenStream {
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
            BinOpKind::Any(ident) => self.print_ident(ident),
        }
    }
    pub fn print_invoke_expr(&self, invoke: &Invoke) -> Result<TokenStream> {
        match invoke.func.get() {
            Expr::Value(value) => match &*value {
                Value::BinOpKind(op) => {
                    let op = self.print_bin_op_kind(op);
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
    pub fn print_invoke_type(&self, invoke: &Invoke) -> Result<TokenStream> {
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
                let name = self.print_type_expr(&variant.name)?;
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
    pub fn print_assign(&self, assign: &StatementAssign) -> Result<TokenStream> {
        let target = self.print_expr(&assign.target)?;
        let value = self.print_expr(&assign.value)?;
        Ok(quote!(
            #target = #value;
        ))
    }
    pub fn print_for_each(&self, for_each: &StatementForEach) -> Result<TokenStream> {
        let name = self.print_ident(&for_each.variable);
        let iter = self.print_expr(&for_each.iterable)?;
        let body = self.print_block(&for_each.body)?;
        Ok(quote!(
            for #name in #iter
                #body
        ))
    }
    pub fn print_while(&self, while_: &StatementWhile) -> Result<TokenStream> {
        let cond = self.print_expr(&while_.cond)?;
        let body = self.print_block(&while_.body)?;
        Ok(quote!(
            while #cond
                #body
        ))
    }
    pub fn print_loop(&self, loop_: &StatementLoop) -> Result<TokenStream> {
        let body = self.print_block(&loop_.body)?;
        Ok(quote!(
            loop
                #body
        ))
    }
    pub fn print_statement(&self, stmt: &Statement) -> Result<TokenStream> {
        match stmt {
            Statement::Item(item) => self.print_item(item),
            Statement::Let(let_) => self.print_let(let_),
            Statement::SideEffect(stmt) => self.print_stmt_expr(&stmt),
            Statement::Expr(expr) => self.print_expr(expr),
            Statement::Any(any) => self.print_any(any),
            Statement::Assign(assign) => self.print_assign(assign),
            Statement::ForEach(for_each) => self.print_for_each(for_each),
            Statement::While(while_) => self.print_while(while_),
            Statement::Loop(loop_) => self.print_loop(loop_),
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
    pub fn print_block(&self, n: &Block) -> Result<TokenStream> {
        let chunk = self.print_statement_chunk(&n.stmts)?;
        Ok(quote!({
            #chunk
        }))
    }
    pub fn print_if(&self, cond: &If) -> Result<TokenStream> {
        let mut ts = vec![];

        for (i, c) in cond.cases.iter().enumerate() {
            let node = &c.cond;
            let co = self.print_expr(node)?;
            let node = &c.body;
            let ex = self.print_expr_optimized(node)?;
            if i == 0 {
                ts.push(quote!(
                    if #co {
                        #ex
                    }
                ));
            } else if i < cond.cases.len() - 1 {
                ts.push(quote!(
                    else if #co {
                        #ex
                    }
                ));
            } else {
                ts.push(quote!(
                    else {
                        #ex
                    }
                ));
            }
        }
        Ok(quote!(#(#ts)*))
    }
    pub fn print_match(&self, m: &Match) -> Result<TokenStream> {
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
        let stmts = self.print_expr_optimized(&func.body)?;
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
    pub fn print_invoke(&self, node: &Invoke) -> Result<TokenStream> {
        let fun = self.print_expr(&node.func.get())?;
        let args: Vec<_> = node
            .args
            .iter()
            .map(|x| self.print_expr(&x.get()))
            .try_collect()?;
        match &node.func.get() {
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
            "+" => quote!(#(#args) + *),
            "-" => quote!(#(#args) - *),
            "/" => quote!(#(#args) / *),
            "|" => quote!(#(#args) | *),
            "*" => {
                let mut result = vec![];
                for (i, a) in args.into_iter().enumerate() {
                    if i != 0 {
                        result.push(quote!(*));
                    }
                    result.push(a);
                }
                quote!(#(#result)*)
            }
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
    pub fn print_ref(&self, n: &Reference) -> Result<TokenStream> {
        let referee = self.print_expr(&n.referee)?;
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
    pub fn print_return_type(&self, node: &TypeValue) -> Result<TokenStream> {
        if matches!(node, TypeValue::Unit(_)) {
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
    pub fn print_select(&self, select: &Select) -> Result<TokenStream> {
        let obj = self.print_expr(&select.obj)?;
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
        let name = self.print_type_expr(&impl_.self_ty)?;
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
    pub fn print_struct_expr(&self, s: &StructExpr) -> Result<TokenStream> {
        let name = self.print_type_expr(&s.name)?;
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

    pub fn print_any(&self, n: &crate::utils::anybox::AnyBox) -> Result<TokenStream> {
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
            Value::Expr(e) => self.print_expr(e),
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
            TypePrimitive::Int(IntType::I64) => Ok(quote!(i64)),
            TypePrimitive::Int(IntType::U64) => Ok(quote!(u64)),
            TypePrimitive::Int(IntType::I32) => Ok(quote!(i32)),
            TypePrimitive::Int(IntType::U32) => Ok(quote!(u32)),
            TypePrimitive::Int(IntType::I16) => Ok(quote!(i16)),
            TypePrimitive::Int(IntType::U16) => Ok(quote!(u16)),
            TypePrimitive::Int(IntType::I8) => Ok(quote!(i8)),
            TypePrimitive::Int(IntType::U8) => Ok(quote!(u8)),
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
            .map(|x| self.print_type_expr(&x))
            .try_collect()?;
        Ok(quote!(#(#bounds)+ *))
    }
    pub fn print_type_value(&self, v: &TypeValue) -> Result<TokenStream> {
        match v {
            TypeValue::Function(f) => self.print_func_type(f),
            TypeValue::Primitive(p) => self.print_primitive_type(*p),
            TypeValue::Struct(s) => self.print_struct_type(s),
            TypeValue::Structural(s) => self.print_unnamed_struct_type(s),
            TypeValue::Expr(e) => self.print_type_expr(e),
            TypeValue::ImplTraits(t) => self.print_impl_traits(t),
            TypeValue::TypeBounds(t) => self.print_type_bounds(t),
            TypeValue::Unit(_) => Ok(quote!(())),
            TypeValue::Any(_) => Ok(quote!(dyn Any)),
            TypeValue::Nothing(_) => Ok(quote!(!)),
            TypeValue::Reference(r) => {
                let ty = self.print_type_value(&r.ty)?;
                if r.mutability == Some(true) {
                    Ok(quote!(&mut #ty))
                } else {
                    Ok(quote!(&#ty))
                }
            }
            TypeValue::Value(v) => self.print_value(&v.value),
            _ => bail!("Not supported {:?}", v),
        }
    }

    pub fn print_path(&self, path: &crate::id::Path) -> TokenStream {
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
    pub fn print_locator(&self, pat: &crate::id::Locator) -> Result<TokenStream> {
        Ok(match pat {
            crate::id::Locator::Ident(n) => self.print_ident(n),
            crate::id::Locator::Path(n) => self.print_path(n),
            crate::id::Locator::ParameterPath(n) => self.print_parameter_path(n)?,
        })
    }
    pub fn print_type_bin_op(&self, op: &TypeBinOp) -> Result<TokenStream> {
        match op {
            TypeBinOp::Add { left, right } => {
                let left = self.print_type_expr(&left)?;
                let right = self.print_type_expr(&right)?;
                Ok(quote!(#left + #right))
            }
            TypeBinOp::Sub { left, right } => {
                let left = self.print_type_expr(&left)?;
                let right = self.print_type_expr(&right)?;
                Ok(quote!(#left - #right))
            }
            #[allow(unreachable_patterns)]
            _ => bail!("Unable to serialize {:?}", op),
        }
    }
    pub fn print_type_expr(&self, node: &TypeExpr) -> Result<TokenStream> {
        match node {
            TypeExpr::Locator(n) => self.print_locator(n),
            TypeExpr::Value(n) => self.print_type_value(n),
            TypeExpr::Invoke(n) => self.print_invoke_type(n),
            TypeExpr::BinOp(op) => self.print_type_bin_op(op),
            _ => bail!("Unable to serialize {:?}", node),
        }
    }
    pub fn print_stmt_expr(&self, node: &SideEffect) -> Result<TokenStream> {
        let expr = self.print_expr(&node.expr)?;
        Ok(quote!(#expr;))
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
            Expr::Locator(crate::id::Locator::Ident(n)) => Ok(self.print_ident(n)),
            Expr::Locator(crate::id::Locator::Path(n)) => Ok(self.print_path(n)),
            Expr::Value(n) => self.print_value(n),
            Expr::Invoke(n) => self.print_invoke_expr(n),
            Expr::Any(n) => self.print_any(n),
            Expr::Match(n) => self.print_match(n),
            Expr::If(n) => self.print_if(n),
            Expr::Block(n) => self.print_block(n),
            Expr::Struct(n) => self.print_struct_expr(n),
            Expr::Select(n) => self.print_select(n),
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
