use common::*;
use itertools::Itertools;
use lang_core::ast::*;
use lang_core::id::{Ident, Locator, ParameterPath, ParameterPathSegment, Path};
use lang_core::ops::{BinOpKind, BuiltinFn, BuiltinFnName};
use lang_core::pat::{Pattern, PatternIdent};
use lang_core::utils::anybox::AnyBox;
use proc_macro2::{Span, TokenStream};
use quote::*;

use crate::{RawExpr, RawExprMacro, RawStmtMacro};

mod attr;
mod expr;
mod item;
pub mod rustfmt;

#[derive(Debug, Clone, Eq, PartialEq, Copy)]
pub struct RustPrinter {
    pub rustfmt: bool,
}

impl RustPrinter {
    pub fn new() -> Self {
        Self { rustfmt: false }
    }
    pub fn set_rustfmt(&mut self, rustfmt: bool) {
        self.rustfmt = rustfmt;
    }
    pub fn maybe_rustfmt_token_stream(&self, code: &TokenStream) -> Result<String> {
        self.maybe_rustfmt(&code.to_string())
    }
    pub fn maybe_rustfmt(&self, code: &str) -> Result<String> {
        if self.rustfmt {
            if let Ok(ok) = rustfmt::format_code(code) {
                return Ok(ok);
            }
        }

        Ok(code.to_string())
    }
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
    pub fn print_trait_bound(&self, n: &ItemDefTrait) -> Result<TokenStream> {
        let name = self.print_ident(&n.name);
        let bounds = self.print_type_bounds(&n.bounds)?;
        Ok(quote!(
            #name: #bounds
        ))
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
    pub fn print_invoke_target(&self, target: &ExprInvokeTarget) -> Result<TokenStream> {
        match target {
            ExprInvokeTarget::Function(locator) => self.print_locator(locator),
            ExprInvokeTarget::Type(t) => self.print_type_value(t),
            ExprInvokeTarget::Method(select) => self.print_select(select),
            ExprInvokeTarget::Closure(fun) => self.print_func_value(fun),
            ExprInvokeTarget::BinOp(op) => Ok(self.print_bin_op_kind(op)),
            ExprInvokeTarget::Expr(expr) => self.print_expr(expr),
        }
    }
    pub fn print_invoke_type(&self, invoke: &ExprInvoke) -> Result<TokenStream> {
        let fun = self.print_invoke_target(&invoke.target)?;
        let args: Vec<_> = invoke
            .args
            .iter()
            .map(|x| self.print_expr(&x.get()))
            .try_collect()?;
        Ok(quote!(
            #fun::<#(#args), *>
        ))
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
    pub fn print_vis(&self, vis: Visibility) -> TokenStream {
        match vis {
            Visibility::Public => quote!(pub),
            Visibility::Private => quote!(),
            Visibility::Inherited => quote!(),
        }
    }

    pub fn print_function(
        &self,
        sig: &FunctionSignature,
        body: &AstExpr,
        vis: Visibility,
    ) -> Result<TokenStream> {
        let name = if let Some(name) = &sig.name {
            self.print_ident(name)
        } else {
            quote!()
        };
        let ret_type = &sig.ret_ty;
        let ret = self.print_return_type(ret_type)?;
        let param_names: Vec<_> = sig
            .params
            .iter()
            .map(|x| self.print_ident(&x.name))
            .collect();
        let param_types: Vec<_> = sig
            .params
            .iter()
            .map(|x| self.print_type_value(&x.ty))
            .try_collect()?;
        let stmts = self.print_expr_optimized(&body.get())?;
        let gg;
        if !sig.generics_params.is_empty() {
            let gt: Vec<_> = sig
                .generics_params
                .iter()
                .map(|x| self.print_ident(&x.name))
                .collect();
            let gb: Vec<_> = sig
                .generics_params
                .iter()
                .map(|x| self.print_type_bounds(&x.bounds))
                .try_collect()?;
            gg = quote!(<#(#gt: #gb), *>)
        } else {
            gg = quote!();
        }
        let vis = self.print_vis(vis);
        // let attrs = self.print_attrs(&func.attrs)?;
        return Ok(quote!(
            // #attrs
            #vis fn #name #gg(#(#param_names: #param_types), *) #ret {
                #stmts
            }
        ));
    }
    pub fn print_value_function(
        &self,
        fun: &ValueFunction,
        vis: Visibility,
    ) -> Result<TokenStream> {
        let sig = &fun.sig;
        let body = &fun.body;
        self.print_function(sig, body, vis)
    }
    pub fn print_func_type_param(&self, param: &FunctionParam) -> Result<TokenStream> {
        let name = self.print_ident(&param.name);
        let ty = self.print_type_value(&param.ty)?;
        Ok(quote!(#name: #ty))
    }
    pub fn print_return_type(&self, node: &AstType) -> Result<TokenStream> {
        if matches!(node, AstType::Unit(_)) {
            return Ok(quote!());
        }
        let ty = self.print_type_value(&node)?;
        Ok(quote!(-> #ty))
    }
    pub fn print_func_value(&self, fun: &ValueFunction) -> Result<TokenStream> {
        self.print_value_function(fun, Visibility::Private)
    }
    pub fn print_func_type(&self, fun: &TypeFunction) -> Result<TokenStream> {
        let args: Vec<_> = fun
            .params
            .iter()
            .map(|x| self.print_type_value(x))
            .try_collect()?;
        let node = &fun.ret_ty;
        let ret = self.print_return_type(node)?;
        Ok(quote!(
            fn(#(#args), *) #ret
        ))
    }
    pub fn print_module(&self, m: &AstModule) -> Result<TokenStream> {
        let stmts = self.print_items_chunk(&m.items)?;

        let mod_name = format_ident!("{}", m.name.as_str());
        Ok(quote!(
            pub mod #mod_name {
                #stmts
            }
        ))
    }
    pub fn print_import(&self, node: &ItemImport) -> Result<TokenStream> {
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
    pub fn print_list_expr(&self, n: &[AstExpr]) -> Result<TokenStream> {
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
    pub fn print_type_value(&self, v: &AstType) -> Result<TokenStream> {
        match v {
            AstType::Function(f) => self.print_func_type(f),
            AstType::Primitive(p) => self.print_primitive_type(*p),
            AstType::Struct(s) => self.print_struct_type(s),
            AstType::Structural(s) => self.print_unnamed_struct_type(s),
            AstType::Expr(e) => self.print_expr(e),
            AstType::ImplTraits(t) => self.print_impl_traits(t),
            AstType::TypeBounds(t) => self.print_type_bounds(t),
            AstType::Unit(_) => Ok(quote!(())),
            AstType::Any(_) => Ok(quote!(dyn Any)),
            AstType::Nothing(_) => Ok(quote!(!)),
            AstType::Unknown(_) => Ok(quote!(_)),
            AstType::Reference(r) => {
                let ty = self.print_type_value(&r.ty)?;
                if r.mutability == Some(true) {
                    Ok(quote!(&mut #ty))
                } else {
                    Ok(quote!(&#ty))
                }
            }
            AstType::Value(v) => self.print_value(&v.value),
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

    pub fn print_file(&self, file: &AstFile) -> Result<TokenStream> {
        let items = self.print_items_chunk(&file.items)?;
        Ok(quote!(#items))
    }
    pub fn print_node(&self, node: &AstNode) -> Result<TokenStream> {
        match node {
            AstNode::Item(n) => self.print_item(n),
            AstNode::Expr(n) => self.print_expr(n),
            AstNode::File(n) => self.print_file(n),
        }
    }
}

impl AstSerializer for RustPrinter {
    fn serialize_node(&self, node: &AstNode) -> Result<String> {
        self.print_node(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_expr(&self, node: &AstExpr) -> Result<String> {
        self.print_expr(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_invoke(&self, node: &ExprInvoke) -> Result<String> {
        self.print_invoke(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_item(&self, node: &AstItem) -> Result<String> {
        self.print_item(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_block(&self, node: &ExprBlock) -> Result<String> {
        self.print_block(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_file(&self, node: &AstFile) -> Result<String> {
        self.print_file(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }
    fn serialize_module(&self, node: &AstModule) -> Result<String> {
        self.print_module(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_value(&self, node: &Value) -> Result<String> {
        self.print_value(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_type(&self, node: &AstType) -> Result<String> {
        self.print_type_value(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_stmt(&self, node: &BlockStmt) -> Result<String> {
        self.print_statement(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_value_function(&self, node: &ValueFunction) -> Result<String> {
        self.print_value_function(node, Visibility::Private)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }
    fn serialize_def_function(&self, node: &ItemDefFunction) -> Result<String> {
        self.print_def_function(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }
}
