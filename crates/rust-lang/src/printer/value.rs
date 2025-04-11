use crate::printer::RustPrinter;
use eyre::bail;
use itertools::Itertools;
use lang_core::ast::{
    AstExpr, AstValue, ValueBool, ValueChar, ValueDecimal, ValueInt, ValueList, ValueString,
    ValueStruct, ValueUndefined, ValueUnit,
};
use proc_macro2::{Span, TokenStream};
use quote::quote;

impl RustPrinter {
    pub fn print_undefined(&self, _n: &ValueUndefined) -> eyre::Result<TokenStream> {
        Ok(quote!(undefined))
    }
    pub fn print_value(&self, v: &AstValue) -> eyre::Result<TokenStream> {
        let v = match v {
            AstValue::Function(f) => self.print_func_value(f)?,
            AstValue::Int(i) => self.print_int(i)?,
            AstValue::Bool(b) => self.print_bool(b)?,
            AstValue::Decimal(d) => self.print_decimal(d)?,
            AstValue::Char(c) => self.print_char(c)?,
            AstValue::String(s) => self.print_string(s)?,
            AstValue::List(l) => self.print_list_value(l)?,
            AstValue::Unit(u) => self.print_unit(u)?,
            AstValue::Type(t) => self.print_type(t)?,
            AstValue::Struct(s) => self.print_struct_value(s)?,
            AstValue::Any(n) => self.print_any(n)?,
            AstValue::BinOpKind(op) => self.print_bin_op_kind(op),
            AstValue::Expr(e) => self.print_expr(&e.get())?,
            AstValue::Undefined(u) => self.print_undefined(u)?,
            AstValue::None(_) => quote!(None),
            AstValue::Some(s) => {
                let s = self.print_value(&s.value)?;
                quote!(Some(#s))
            }
            AstValue::Option(o) => match o.value {
                Some(ref v) => {
                    let v = self.print_value(v)?;
                    quote!(Some(#v))
                }
                None => quote!(None),
            },
            _ => bail!("Not supported {:?}", v),
        };
        Ok(v)
    }
    pub fn print_struct_value(&self, s: &ValueStruct) -> eyre::Result<TokenStream> {
        let name = self.print_ident(&s.ty.name);
        let kwargs: Vec<_> = s
            .structural
            .fields
            .iter()
            .map(|x| self.print_field_value(x))
            .try_collect()?;
        Ok(quote!(#name { #(#kwargs), * }))
    }

    pub fn print_int(&self, n: &ValueInt) -> eyre::Result<TokenStream> {
        let n = syn::LitInt::new(&n.value.to_string(), Span::call_site());
        Ok(quote!(#n))
    }
    pub fn print_bool(&self, n: &ValueBool) -> eyre::Result<TokenStream> {
        let n = n.value;
        Ok(quote!(#n))
    }
    pub fn print_decimal(&self, n: &ValueDecimal) -> eyre::Result<TokenStream> {
        let n = syn::LitFloat::new(&n.value.to_string(), Span::call_site());
        Ok(quote!(#n))
    }
    pub fn print_char(&self, n: &ValueChar) -> eyre::Result<TokenStream> {
        let n = n.value;
        Ok(quote!(#n))
    }
    pub fn print_string(&self, n: &ValueString) -> eyre::Result<TokenStream> {
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
    pub fn print_list_expr(&self, n: &[AstExpr]) -> eyre::Result<TokenStream> {
        let n: Vec<_> = n.iter().map(|x| self.print_expr(x)).try_collect()?;
        Ok(quote!(vec![#(#n),*]))
    }
    pub fn print_list_value(&self, n: &ValueList) -> eyre::Result<TokenStream> {
        let n: Vec<_> = n.values.iter().map(|x| self.print_value(x)).try_collect()?;
        Ok(quote!(vec![#(#n),*]))
    }
    pub fn print_unit(&self, _n: &ValueUnit) -> eyre::Result<TokenStream> {
        Ok(quote!(()))
    }
}
