use crate::printer::RustPrinter;
use fp_core::ast::{AstAttrMeta, AstAttrStyle, AstAttribute};
use fp_core::error::Result;
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::quote;

impl RustPrinter {
    pub fn print_attr_style(&self, style: &AstAttrStyle) -> Result<TokenStream> {
        let style = match style {
            AstAttrStyle::Outer => quote! {},
            AstAttrStyle::Inner => quote! { ! },
        };
        Ok(style)
    }
    pub fn print_attr_meta(&self, meta: &AstAttrMeta) -> Result<TokenStream> {
        let meta = match meta {
            AstAttrMeta::Path(path) => {
                let path = self.print_path(&path);
                quote! { #path }
            }
            AstAttrMeta::List(list) => {
                let name = self.print_path(&list.name);
                let items: Vec<TokenStream> = list
                    .items
                    .iter()
                    .map(|item| self.print_attr_meta(item))
                    .try_collect()?;

                quote! { #name(#(#items),*) }
            }
            AstAttrMeta::NameValue(nv) => {
                let name = self.print_path(&nv.name);
                let value = self.print_expr(&nv.value)?;
                quote! { #name = #value }
            }
        };
        Ok(meta)
    }
    pub fn print_attr(&self, attr: &AstAttribute) -> Result<TokenStream> {
        let style = self.print_attr_style(&attr.style)?;
        let meta = self.print_attr_meta(&attr.meta)?;
        Ok(quote! {
            #[#style(#meta)]
        })
    }
    pub fn print_attrs(&self, attrs: &[AstAttribute]) -> Result<TokenStream> {
        let attrs: Vec<TokenStream> = attrs
            .iter()
            .map(|attr| self.print_attr(attr))
            .try_collect()?;
        Ok(quote! { #(#attrs)* })
    }
}
