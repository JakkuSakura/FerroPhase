use eyre::{bail, ContextCompat, Result};
use itertools::Itertools;
use lang_core::ast::{
    AstItem, ItemDefConst, ItemDefFunction, ItemDefStatic, ItemDefStruct, ItemDefTrait,
    ItemDefType, ItemImpl,
};
use proc_macro2::TokenStream;
use quote::quote;

use crate::printer::RustPrinter;

impl RustPrinter {
    pub fn print_items_chunk(&self, items: &[AstItem]) -> Result<TokenStream> {
        let mut stmts = vec![];
        for item in items {
            let item = self.print_item(item)?;
            stmts.push(item);
        }
        Ok(quote!(#(#stmts) *))
    }

    pub fn print_def_struct(&self, def: &ItemDefStruct) -> Result<TokenStream> {
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
    pub fn print_def_type(&self, def: &ItemDefType) -> Result<TokenStream> {
        let name = self.print_ident(&def.name);
        let ty = self.print_type_value(&def.value)?;
        return Ok(quote!(
            type #name = t!{ #ty };
        ));
    }
    pub fn print_def_const(&self, def: &ItemDefConst) -> Result<TokenStream> {
        let name = self.print_ident(&def.name);
        let ty = self.print_type_value(&def.ty.as_ref().context("No type")?.clone())?;
        let value = self.print_expr(&def.value)?;
        return Ok(quote!(
            const #name: #ty = #value;
        ));
    }
    pub fn print_def_static(&self, def: &ItemDefStatic) -> Result<TokenStream> {
        let name = self.print_ident(&def.name);
        let ty = self.print_type_value(&def.ty)?;
        let value = self.print_expr(&def.value)?;
        return Ok(quote!(x
            static #name: #ty = #value;
        ));
    }
    pub fn print_def_trait(&self, def: &ItemDefTrait) -> Result<TokenStream> {
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

    pub fn print_impl(&self, impl_: &ItemImpl) -> Result<TokenStream> {
        let name = self.print_expr(&impl_.self_ty)?;
        let methods = self.print_items_chunk(&impl_.items)?;
        Ok(quote!(
            impl #name {
                #methods
            }
        ))
    }
    pub fn print_def_function(&self, func: &ItemDefFunction) -> Result<TokenStream> {
        let attrs = self.print_attrs(&func.attrs)?;
        let func = self.print_function(&func.sig, &func.body, func.visibility)?;
        Ok(quote!(
            #attrs
            #func
        ))
    }
    pub fn print_item(&self, item: &AstItem) -> Result<TokenStream> {
        match item {
            AstItem::DefFunction(n) => self.print_def_function(n),
            AstItem::DefType(n) => self.print_def_type(n),
            AstItem::DefStruct(n) => self.print_def_struct(n),
            AstItem::DefTrait(n) => self.print_def_trait(n),
            AstItem::DefConst(n) => self.print_def_const(n),
            AstItem::DefStatic(n) => self.print_def_static(n),
            // AstItem::DefEnum(n) => self.print_def_enum(n),
            AstItem::Impl(n) => self.print_impl(n),
            AstItem::Module(n) => self.print_module(n),
            AstItem::Import(n) => self.print_import(n),
            AstItem::Expr(n) => self.print_expr(n),
            _ => bail!("Unable to serialize {:?}", item),
        }
    }
}
