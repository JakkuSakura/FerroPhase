use eyre::ContextCompat;
use fp_core::ast::{
    Item, ItemDefConst, ItemDefFunction, ItemDefStatic, ItemDefStruct, ItemDefTrait, ItemDefType,
    ItemImpl,
};
use fp_core::{bail, Result};
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::quote;

use crate::printer::RustPrinter;

impl RustPrinter {
    pub fn print_items_chunk(&self, items: &[Item]) -> Result<TokenStream> {
        let mut stmts = vec![];
        for item in items {
            let item = self.print_item(item)?;
            stmts.push(item);
        }
        Ok(quote!(#(#stmts) *))
    }

    pub fn print_def_struct(&self, def: &ItemDefStruct) -> Result<TokenStream> {
        let vis = self.print_vis(def.visibility);
        let name = self.print_ident(&def.name);
        let fields: Vec<_> = def
            .value
            .fields
            .iter()
            .map(|x| self.print_field(&x))
            .try_collect()?;
        Ok(quote!(
            #vis struct #name {
                #(#fields), *
            }
        ))
    }
    pub fn print_def_type(&self, def: &ItemDefType) -> Result<TokenStream> {
        let vis = self.print_vis(def.visibility);
        let name = self.print_ident(&def.name);
        let ty = self.print_type(&def.value)?;
        return Ok(quote!(
            #vis type #name = t!{ #ty };
        ));
    }
    pub fn print_def_const(&self, def: &ItemDefConst) -> Result<TokenStream> {
        let vis = self.print_vis(def.visibility);
        let name = self.print_ident(&def.name);
        let ty = self.print_type(&def.ty.as_ref().context("No type")?.clone())?;
        let value = self.print_expr(&def.value)?;
        return Ok(quote!(
            #vis const #name: #ty = #value;
        ));
    }
    pub fn print_def_static(&self, def: &ItemDefStatic) -> Result<TokenStream> {
        let vis = self.print_vis(def.visibility);
        let name = self.print_ident(&def.name);
        let ty = self.print_type(&def.ty)?;
        let value = self.print_expr(&def.value)?;
        return Ok(quote!(
            #vis static #name: #ty = #value;
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
    pub fn print_item(&self, item: &Item) -> Result<TokenStream> {
        match item.kind() {
            ItemKind::DefFunction(n) => self.print_def_function(n),
            ItemKind::DefType(n) => self.print_def_type(n),
            ItemKind::DefStruct(n) => self.print_def_struct(n),
            ItemKind::DefTrait(n) => self.print_def_trait(n),
            ItemKind::DefConst(n) => self.print_def_const(n),
            ItemKind::DefStatic(n) => self.print_def_static(n),
            // ItemKind::DefEnum(n) => self.print_def_enum(n),
            ItemKind::Impl(n) => self.print_impl(n),
            ItemKind::Module(n) => self.print_module(n),
            ItemKind::Import(n) => self.print_import(n),
            ItemKind::Expr(n) => self.print_expr(n),
            _ => bail!("Unable to serialize {:?}", item),
        }
    }
}
