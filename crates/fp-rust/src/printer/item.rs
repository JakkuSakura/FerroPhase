use eyre::ContextCompat;
use fp_core::ast::{
    FunctionSignature, GenericParam, Item, ItemDeclConst, ItemDeclFunction, ItemDeclStatic,
    ItemDeclType, ItemDefConst, ItemDefEnum, ItemDefFunction, ItemDefStatic, ItemDefStruct,
    ItemDefStructural, ItemDefTrait, ItemDefType, ItemImpl, ItemKind, Ty,
};
use fp_core::{Error, Result};
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
        Ok(quote!(#(#stmts)*))
    }

    pub fn print_def_struct(&self, def: &ItemDefStruct) -> Result<TokenStream> {
        let vis = self.print_vis(&def.visibility);
        let name = self.print_ident(&def.name);
        let generics = self.print_generics_params(&def.value.generics_params)?;
        let fields: Vec<_> = def
            .value
            .fields
            .iter()
            .map(|x| self.print_field(&x))
            .try_collect()?;
        Ok(quote!(
            #vis struct #name #generics {
                #(#fields), *
            }
        ))
    }
    pub fn print_def_structural(&self, def: &ItemDefStructural) -> Result<TokenStream> {
        let vis = self.print_vis(&def.visibility);
        let name = self.print_ident(&def.name);
        let fields: Vec<_> = def
            .value
            .fields
            .iter()
            .map(|field| self.print_field(field))
            .try_collect()?;
        Ok(quote!(
            #vis struct #name {
                #(#fields), *
            }
        ))
    }
    pub fn print_def_type(&self, def: &ItemDefType) -> Result<TokenStream> {
        let vis = self.print_vis(&def.visibility);
        let name = self.print_ident(&def.name);
        let ty = self.print_type(&def.value)?;
        return Ok(quote!(
            #vis type #name = t!{ #ty };
        ));
    }
    pub fn print_def_enum(&self, def: &ItemDefEnum) -> Result<TokenStream> {
        let vis = self.print_vis(&def.visibility);
        let name = self.print_ident(&def.name);
        let generics = self.print_generics_params(&def.value.generics_params)?;
        let variants: Vec<_> = def
            .value
            .variants
            .iter()
            .map(|variant| {
                let variant_name = self.print_ident(&variant.name);
                let base = match &variant.value {
                    Ty::Any(_) | Ty::Unit(_) => quote!(#variant_name),
                    Ty::Tuple(tuple) => {
                        if tuple.types.is_empty() {
                            quote!(#variant_name)
                        } else {
                            let elems: Vec<_> = tuple
                                .types
                                .iter()
                                .map(|ty| self.print_type(ty))
                                .try_collect()?;
                            quote!(#variant_name(#(#elems),*))
                        }
                    }
                    Ty::Structural(structural) => {
                        let fields: Vec<_> = structural
                            .fields
                            .iter()
                            .map(|field| {
                                let field_name = self.print_ident(&field.name);
                                let field_ty = self.print_type(&field.value)?;
                                Ok::<TokenStream, Error>(quote!(#field_name: #field_ty))
                            })
                            .try_collect()?;
                        quote!(#variant_name { #(#fields),* })
                    }
                    ty => {
                        let ty_tokens = self.print_type(ty)?;
                        quote!(#variant_name(#ty_tokens))
                    }
                };
                let tokens = if let Some(expr) = &variant.discriminant {
                    let expr_tokens = self.print_expr(expr.as_ref())?;
                    quote!(#base = #expr_tokens)
                } else {
                    base
                };
                Ok::<TokenStream, Error>(tokens)
            })
            .try_collect()?;

        Ok(quote!(
            #vis enum #name #generics {
                #(#variants), *
            }
        ))
    }
    pub fn print_def_const(&self, def: &ItemDefConst) -> Result<TokenStream> {
        let vis = self.print_vis(&def.visibility);
        let name = self.print_ident(&def.name);
        let ty = self.print_type(&def.ty.as_ref().context("No type")?.clone())?;
        let value = self.print_expr(&def.value)?;
        return Ok(quote!(
            #vis const #name: #ty = #value;
        ));
    }
    pub fn print_def_static(&self, def: &ItemDefStatic) -> Result<TokenStream> {
        let vis = self.print_vis(&def.visibility);
        let name = self.print_ident(&def.name);
        let ty = self.print_type(&def.ty)?;
        let value = self.print_expr(&def.value)?;
        return Ok(quote!(
            #vis static #name: #ty = #value;
        ));
    }
    pub fn print_decl_const(&self, decl: &ItemDeclConst) -> Result<TokenStream> {
        let name = self.print_ident(&decl.name);
        let ty = if let Some(ty) = decl.ty_annotation.as_ref() {
            self.print_type(ty)?
        } else {
            self.print_type(&decl.ty)?
        };
        Ok(quote!(const #name: #ty;))
    }
    pub fn print_decl_static(&self, decl: &ItemDeclStatic) -> Result<TokenStream> {
        let name = self.print_ident(&decl.name);
        let ty = if let Some(ty) = decl.ty_annotation.as_ref() {
            self.print_type(ty)?
        } else {
            self.print_type(&decl.ty)?
        };
        Ok(quote!(static #name: #ty;))
    }
    pub fn print_decl_type(&self, decl: &ItemDeclType) -> Result<TokenStream> {
        let name = self.print_ident(&decl.name);
        let bounds_tokens = self.print_type_bounds(&decl.bounds)?;
        if decl.bounds.bounds.is_empty() {
            Ok(quote!(type #name;))
        } else {
            Ok(quote!(type #name: #bounds_tokens;))
        }
    }
    pub fn print_decl_function(&self, decl: &ItemDeclFunction) -> Result<TokenStream> {
        self.print_function_decl(&decl.sig)
    }
    pub fn print_def_trait(&self, def: &ItemDefTrait) -> Result<TokenStream> {
        let vis = self.print_vis(&def.visibility);
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
        let generics = self.print_generics_params(&impl_.generics_params)?;
        let trait_part = if let Some(trait_ty) = &impl_.trait_ty {
            let trait_tokens = self.print_locator(trait_ty)?;
            quote!(#trait_tokens for)
        } else {
            quote!()
        };
        Ok(quote!(
            impl #generics #trait_part #name {
                #methods
            }
        ))
    }
    pub fn print_def_function(&self, func: &ItemDefFunction) -> Result<TokenStream> {
        let attrs = self.print_attrs(&func.attrs)?;
        let func = self.print_function(&func.sig, &func.body, &func.visibility)?;
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
            ItemKind::DefStructural(n) => self.print_def_structural(n),
            ItemKind::DefEnum(n) => self.print_def_enum(n),
            ItemKind::DefTrait(n) => self.print_def_trait(n),
            ItemKind::DefConst(n) => self.print_def_const(n),
            ItemKind::DefStatic(n) => self.print_def_static(n),
            ItemKind::DeclConst(n) => self.print_decl_const(n),
            ItemKind::DeclStatic(n) => self.print_decl_static(n),
            ItemKind::DeclType(n) => self.print_decl_type(n),
            ItemKind::DeclFunction(n) => self.print_decl_function(n),
            ItemKind::Impl(n) => self.print_impl(n),
            ItemKind::Module(n) => self.print_module(n),
            ItemKind::Import(n) => self.print_import(n),
            ItemKind::Macro(_n) => Ok(quote!()),
            ItemKind::Expr(n) => self.print_expr(n),
            ItemKind::Any(n) => self.print_any(n),
        }
    }
}

impl RustPrinter {
    fn print_function_decl(&self, sig: &FunctionSignature) -> Result<TokenStream> {
        let name_ident = sig
            .name
            .as_ref()
            .context("function declaration missing name")?;
        let name = self.print_ident(name_ident);

        let mut params: Vec<TokenStream> = Vec::new();
        if let Some(receiver) = &sig.receiver {
            params.push(self.print_receiver(receiver)?);
        }
        for param in &sig.params {
            params.push(self.print_func_type_param(param)?);
        }

        let generics = if sig.generics_params.is_empty() {
            quote!()
        } else {
            let gt: Vec<_> = sig
                .generics_params
                .iter()
                .map(|param| self.print_ident(&param.name))
                .collect();
            let gb: Vec<_> = sig
                .generics_params
                .iter()
                .map(|param| self.print_type_bounds(&param.bounds))
                .try_collect()?;
            quote!(<#(#gt: #gb), *>)
        };

        let ret = self.print_return_type(sig.ret_ty.as_ref())?;
        let params_iter = params.iter();

        let const_kw = if sig.is_const { quote!(const) } else { quote!() };
        Ok(quote!(#const_kw fn #name #generics(#(#params_iter),*) #ret;))
    }

    fn print_generics_params(&self, params: &[GenericParam]) -> Result<TokenStream> {
        if params.is_empty() {
            return Ok(quote!());
        }
        let idents: Vec<_> = params.iter().map(|p| self.print_ident(&p.name)).collect();
        let bounds: Vec<_> = params
            .iter()
            .map(|p| self.print_type_bounds(&p.bounds))
            .try_collect()?;
        Ok(quote!(<#(#idents: #bounds), *>))
    }
}
