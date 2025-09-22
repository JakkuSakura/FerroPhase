use eyre;
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::*;

use crate::{RawExpr, RawExprMacro, RawStmtMacro};
use fp_core::{Error, Result};
use fp_core::ast::*;
use fp_core::printer::AstSerializerConfig;
use fp_core::bail;
use fp_core::id::{Ident, Locator, ParameterPath, ParameterPathSegment, Path};
use fp_core::ops::{BuiltinFn, BuiltinFnName};
use fp_core::pat::{Pattern, PatternIdent};
use fp_core::utils::anybox::AnyBox;

mod attr;
mod expr;
mod item;
mod ty;
mod value;

pub mod rustfmt;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RustPrinter {
    pub rustfmt: bool,
    pub config: AstSerializerConfig,
}

impl RustPrinter {
    pub fn new() -> Self {
        Self { 
            rustfmt: false,
            config: AstSerializerConfig::standard(),
        }
    }
    
    pub fn with_config(config: AstSerializerConfig) -> Self {
        Self {
            rustfmt: false,
            config,
        }
    }
    
    pub fn config(&self) -> &AstSerializerConfig {
        &self.config
    }
    pub fn set_rustfmt(&mut self, rustfmt: bool) {
        self.rustfmt = rustfmt;
    }
    pub fn maybe_rustfmt_token_stream(&self, code: &TokenStream) -> fp_core::error::Result<String> {
        self.maybe_rustfmt(&code.to_string())
    }
    pub fn maybe_rustfmt(&self, code: &str) -> fp_core::error::Result<String> {
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
            Pattern::Type(type_) => {
                let pattern = self.print_pattern(&type_.pat)?;
                let ty = self.print_type(&type_.ty)?;
                Ok(quote!(#pattern: #ty))
            }
            _ => todo!("pattern not implemented: {:?}", pat),
        }
    }

    pub fn print_vis(&self, vis: Visibility) -> TokenStream {
        match vis {
            Visibility::Public => quote!(pub),
            Visibility::Private => quote!(),
            Visibility::Inherited => quote!(),
        }
    }
    pub fn print_receiver(&self, receiver: &FunctionParamReceiver) -> Result<TokenStream> {
        match receiver {
            FunctionParamReceiver::Implicit => bail!("Implicit receiver not supported"),
            FunctionParamReceiver::Value => Ok(quote!(self)),
            FunctionParamReceiver::MutValue => Ok(quote!(mut self)),
            FunctionParamReceiver::Ref => Ok(quote!(&self)),
            FunctionParamReceiver::RefStatic => Ok(quote!(&'static self)),
            FunctionParamReceiver::RefMut => Ok(quote!(&mut self)),
            FunctionParamReceiver::RefMutStatic => Ok(quote!(&'static mut self)),
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
        let ret = self.print_return_type(sig.ret_ty.as_ref())?;
        let receiver = if let Some(receiver) = &sig.receiver {
            let rec = self.print_receiver(receiver)?;
            quote!(#rec,)
        } else {
            quote!()
        };
        let param_names: Vec<_> = sig
            .params
            .iter()
            .map(|x| self.print_ident(&x.name))
            .collect();
        let param_types: Vec<_> = sig
            .params
            .iter()
            .map(|x| self.print_type(&x.ty))
            .try_collect()?;
        let stmts = self.print_expr_no_braces(&body)?;
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
            #vis fn #name #gg(#receiver #(#param_names: #param_types), *) #ret {
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
        let ty = self.print_type(&param.ty)?;
        Ok(quote!(#name: #ty))
    }
    pub fn print_return_type(&self, node: Option<&AstType>) -> Result<TokenStream> {
        if let Some(node) = node {
            let ty = self.print_type(node)?;
            Ok(quote!(-> #ty))
        } else {
            Ok(quote!())
        }
    }
    pub fn print_func_value(&self, fun: &ValueFunction) -> Result<TokenStream> {
        self.print_value_function(fun, Visibility::Private)
    }
    pub fn print_func_type(&self, fun: &TypeFunction) -> Result<TokenStream> {
        let args: Vec<_> = fun
            .params
            .iter()
            .map(|x| self.print_type(x))
            .try_collect()?;
        let ret = self.print_return_type(fun.ret_ty.as_deref())?;
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
        let import: syn::UseTree = syn::parse_str(&node.tree.to_string()).map_err(|e| eyre::eyre!(e.to_string()))?;
        let vis = self.print_vis(node.visibility);

        Ok(quote!(#vis use #import;))
    }
    pub fn print_field_value(&self, s: &ValueField) -> Result<TokenStream> {
        let name = self.print_ident(&s.name);
        let value = self.print_value(&s.value)?;
        Ok(quote!(#name: #value))
    }

    pub fn print_builtin_fn(&self, bt: &BuiltinFn) -> Result<TokenStream> {
        match bt.name {
            BuiltinFnName::BinOpKind(ref op) => Ok(self.print_bin_op_kind(op)),
            BuiltinFnName::Name(ref name) => Ok(self.print_ident(name)),
        }
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
            .map(|x| self.print_type(x))
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
    pub fn print_node(&self, node: &AstNode) -> fp_core::Result<TokenStream> {
        match node {
            AstNode::Item(n) => self.print_item(n),
            AstNode::Expr(n) => self.print_expr(n),
            AstNode::File(n) => self.print_file(n),
        }
    }
}

impl AstSerializer for RustPrinter {
    fn serialize_node(&self, node: &AstNode) -> fp_core::error::Result<String> {
        self.print_node(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_expr(&self, node: &AstExpr) -> fp_core::error::Result<String> {
        Ok(self.print_expr(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))?)
    }

    fn serialize_invoke(&self, node: &ExprInvoke) -> fp_core::error::Result<String> {
        Ok(self.print_invoke(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))?)
    }

    fn serialize_item(&self, node: &AstItem) -> fp_core::error::Result<String> {
        Ok(self.print_item(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))?)
    }

    fn serialize_block(&self, node: &ExprBlock) -> fp_core::error::Result<String> {
        Ok(self.print_block(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))?)
    }

    fn serialize_file(&self, node: &AstFile) -> fp_core::error::Result<String> {
        Ok(self.print_file(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))?)
    }
    fn serialize_module(&self, node: &AstModule) -> fp_core::error::Result<String> {
        Ok(self.print_module(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))?)
    }

    fn serialize_value(&self, node: &AstValue) -> fp_core::error::Result<String> {
        self.print_value(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_type(&self, node: &AstType) -> fp_core::error::Result<String> {
        self.print_type(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_stmt(&self, node: &BlockStmt) -> fp_core::error::Result<String> {
        self.print_statement(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_value_function(&self, node: &ValueFunction) -> fp_core::error::Result<String> {
        self.print_value_function(node, Visibility::Private)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }
    fn serialize_def_function(&self, node: &ItemDefFunction) -> fp_core::error::Result<String> {
        self.print_def_function(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }
}
