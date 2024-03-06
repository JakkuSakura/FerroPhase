use common::*;

use common_lang::ast::{File, Module};
use common_lang::context::{ScopedContext, SharedScopedContext};
use common_lang::expr::Expr;
use common_lang::optimizer::{load_optimizers, FoldOptimizer};
use proc_macro::TokenStream;
use rust_lang::parser::RustParser;
use rust_lang::printer::RustPrinter;
use rust_lang::RustSerde;
use std::rc::Rc;
use std::sync::Arc;

trait Optimizee {
    fn optimize(
        self,
        optimizer: Vec<FoldOptimizer>,
        ctx: &SharedScopedContext,
    ) -> Result<TokenStream>;
}
impl Optimizee for Expr {
    fn optimize(
        mut self,
        optimizer: Vec<FoldOptimizer>,
        ctx: &SharedScopedContext,
    ) -> Result<TokenStream> {
        for opt in optimizer {
            self = opt.optimize_expr(self, ctx)?;
        }

        let node = RustPrinter.print_expr(&self)?;
        Ok(node.into())
    }
}
impl Optimizee for Module {
    fn optimize(
        mut self,
        optimizer: Vec<FoldOptimizer>,
        ctx: &SharedScopedContext,
    ) -> Result<TokenStream> {
        for opt in optimizer {
            self = opt.optimize_module(self, ctx, true)?;
        }

        let node = RustPrinter.print_module(&self)?;
        Ok(node.into())
    }
}
impl Optimizee for File {
    fn optimize(
        mut self,
        optimizer: Vec<FoldOptimizer>,
        ctx: &SharedScopedContext,
    ) -> Result<TokenStream> {
        for opt in optimizer {
            self = opt.optimize_file(self, ctx)?;
        }
        let node = RustPrinter.print_file(&self)?;
        Ok(node.into())
    }
}
fn specialize_inner(code: impl Optimizee) -> Result<TokenStream> {
    let ctx = Arc::new(ScopedContext::new());
    let formatter = RustSerde::new();
    let optimizer = load_optimizers(Rc::new(formatter));
    let node = code.optimize(optimizer, &ctx)?;

    Ok(node.into())
}
#[proc_macro]
pub fn specialize(input: TokenStream) -> TokenStream {
    let input: syn::File = syn::parse(input.into()).unwrap();
    let input = RustParser::new().parse_file("".into(), input).unwrap();
    specialize_inner(input).unwrap().into()
}

#[proc_macro_attribute]
pub fn specialize_module(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let input: syn::ItemMod = syn::parse(input.into()).unwrap();
    let input = RustParser::new().parse_module(input).unwrap();

    specialize_inner(input).unwrap().into()
}
