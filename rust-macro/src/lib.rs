use common::*;

use common_lang::context::ScopedContext;
use common_lang::optimizer::{load_optimizer, FoldOptimizer};

use common_lang::ast::{Expr, Module};
use proc_macro::TokenStream;
use rust_lang::parser::RustParser;
use rust_lang::printer::RustPrinter;
use rust_lang::RustSerde;
use std::rc::Rc;

trait Optomizee {
    fn optimize(self, optimizer: FoldOptimizer, ctx: &ScopedContext) -> Result<TokenStream>;
}
impl Optomizee for Expr {
    fn optimize(self, optimizer: FoldOptimizer, ctx: &ScopedContext) -> Result<TokenStream> {
        let node = optimizer.optimize_expr(self, ctx)?;
        let node = RustPrinter.print_expr(&node)?;
        Ok(node.into())
    }
}
impl Optomizee for Module {
    fn optimize(self, optimizer: FoldOptimizer, ctx: &ScopedContext) -> Result<TokenStream> {
        let node = optimizer.optimize_module(self, ctx)?;
        let node = RustPrinter.print_module(&node)?;
        Ok(node.into())
    }
}
fn specialize_inner(code: impl Optomizee) -> Result<TokenStream> {
    let ctx = ScopedContext::new();
    let formatter = RustSerde::new(false);
    let optimizer = load_optimizer(Rc::new(formatter));
    let node = code.optimize(optimizer, &ctx)?;

    Ok(node.into())
}
#[proc_macro]
pub fn specialize(input: TokenStream) -> TokenStream {
    let input: syn::File = syn::parse(input.into()).unwrap();
    let input = RustParser.parse_file(input).unwrap();
    specialize_inner(input).unwrap().into()
}

#[proc_macro_attribute]
pub fn specialize_module(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let input: syn::ItemMod = syn::parse(input.into()).unwrap();
    let input = RustParser.parse_module(input).unwrap();

    specialize_inner(input).unwrap().into()
}
