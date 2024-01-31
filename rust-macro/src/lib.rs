use common::*;

use common_lang::ast::{File, Module, Tree};
use common_lang::context::{ArcScopedContext, ScopedContext};
use common_lang::expr::Expr;
use common_lang::optimizer::{load_optimizer, FoldOptimizer};
use common_lang::passes::OptimizePass;
use proc_macro::TokenStream;
use rust_lang::parser::RustParser;
use rust_lang::printer::RustPrinter;
use rust_lang::RustSerde;
use std::rc::Rc;
use std::sync::Arc;

trait Optimizee {
    fn optimize<P: OptimizePass>(
        self,
        optimizer: FoldOptimizer<P>,
        ctx: &ArcScopedContext,
    ) -> Result<TokenStream>;
}
impl Optimizee for Expr {
    fn optimize<P: OptimizePass>(
        self,
        optimizer: FoldOptimizer<P>,
        ctx: &ArcScopedContext,
    ) -> Result<TokenStream> {
        let node = optimizer.optimize_expr(self, ctx)?;
        let node = RustPrinter.print_expr(&node)?;
        Ok(node.into())
    }
}
impl Optimizee for Module {
    fn optimize<P: OptimizePass>(
        self,
        optimizer: FoldOptimizer<P>,
        ctx: &ArcScopedContext,
    ) -> Result<TokenStream> {
        let node = optimizer.optimize_module(self, ctx, true)?;
        let node = RustPrinter.print_module(&node)?;
        Ok(node.into())
    }
}
impl Optimizee for File {
    fn optimize<P: OptimizePass>(
        self,
        optimizer: FoldOptimizer<P>,
        ctx: &ArcScopedContext,
    ) -> Result<TokenStream> {
        let node = optimizer.optimize_tree(Tree::File(self), ctx)?;
        let node = RustPrinter.print_tree(&node)?;
        Ok(node.into())
    }
}
fn specialize_inner(code: impl Optimizee) -> Result<TokenStream> {
    let ctx = Arc::new(ScopedContext::new());
    let formatter = RustSerde::new();
    let optimizer = load_optimizer(Rc::new(formatter));
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
