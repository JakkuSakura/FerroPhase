use common::*;
use common_lang::ast::Expr;
use common_lang::interpreter::ExecutionContext;
use common_lang::specializer::Specializer;
use proc_macro::TokenStream;
use rust_lang::parser::RustParser;
use rust_lang::printer::RustPrinter;
use rust_lang::RustSerde;
use std::rc::Rc;

fn specialize_inner(code: Expr) -> Result<TokenStream> {
    let ctx = ExecutionContext::new();
    let node = Specializer::new(Rc::new(RustSerde)).specialize_expr(&code, &ctx)?;
    let node = RustPrinter.print_expr(&node)?;
    Ok(node.into())
}
#[proc_macro]
pub fn specialize(input: TokenStream) -> TokenStream {
    let input: syn::File = syn::parse(input.into()).unwrap();
    let input = RustParser.parse_file(input).unwrap();
    specialize_inner(input.into()).unwrap().into()
}

#[proc_macro_attribute]
pub fn specialize_module(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let input: syn::ItemMod = syn::parse(input.into()).unwrap();
    let input = RustParser.parse_module(input).unwrap();

    specialize_inner(input).unwrap().into()
}
