use common::*;
use common_lang::ast::*;

use proc_macro::TokenStream;
use quote::quote;
use rust_lang::parser::RustParser;
use rust_lang::printer::RustPrinter;
use rust_lang::*;

fn get_pipe_op(expr: &Expr) -> Option<&Call> {
    if let Some(x) = expr.as_ast::<Call>() {
        if x.fun.as_ast::<Ident>() == Some(&Ident::new("|")) {
            return Some(x);
        }
    }
    None
}
fn process_fanout_op(expr: &Expr) -> Option<&Call> {
    if let Some(x) = expr.as_ast::<Call>() {
        if x.fun.as_ast::<Ident>() == Some(&Ident::new("tuple")) {
            return Some(x);
        }
    }
    None
}
fn process_fanout(expr: &Expr) -> Expr {
    if let Some(x) = process_fanout_op(expr) {
        let mut args: Vec<_> = x.args.iter().map(process_actor).collect();
        let mut result = RustPrinter.print_expr(&args.pop().unwrap()).unwrap();
        while !args.is_empty() {
            let left = RustPrinter.print_expr(&args.pop().unwrap()).unwrap();
            result = quote!(
                shell_lang::Fanout::new(#left, #result)
            );
        }
        return RawTokenSteam { raw: result }.into();
    }
    expr.clone()
}
fn process_pipe(expr: &Expr) -> Expr {
    if let Some(x) = get_pipe_op(expr) {
        let mut args: Vec<_> = x.args.iter().map(process_actor).collect();
        let mut result = RustPrinter.print_expr(&args.pop().unwrap()).unwrap();
        while !args.is_empty() {
            let left = RustPrinter.print_expr(&args.pop().unwrap()).unwrap();
            result = quote!(
                shell_lang::Pipe::new(#left, #result)
            );
        }
        return RawTokenSteam { raw: result }.into();
    }
    expr.clone()
}
fn process_actor(expr: &Expr) -> Expr {
    if let Some(x) = expr.as_ast::<Call>() {
        if let Some(x) = x.fun.as_ast::<Ident>() {
            match x.as_str() {
                "tuple" => return process_fanout(expr),
                "|" => return process_pipe(expr),
                _ => {}
            }
        }
    }
    expr.clone()
}
fn shell_inner(code: Expr) -> Result<TokenStream> {
    // panic!("{:#?}", code);

    let code = process_pipe(&code);
    let node = RustPrinter.print_expr(&code)?;
    // panic!("{}", node);
    Ok(node.into())
}

#[proc_macro]
pub fn pipe(input: TokenStream) -> TokenStream {
    let input: syn::Expr = syn::parse(input.into()).unwrap();
    let input = RustParser.parse_expr(input).unwrap();
    shell_inner(input).unwrap().into()
}
