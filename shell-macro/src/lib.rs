use common::*;
use common_lang::*;
use proc_macro::TokenStream;
use quote::quote;
use rust_lang::*;

fn get_pipe_call_op(expr: &Expr) -> Option<&Call> {
    if let Some(x) = expr.as_ast::<Call>() {
        if x.fun.as_ast::<Ident>() == Some(&Ident::new("|")) {
            return Some(x);
        }
    }
    None
}
fn process_pipe(expr: &Expr) -> proc_macro2::TokenStream {
    if let Some(x) = get_pipe_call_op(expr) {
        let mut args: Vec<_> = x.args.args.iter().collect();
        let mut result = RustSerde.serialize_expr(&args.pop().unwrap()).unwrap();
        while !args.is_empty() {
            let left = RustSerde.serialize_expr(&args.pop().unwrap()).unwrap();
            result = quote!(
                shell_lang::Pipe::new(#left, #result)
            );
        }
        return result;
    }
    RustSerde.serialize_ast(&**expr).unwrap()
}

fn shell_inner(code: Expr) -> Result<TokenStream> {
    // panic!("{:#?}", code);

    let code = process_pipe(&code);
    let node = code;
    // panic!("{}", node);
    Ok(node.into())
}

#[proc_macro]
pub fn pipe(input: TokenStream) -> TokenStream {
    let input: syn::Expr = syn::parse(input.into()).unwrap();
    let input = RustSerde.deserialize_expr(input).unwrap();
    shell_inner(input).unwrap().into()
}
