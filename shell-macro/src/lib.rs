use common::*;
use common_lang::*;
use proc_macro::TokenStream;
use quote::quote;
use rust_lang::*;
use std::cell::RefCell;

fn get_pipe_call_op(expr: &Expr) -> Option<&Call> {
    if let Some(x) = expr.as_ast::<Call>() {
        if x.fun.as_ast::<Ident>() == Some(&Ident::new("|")) {
            return Some(x);
        }
    }
    None
}
fn process_pipe(
    expr: &Expr,
    decl: &mut Vec<proc_macro2::TokenStream>,
    get_id: impl Fn() -> Ident + Clone,
) -> Expr {
    if let Some(x) = get_pipe_call_op(expr) {
        let args = x
            .args
            .args
            .iter()
            .map(|x0| {
                if x0.as_ast::<Ident>().is_some() {
                    return x0.clone();
                } else if let Some(_x) = get_pipe_call_op(x0) {
                    let x = process_pipe(x0, decl, get_id.clone());
                    return x.clone();
                }

                let id = (get_id)();
                let id_ = RustSerde.serialize_ident(&id);
                let expr = RustSerde.serialize_expr(&x0).unwrap();
                decl.push(quote!(
                   let #id_ = #expr;
                ));
                id.into()
            })
            .collect();

        return Call {
            fun: x.fun.clone(),
            args: PosArgs { args },
        }
        .into();
    }
    expr.clone()
}

fn shell_inner(code: Expr) -> Result<TokenStream> {
    // panic!("{:#?}", code);

    let i = RefCell::new(0);
    let get_id = || {
        *i.borrow_mut() += 1;
        Ident::new(format!("id_{}", *i.borrow()))
    };
    let mut decls = vec![];
    let code = process_pipe(&code, &mut decls, get_id);
    let node = RustSerde.serialize_expr(&code)?;
    let node = quote!(
        #(#decls) *
        let _ = pipe!(#node).start();
        ()
    );
    // panic!("{}", node);
    Ok(node.into())
}

#[proc_macro]
pub fn shell(input: TokenStream) -> TokenStream {
    let input: syn::Expr = syn::parse(input.into()).unwrap();
    let input = RustSerde.deserialize_expr(input).unwrap();
    shell_inner(input).unwrap().into()
}
