use crate::{RawExpr, RawExprMacro, RawImplTrait, RawItemMacro, RawTokenSteam, RawType, RawUse};
use common::Result;
use common::*;
use common_lang::ast::*;
use proc_macro2::TokenStream;
use quote::*;

pub struct RustPrinter;
impl RustPrinter {
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
            "&Self" => quote!(&Self),
            "&mut Self" => quote!(&mut Self),
            "Self" => quote!(Self),
            "mut Self" => quote!(mut Self),
            a => format_ident!("{}", a).into_token_stream(),
        }
    }
    pub fn print_def(&self, n: &Def) -> Result<TokenStream> {
        let vis = n.visibility;
        let mut decl = &n.value;
        let mut g = None;
        if let Some(d) = decl.as_ast::<Generics>() {
            decl = &d.value;
            g = Some(d)
        }
        if let Some(n) = decl.as_ast::<FuncDecl>() {
            return self.print_func_decl(n, g, vis);
        }
        bail!("Not supported {:?}", n)
    }
    pub fn print_field(&self, field: &Field) -> Result<TokenStream> {
        let name = self.print_ident(&field.name);
        let ty = self.print_expr(&field.ty)?;
        Ok(quote!(pub #name: #ty ))
    }
    pub fn print_struct(&self, s: &Struct) -> Result<TokenStream> {
        let name = self.print_ident(&s.name);
        let fields: Vec<_> = s
            .fields
            .iter()
            .map(|x| self.print_field(&x))
            .try_collect()?;
        Ok(quote!(
            pub struct #name {
                #(#fields), *
            }
        ))
    }

    pub fn print_block(&self, n: &Block) -> Result<TokenStream> {
        let stmts: Vec<_> = n.stmts.iter().map(|x| self.print_expr(x)).try_collect()?;
        let q = if n.last_value {
            quote!({
                #(#stmts);*
            })
        } else {
            quote!({
                #(#stmts;)*
            })
        };
        return Ok(q);
    }
    pub fn print_cond(&self, cond: &Cond) -> Result<TokenStream> {
        let mut ts = vec![];
        if cond.if_style {
            for (i, c) in cond.cases.iter().enumerate() {
                let node = &c.cond;
                let co = self.print_expr(node)?;
                let node = &c.body;
                let ex = self.print_expr(node)?;
                if i == 0 {
                    ts.push(quote!(
                        if #co {
                            #ex
                        }
                    ));
                } else if c.cond.as_ast::<LiteralBool>().map(|x| x.value) != Some(false) {
                    ts.push(quote!(
                        else if #co {
                            #ex
                        }
                    ));
                } else {
                    ts.push(quote!(
                        else {
                            #ex
                        }
                    ));
                }
            }
            Ok(quote!(#(#ts)*))
        } else {
            for (_i, c) in cond.cases.iter().enumerate() {
                let node = &c.cond;
                let co = self.print_expr(node)?;
                let node = &c.body;
                let ex = self.print_expr(node)?;
                ts.push(quote!(
                    if #co => { #ex }
                ))
            }
            Ok(quote!(match () {
                () #(#ts)*
                _ => {}
            }))
        }
    }
    pub fn print_vis(&self, vis: Visibility) -> TokenStream {
        match vis {
            Visibility::Public => quote!(pub),
            Visibility::Private => quote!(),
        }
    }
    pub fn print_func_decl(
        &self,
        n: &FuncDecl,
        g: Option<&Generics>,
        vis: Visibility,
    ) -> Result<TokenStream> {
        let name = format_ident!("{}", n.name.as_ref().unwrap().name);
        let node = &n.ret;
        let ret = self.print_expr(node)?;
        let param_names: Vec<_> = n
            .params
            .params
            .iter()
            .map(|x| self.print_ident(&x.name))
            .collect();
        let param_types: Vec<_> = n
            .params
            .params
            .iter()
            .map(|x| self.print_expr(&x.ty))
            .try_collect()?;
        let stmts = self.print_block(n.body.as_ref().unwrap())?;
        let gg;
        if let Some(g) = g {
            let gt: Vec<_> = g
                .params
                .params
                .iter()
                .map(|x| self.print_ident(&x.name))
                .collect();
            let gb: Vec<_> = g
                .params
                .params
                .iter()
                .map(|x| self.print_expr(&x.ty))
                .try_collect()?;
            gg = quote!(<#(#gt: #gb), *>)
        } else {
            gg = quote!();
        }
        let vis = self.print_vis(vis);
        return Ok(quote!(
            #vis fn #name #gg(#(#param_names: #param_types), *) -> #ret
                #stmts

        ));
    }
    pub fn print_call(&self, node: &Call) -> Result<TokenStream> {
        let node1 = &node.fun;
        let fun = self.print_expr(node1)?;
        let args: Vec<_> = node
            .args
            .args
            .iter()
            .map(|x| self.print_expr(x))
            .try_collect()?;
        if let Some(select) = node.fun.as_ast::<Select>() {
            match select.select {
                SelectType::Field => {
                    return Ok(quote!(
                        (#fun)(#(#args), *)
                    ))
                }
                _ => {}
            }
        }
        let fun_str = fun.to_string();

        let code = match fun_str.as_str() {
            "+" => quote!(#(#args) + *),
            "-" => quote!(#(#args) - *),
            "/" => quote!(#(#args) / *),
            "|" => quote!(#(#args) | *),
            "*" => {
                let mut result = vec![];
                for (i, a) in args.into_iter().enumerate() {
                    if i != 0 {
                        result.push(quote!(*));
                    }
                    result.push(a);
                }
                quote!(#(#result)*)
            }
            "tuple" => quote!(
                (#(#args), *)
            ),
            _ => quote!(
                #fun(#(#args), *)
            ),
        };
        // if true {
        //     return Ok(quote!((#code)));
        // }
        Ok(code)
    }
    pub fn print_ref(&self, n: &Reference) -> Result<TokenStream> {
        let referee = self.print_expr(&n.referee)?;
        if n.mutable == Some(true) {
            Ok(quote!(&mut #referee))
        } else {
            Ok(quote!(&#referee))
        }
    }
    pub fn print_literal(&self, n: &Expr) -> Result<TokenStream> {
        if let Some(n) = n.as_ast::<LiteralInt>() {
            let n = n.value;
            return Ok(quote!(
                #n
            ));
        }
        if let Some(n) = n.as_ast::<LiteralBool>() {
            let n = n.value;
            return Ok(quote!(
                #n
            ));
        }
        if let Some(n) = n.as_ast::<LiteralDecimal>() {
            let n = n.value;
            return Ok(quote!(
                #n
            ));
        }
        bail!("Failed to serialize literal {:?}", n)
    }
    pub fn print_func_type(&self, fun: &FuncType) -> Result<TokenStream> {
        let args: Vec<_> = fun
            .params
            .iter()
            .map(|x| self.print_expr(&x))
            .try_collect()?;
        let node = &fun.ret;
        let ret = self.print_expr(node)?;
        Ok(quote!(
            fn(#(#args), *) -> #ret
        ))
    }
    pub fn print_select(&self, select: &Select) -> Result<TokenStream> {
        let obj = self.print_expr(&select.obj)?;
        let field = self.print_ident(&select.field);

        Ok(quote!(
            #obj.#field
        ))
    }
    pub fn print_args(&self, node: &PosArgs) -> Result<TokenStream> {
        let args: Vec<_> = node.args.iter().map(|x| self.print_expr(x)).try_collect()?;
        Ok(quote!((#(#args),*)))
    }
    pub fn print_generics(&self, node: &Generics) -> Result<TokenStream> {
        let debug = format!("{:?}", node);
        Ok(quote!(#debug))
    }
    pub fn print_impl(&self, impl_: &Impl) -> Result<TokenStream> {
        let name = self.print_expr(&impl_.name)?;
        let methods: Vec<_> = impl_.defs.iter().map(|x| self.print_def(x)).try_collect()?;
        Ok(quote!(
            impl #name {
                #(#methods)*
            }
        ))
    }
    pub fn print_module(&self, m: &Module) -> Result<TokenStream> {
        let stmts: Vec<_> = m.items.iter().map(|x| self.print_expr(x)).try_collect()?;
        if m.name.as_str() == "__file__" {
            return Ok(quote!(
                #(#stmts)*
            ));
        } else {
            let file = format_ident!("{}", m.name.as_str());
            return Ok(quote!(
                pub mod #file {
                    #(#stmts)*
                }
            ));
        }
    }
    pub fn print_import(&self, node: &Import) -> Result<TokenStream> {
        let vis = self.print_vis(node.visibility);
        let segments = node
            .segments
            .iter()
            .map(|x| self.print_ident(x))
            .collect::<Vec<_>>();
        Ok(quote!(#vis use #(#segments):: *))
    }
    pub fn print_expr(&self, node: &Expr) -> Result<TokenStream> {
        let node = &uplift_common_ast(node);

        if let Some(n) = node.as_ast::<Uplifted>() {
            return self.print_expr(&n.uplifted);
        }
        if let Some(n) = node.as_ast::<Types>() {
            return self.print_expr(&n.clone().into());
        }
        if let Some(n) = node.as_ast() {
            return self.print_block(n);
        }
        if let Some(n) = node.as_ast() {
            return self.print_args(n);
        }
        if let Some(n) = node.as_ast() {
            return self.print_generics(n);
        }
        if let Some(m) = node.as_ast::<Module>() {
            return self.print_module(m);
        }
        if let Some(n) = node.as_ast::<Def>() {
            return self.print_def(n);
        }
        if let Some(n) = node.as_ast::<Ident>() {
            return Ok(self.print_ident(n));
        }

        if let Some(_n) = node.as_ast::<Unit>() {
            return Ok(quote!(()));
        }

        if let Some(n) = node.as_ast() {
            return self.print_call(n);
        }
        if node.is_literal() {
            return self.print_literal(node);
        }
        if let Some(n) = node.as_ast::<RawExprMacro>() {
            return Ok(n.raw.to_token_stream());
        }
        if let Some(n) = node.as_ast::<RawItemMacro>() {
            return Ok(n.raw.to_token_stream());
        }
        if let Some(n) = node.as_ast::<RawUse>() {
            return Ok(n.raw.to_token_stream());
        }

        if let Some(n) = node.as_ast::<RawImplTrait>() {
            return Ok(n.raw.to_token_stream());
        }
        if let Some(n) = node.as_ast::<RawExpr>() {
            return Ok(n.raw.to_token_stream());
        }
        if let Some(n) = node.as_ast::<RawTokenSteam>() {
            return Ok(n.raw.to_token_stream());
        }
        if let Some(n) = node.as_ast::<RawType>() {
            return Ok(n.raw.to_token_stream());
        }
        if let Some(n) = node.as_ast() {
            return self.print_impl(n);
        }
        if let Some(n) = node.as_ast() {
            return self.print_func_type(n);
        }
        if let Some(n) = node.as_ast() {
            return self.print_select(n);
        }

        if let Some(n) = node.as_ast() {
            return self.print_cond(n);
        }
        if let Some(n) = node.as_ast() {
            return self.print_struct(n);
        }

        if let Some(n) = node.as_ast() {
            return self.print_ref(n);
        }
        if let Some(n) = node.as_ast() {
            return self.print_import(n);
        }
        bail!("Unable to serialize {:?}", node)
    }
}
