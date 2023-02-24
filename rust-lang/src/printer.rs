use crate::{RawExpr, RawImplTrait, RawMacro, RawUse, RustSerde};
use common::Result;
use common::*;
use common_lang::{Block, Ident, *};
use proc_macro2::TokenStream;
use quote::*;

impl RustSerde {
    pub fn serialize_ident(&self, i: &Ident) -> TokenStream {
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
    pub fn serialize_def(&self, n: &Def) -> Result<TokenStream> {
        let vis = n.visibility;
        let mut decl = &n.value;
        let mut g = None;
        if let Some(d) = decl.as_ast::<Generics>() {
            decl = &d.value;
            g = Some(d)
        }
        if let Some(n) = decl.as_ast::<FuncDecl>() {
            return self.serialize_func_decl(n, g, vis);
        }
        bail!("Not supported {:?}", n)
    }
    pub fn serialize_block(&self, n: &Block) -> Result<TokenStream> {
        let stmts: Vec<_> = n
            .stmts
            .iter()
            .map(|x| self.serialize_ast(&**x))
            .try_collect()?;
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
    pub fn serialize_cond(&self, cond: &Cond) -> Result<TokenStream> {
        let mut ts = vec![];
        if cond.if_style {
            for (i, c) in cond.cases.iter().enumerate() {
                let co = self.serialize_ast(&*c.cond)?;
                let ex = self.serialize_ast(&*c.body)?;
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
                let co = self.serialize_ast(&*c.cond)?;
                let ex = self.serialize_ast(&*c.body)?;
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
    pub fn serialize_func_decl(
        &self,
        n: &FuncDecl,
        g: Option<&Generics>,
        vis: Visibility,
    ) -> Result<TokenStream> {
        let name = format_ident!("{}", n.name.as_ref().unwrap().name);
        let ret = self.serialize_ast(&*n.ret)?;
        let param_names: Vec<_> = n
            .params
            .params
            .iter()
            .map(|x| self.serialize_ident(&x.name))
            .collect();
        let param_types: Vec<_> = n
            .params
            .params
            .iter()
            .map(|x| self.serialize_ast(&*x.ty))
            .try_collect()?;
        let stmts = self.serialize_block(n.body.as_ref().unwrap())?;
        let gg;
        if let Some(g) = g {
            let gt: Vec<_> = g
                .params
                .params
                .iter()
                .map(|x| self.serialize_ident(&x.name))
                .collect();
            let gb: Vec<_> = g
                .params
                .params
                .iter()
                .map(|x| self.serialize_ast(&*x.ty))
                .try_collect()?;
            gg = quote!(<#(#gt: #gb), *>)
        } else {
            gg = quote!();
        }
        let vis = match vis {
            Visibility::Public => quote!(pub),
            Visibility::Private => quote!(),
        };
        return Ok(quote!(
            #vis fn #name #gg(#(#param_names: #param_types), *) -> #ret
                #stmts

        ));
    }
    pub fn serialize_call(&self, node: &Call) -> Result<TokenStream> {
        let fun = self.serialize_ast(&*node.fun)?;
        let args: Vec<_> = node
            .args
            .args
            .iter()
            .map(|x| self.serialize_ast(&**x))
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
            // TODO: can't tell method or member
            _ => quote!(
                #fun(#(#args), *)
            ),
        };
        // if true {
        //     return Ok(quote!((#code)));
        // }
        Ok(code)
    }
    pub fn serialize_ref(&self, n: &Reference) -> Result<TokenStream> {
        let referee = self.serialize_expr(&n.referee)?;
        if n.mutable == Some(true) {
            Ok(quote!(&mut #referee))
        } else {
            Ok(quote!(&#referee))
        }
    }
    pub fn serialize_literal(&self, n: &dyn AnyAst) -> Result<TokenStream> {
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
    pub fn serialize_func_type(&self, fun: &FuncType) -> Result<TokenStream> {
        let args: Vec<_> = fun
            .params
            .iter()
            .map(|x| self.serialize_ast(&**x))
            .try_collect()?;
        let ret = self.serialize_ast(&*fun.ret)?;
        Ok(quote!(
            fn(#(#args), *) -> #ret
        ))
    }
    pub fn serialize_select(&self, select: &Select) -> Result<TokenStream> {
        let obj = self.serialize_ast(&*select.obj)?;
        let field = self.serialize_ident(&select.field);

        Ok(quote!(
            #obj.#field
        ))
    }
    pub fn serialize_args(&self, node: &PosArgs) -> Result<TokenStream> {
        let args: Vec<_> = node
            .args
            .iter()
            .map(|x| self.serialize_ast(&**x))
            .try_collect()?;
        Ok(quote!((#(#args),*)))
    }
    pub fn serialize_generics(&self, node: &Generics) -> Result<TokenStream> {
        let debug = format!("{:?}", node);
        Ok(quote!(#debug))
    }
    // TODO: use serialize_expr as much as possible
    pub fn serialize_expr(&self, node: &Expr) -> Result<TokenStream> {
        self.serialize_ast(&**node)
    }

    pub fn serialize_ast(&self, node: &dyn AnyAst) -> Result<TokenStream> {
        if let Some(n) = node.as_ast() {
            return self.serialize_block(n);
        }
        if let Some(n) = node.as_ast() {
            return self.serialize_args(n);
        }
        if let Some(n) = node.as_ast() {
            return self.serialize_generics(n);
        }
        if let Some(m) = node.as_ast::<Module>() {
            let stmts: Vec<_> = m
                .stmts
                .iter()
                .map(|x| self.serialize_ast(&**x))
                .try_collect()?;
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
        if let Some(n) = node.as_ast::<Def>() {
            return self.serialize_def(n);
        }
        if let Some(n) = node.as_ast::<Ident>() {
            return Ok(self.serialize_ident(n).to_token_stream());
        }

        if let Some(_n) = node.as_ast::<Unit>() {
            return Ok(quote!(()));
        }

        if let Some(n) = node.as_ast() {
            return self.serialize_call(n);
        }
        if node.is_literal() {
            return self.serialize_literal(node);
        }
        if let Some(n) = node.as_ast::<RawMacro>() {
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
        if let Some(n) = node.as_ast() {
            return self.serialize_func_type(n);
        }
        if let Some(n) = node.as_ast() {
            return self.serialize_select(n);
        }

        if let Some(n) = node.as_ast() {
            return self.serialize_cond(n);
        }

        if let Some(n) = node.as_ast() {
            return self.serialize_ref(n);
        }
        bail!("Unable to serialize {:?}", node)
    }
}
