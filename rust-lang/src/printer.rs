use common::Result;
use common::*;

use common_lang::ops::BuiltinFn;
use common_lang::tree::FieldValueExpr;
use common_lang::tree::*;
use common_lang::value::*;
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
            "unit" => quote!(()),
            a => format_ident!("{}", a).into_token_stream(),
        }
    }
    pub fn print_def(&self, n: &Def) -> Result<TokenStream> {
        let vis = n.visibility;
        let mut decl = &n.value;
        let g = None;
        // if let Some(d) = decl.as_ast::<Generics>() {
        //     decl = &d.value;
        //     g = Some(d)
        // }
        match decl {
            DefValue::Function(n) => {
                return self.print_func_decl(n, g, vis);
            }
            DefValue::Type(_) => {}
            DefValue::Const(_) => {}
            DefValue::Variable(_) => {}
        }

        bail!("Not supported {:?}", n)
    }
    pub fn print_field(&self, field: &FieldTypeExpr) -> Result<TokenStream> {
        let name = self.print_ident(&field.name);
        let ty = self.print_type_expr(&field.ty)?;
        Ok(quote!(pub #name: #ty ))
    }
    pub fn print_struct_type(&self, s: &NamedStructTypeExpr) -> Result<TokenStream> {
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
        let stmts: Vec<_> = n.stmts.iter().map(|x| self.print_item(x)).try_collect()?;
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
                } else if i < cond.cases.len() - 1 {
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
            Visibility::Inherited => quote!(),
        }
    }
    pub fn print_func_decl(
        &self,
        n: &FuncDecl,
        g: Option<&Generics>,
        vis: Visibility,
    ) -> Result<TokenStream> {
        let name = format_ident!("{}", n.name.as_str());
        let ret_type = &n.ret;
        let ret = self.print_type_expr(ret_type)?;
        let param_names: Vec<_> = n.params.iter().map(|x| self.print_ident(&x.name)).collect();
        let param_types: Vec<_> = n
            .params
            .iter()
            .map(|x| self.print_type_expr(&x.ty))
            .try_collect()?;
        let stmts = self.print_block(&n.body)?;
        let gg;
        if !n.generics_params.is_empty() {
            let gt: Vec<_> = n
                .generics_params
                .iter()
                .map(|x| self.print_ident(&x.name))
                .collect();
            let gb: Vec<_> = n
                .generics_params
                .iter()
                .map(|x| self.print_type_expr(&x.ty))
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
    pub fn print_invoke(&self, node: &InvokeExpr) -> Result<TokenStream> {
        let fun = self.print_expr(&node.fun)?;
        let args: Vec<_> = node.args.iter().map(|x| self.print_expr(x)).try_collect()?;
        match &*node.fun {
            Expr::Select(select) => match select.select {
                SelectType::Field => {
                    return Ok(quote!(
                        (#fun)(#(#args), *)
                    ))
                }
                _ => {}
            },
            _ => {}
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

    pub fn print_func_type(&self, fun: &FuncTypeExpr) -> Result<TokenStream> {
        let args: Vec<_> = fun
            .params
            .iter()
            .map(|x| self.print_type_expr(x))
            .try_collect()?;
        let node = &fun.ret;
        let ret = self.print_type_expr(node)?;
        Ok(quote!(
            fn(#(#args), *) -> #ret
        ))
    }
    pub fn print_select(&self, select: &Select) -> Result<TokenStream> {
        let obj = self.print_expr(&select.obj)?;
        let field = self.print_ident(&select.field);
        match select.select {
            SelectType::Const => Ok(quote!(
                #obj::#field
            )),
            _ => Ok(quote!(
                #obj.#field
            )),
        }
    }
    pub fn print_args(&self, node: &Vec<Expr>) -> Result<TokenStream> {
        let args: Vec<_> = node.iter().map(|x| self.print_expr(x)).try_collect()?;
        Ok(quote!((#(#args),*)))
    }
    pub fn print_generics(&self, node: &Generics) -> Result<TokenStream> {
        let debug = format!("{:?}", node);
        Ok(quote!(#debug))
    }
    pub fn print_impl(&self, impl_: &Impl) -> Result<TokenStream> {
        let name = self.print_ident(&impl_.name);
        let methods: Vec<_> = impl_.defs.iter().map(|x| self.print_def(x)).try_collect()?;
        Ok(quote!(
            impl #name {
                #(#methods)*
            }
        ))
    }
    pub fn print_module(&self, m: &Module) -> Result<TokenStream> {
        let stmts: Vec<_> = m.items.iter().map(|x| self.print_item(x)).try_collect()?;
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
        Ok(quote!(#vis use #(#segments)::*;))
    }
    pub fn print_field_value(&self, s: &FieldValueExpr) -> Result<TokenStream> {
        let name = self.print_ident(&s.name);
        let value = self.print_expr(&s.value)?;
        Ok(quote!(#name: #value))
    }
    pub fn print_build_struct(&self, s: &StructExpr) -> Result<TokenStream> {
        let name = self.print_type_expr(&s.name)?;
        let kwargs: Vec<_> = s
            .fields
            .iter()
            .map(|x| self.print_field_value(x))
            .try_collect()?;
        Ok(quote!(#name { #(#kwargs), * }))
    }
    pub fn print_builtin_fn(&self, bt: &BuiltinFn) -> Result<TokenStream> {
        let name = self.print_ident(&Ident::new(bt.name.clone()));
        Ok(quote!(#name))
    }
    pub fn print_int(&self, n: &IntValue) -> Result<TokenStream> {
        let n = n.value;
        Ok(quote!(#n))
    }
    pub fn print_bool(&self, n: &BoolValue) -> Result<TokenStream> {
        let n = n.value;
        Ok(quote!(#n))
    }
    pub fn print_decimal(&self, n: &DecimalValue) -> Result<TokenStream> {
        let n = n.value;
        Ok(quote!(#n))
    }
    pub fn print_char(&self, n: &CharValue) -> Result<TokenStream> {
        let n = n.value;
        Ok(quote!(#n))
    }
    pub fn print_string(&self, n: &StringValue) -> Result<TokenStream> {
        let v = &n.value;
        return if n.owned {
            Ok(quote!(
                #v.to_string()
            ))
        } else {
            Ok(quote!(
                #v
            ))
        };
    }
    pub fn print_list(&self, n: &Vec<Expr>) -> Result<TokenStream> {
        let n: Vec<_> = n.iter().map(|x| self.print_expr(x)).try_collect()?;
        Ok(quote!(vec![#(#n),*]))
    }
    pub fn print_unit(&self, _n: &UnitValue) -> Result<TokenStream> {
        Ok(quote!(()))
    }
    pub fn print_type(&self, t: &TypeValue) -> Result<TokenStream> {
        match t {
            TypeValue::FuncType(f) => self.print_func_type(&f),
            TypeValue::UnnamedStruct(s) => self.print_struct_type(&s),
            _ => bail!("Not supported {:?}", t),
        }
    }

    pub fn print_value(&self, v: &Value) -> Result<TokenStream> {
        match v {
            Value::Function(f) => self.print_func_type(&f),
            Value::Int(i) => self.print_int(i),
            Value::Bool(b) => self.print_bool(b),
            Value::Decimal(d) => self.print_decimal(d),
            Value::Char(c) => self.print_char(c),
            Value::String(s) => self.print_string(s),
            Value::List(l) => self.print_list(l),
            Value::Unit(u) => self.print_unit(u),
            Value::Type(t) => self.print_type(t.clone()),
            Value::Struct(s) => self.print_build_struct(s),
            _ => bail!("Not supported {:?}", v),
        }
    }
    pub fn print_type_expr(&self, node: &TypeExpr) -> Result<TokenStream> {
        match node {
            TypeExpr::Primitive(p) => Ok(self.print_ident(&p.name)),
            _ => {}
        }
        bail!("Unable to serialize {:?}", node)
    }
    pub fn print_expr(&self, node: &Expr) -> Result<TokenStream> {
        match node {
            Expr::Ident(n) => Ok(self.print_ident(n)),
            Expr::Path(n) => Ok(self.print_ident(&n.segments.last().unwrap().ident)),
            Expr::Value(n) => self.print_value(n),
            Expr::Block(_) => {}
            Expr::Cond(_) => {}
            Expr::Invoke(_) => {}
            Expr::BuiltinFn(_) => {}
            Expr::Select(_) => {}
            Expr::Any(_) => {}
        }
        bail!("Unable to serialize {:?}", node)
    }
    pub fn print_item(&self, item: &Item) -> Result<TokenStream> {
        match item {
            Item::Def(n) => self.print_def(n),
            Item::Module(n) => self.print_module(n),
            Item::Import(n) => self.print_import(n),
        }
    }
    pub fn print_tree(&self, node: &Tree) -> Result<TokenStream> {
        match node {
            Tree::Item(n) => self.print_item(n),
        }
    }
}
