pub mod parser;
pub mod preloader;
pub mod printer;
pub mod rustfmt;

use common::Result;
use common_lang::ast::{Expr, *};
use common_lang::*;

use std::fmt::Debug;
use syn::*;
#[derive(Debug, Clone)]
pub struct RawMacro {
    raw: syn::ExprMacro,
}
impl Ast for RawMacro {
    fn is_raw(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone)]
pub struct RawUse {
    raw: syn::ItemUse,
}
impl Ast for RawUse {
    fn is_raw(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone)]
pub struct RawImplTrait {
    raw: syn::TypeImplTrait,
}
impl Ast for RawImplTrait {
    fn is_raw(&self) -> bool {
        true
    }
}
#[derive(Debug, Clone)]
pub struct RawExpr {
    raw: syn::Expr,
}
impl Ast for RawExpr {
    fn is_raw(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone)]
pub struct RawTokenSteam {
    pub raw: proc_macro2::TokenStream,
}
impl Ast for RawTokenSteam {
    fn is_raw(&self) -> bool {
        true
    }
}
pub struct RustSerde;
impl Serializer for RustSerde {
    fn serialize(&self, node: &Expr) -> Result<String> {
        self.serialize_expr(node).map(|x| x.to_string())
    }
}
impl Deserializer for RustSerde {
    fn deserialize(&self, code: &str) -> Result<Expr> {
        let code: syn::File = parse_str(code)?;
        Ok(self.deserialize_file(code)?.into())
    }
}
