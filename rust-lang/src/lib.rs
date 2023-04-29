pub mod parser;
pub mod printer;
pub mod rustfmt;

use common::Result;
use common_lang::ast::{Expr, *};
use common_lang::*;

use crate::parser::RustParser;
use crate::printer::RustPrinter;
use std::fmt::Debug;
use syn::*;

#[derive(Debug, Clone)]
pub struct RawExprMacro {
    pub raw: syn::ExprMacro,
}
impl Ast for RawExprMacro {
    fn is_raw(&self) -> bool {
        true
    }
}
impl_panic_serde!(RawExprMacro);

#[derive(Debug, Clone)]
pub struct RawItemMacro {
    pub raw: syn::ItemMacro,
}
impl Ast for RawItemMacro {
    fn is_raw(&self) -> bool {
        true
    }
}
impl_panic_serde!(RawItemMacro);

#[derive(Debug, Clone)]
pub struct RawType {
    pub raw: syn::TypePath,
}
impl Ast for RawType {
    fn is_raw(&self) -> bool {
        true
    }
}
impl_panic_serde!(RawType);

#[derive(Debug, Clone)]
pub struct RawUse {
    pub raw: syn::ItemUse,
}
impl Ast for RawUse {
    fn is_raw(&self) -> bool {
        true
    }
}
impl_panic_serde!(RawUse);

#[derive(Debug, Clone)]
pub struct RawImplTrait {
    pub raw: syn::TypeImplTrait,
}
impl Ast for RawImplTrait {
    fn is_raw(&self) -> bool {
        true
    }
}
impl_panic_serde!(RawImplTrait);

#[derive(Debug, Clone)]
pub struct RawExpr {
    pub raw: syn::Expr,
}
impl Ast for RawExpr {
    fn is_raw(&self) -> bool {
        true
    }
}
impl_panic_serde!(RawExpr);

#[derive(Debug, Clone)]
pub struct RawTokenSteam {
    pub raw: proc_macro2::TokenStream,
}
impl Ast for RawTokenSteam {
    fn is_raw(&self) -> bool {
        true
    }
}
impl_panic_serde!(RawTokenSteam);

pub struct RustSerde;
impl Serializer for RustSerde {
    fn serialize(&self, node: &Expr) -> Result<String> {
        RustPrinter.print_expr(node).map(|x| x.to_string())
    }
}
impl Deserializer for RustSerde {
    fn deserialize(&self, code: &str) -> Result<Expr> {
        let code: syn::File = parse_str(code)?;
        Ok(RustParser.parse_file(code)?.into())
    }
}
