pub mod parser;
pub mod printer;
pub mod rustfmt;

use common::Result;
use common_lang::tree::{Tree, *};
use common_lang::*;

use crate::parser::RustParser;
use crate::printer::RustPrinter;
use serde::{Deserialize, Serialize};
use std::fmt::Debug;
use syn::parse_str;

#[derive(Debug, Clone)]
pub struct RawExprMacro {
    pub raw: syn::ExprMacro,
}

#[derive(Debug, Clone)]
pub struct RawItemMacro {
    pub raw: syn::ItemMacro,
}

#[derive(Debug, Clone)]
pub struct RawType {
    pub raw: syn::TypePath,
}

impl Serialize for RawType {
    fn serialize<S>(&self, _serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        unreachable!()
    }
}
impl<'de> Deserialize<'de> for RawType {
    fn deserialize<D>(_deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        unreachable!()
    }
}
#[derive(Debug, Clone)]
pub struct RawUse {
    pub raw: syn::ItemUse,
}

#[derive(Debug, Clone)]
pub struct RawImplTrait {
    pub raw: syn::TypeImplTrait,
}

#[derive(Debug, Clone)]
pub struct RawExpr {
    pub raw: syn::Expr,
}

#[derive(Debug, Clone)]
pub struct RawTokenSteam {
    pub raw: proc_macro2::TokenStream,
}

pub struct RustSerde;
impl Serializer for RustSerde {
    fn serialize_tree(&self, node: &Tree) -> Result<String> {
        RustPrinter.print_tree(node).map(|x| x.to_string())
    }

    fn serialize_expr(&self, node: &Expr) -> Result<String> {
        RustPrinter.print_expr(node).map(|x| x.to_string())
    }

    fn serialize_invoke(&self, node: &InvokeExpr) -> Result<String> {
        RustPrinter.print_invoke(node).map(|x| x.to_string())
    }

    fn serialize_item(&self, node: &Item) -> Result<String> {
        RustPrinter.print_item(node).map(|x| x.to_string())
    }

    fn serialize_block(&self, node: &Block) -> Result<String> {
        RustPrinter.print_block(node).map(|x| x.to_string())
    }
}
impl Deserializer for RustSerde {
    fn deserialize(&self, code: &str) -> Result<Tree> {
        let code: syn::File = parse_str(code)?;
        RustParser
            .parse_file(code)
            .map(Item::Module)
            .map(Tree::Item)
    }
}
