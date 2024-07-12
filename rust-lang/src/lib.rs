use std::fmt::Debug;

use common::Result;
use lang_core::ast::*;
use serde::{Deserialize, Serialize};

use crate::parser::RustParser;
use crate::printer::RustPrinter;

pub mod parser;
pub mod printer;

macro_rules! unsafe_impl_send_sync {
    ($t: ty) => {
        unsafe impl Send for $t {}
        unsafe impl Sync for $t {}
    };
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RawExprMacro {
    pub raw: syn::ExprMacro,
}

unsafe_impl_send_sync!(RawExprMacro);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RawItemMacro {
    pub raw: syn::ItemMacro,
}
unsafe_impl_send_sync!(RawItemMacro);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RawStmtMacro {
    pub raw: syn::StmtMacro,
}
unsafe_impl_send_sync!(RawStmtMacro);
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RawType {
    pub raw: syn::TypePath,
}
unsafe_impl_send_sync!(RawType);

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
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RawUse {
    pub raw: syn::ItemUse,
}
unsafe_impl_send_sync!(RawUse);
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RawImplTrait {
    pub raw: syn::TypeImplTrait,
}
unsafe_impl_send_sync!(RawImplTrait);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RawExpr {
    pub raw: syn::Expr,
}
unsafe_impl_send_sync!(RawExpr);

#[derive(Debug, Clone)]
pub struct RawTokenSteam {
    pub raw: proc_macro2::TokenStream,
}
unsafe_impl_send_sync!(RawTokenSteam);
impl PartialEq for RawTokenSteam {
    fn eq(&self, other: &Self) -> bool {
        self.raw.to_string() == other.raw.to_string()
    }
}
impl Eq for RawTokenSteam {}
#[macro_export]
macro_rules! t {
    ($t: tt) => {};
}
#[derive(Debug, Clone, Eq, PartialEq, Copy)]
pub struct RustSerde {
    printer: RustPrinter,
    parser: RustParser,
}
impl RustSerde {
    pub fn new() -> Self {
        Self {
            printer: RustPrinter::new(),
            parser: RustParser::new(),
        }
    }
    pub fn set_rustfmt(&mut self, rustfmt: bool) {
        self.printer.rustfmt = rustfmt;
    }
}
impl AstSerializer for RustSerde {
    fn serialize_tree(&self, node: &AstTree) -> Result<String> {
        self.printer.serialize_tree(node)
    }

    fn serialize_expr(&self, node: &AstExpr) -> Result<String> {
        self.printer.serialize_expr(node)
    }

    fn serialize_invoke(&self, node: &ExprInvoke) -> Result<String> {
        self.printer.serialize_invoke(node)
    }

    fn serialize_item(&self, node: &AstItem) -> Result<String> {
        self.printer.serialize_item(node)
    }

    fn serialize_block(&self, node: &ExprBlock) -> Result<String> {
        self.printer.serialize_block(node)
    }

    fn serialize_module(&self, node: &Module) -> Result<String> {
        self.printer.serialize_module(node)
    }

    fn serialize_value(&self, node: &Value) -> Result<String> {
        self.printer.serialize_value(node)
    }

    fn serialize_type(&self, node: &AstType) -> Result<String> {
        self.printer.serialize_type(node)
    }

    fn serialize_stmt(&self, node: &Statement) -> Result<String> {
        self.printer.serialize_stmt(node)
    }

    fn serialize_function(&self, node: &ValueFunction) -> Result<String> {
        self.printer.serialize_function(node)
    }
}
impl AstDeserializer for RustSerde {
    fn deserialize_tree(&self, code: &str) -> Result<AstTree> {
        self.parser.deserialize_tree(code)
    }

    fn deserialize_expr(&self, code: &str) -> Result<AstExpr> {
        self.parser.deserialize_expr(code)
    }

    fn deserialize_item(&self, code: &str) -> Result<AstItem> {
        self.parser.deserialize_item(code)
    }

    fn deserialize_file(&self, path: &std::path::Path) -> Result<AstFile> {
        self.parser.deserialize_file(path)
    }
    fn deserialize_type(&self, code: &str) -> Result<AstType> {
        self.parser.deserialize_type(code)
    }
}
