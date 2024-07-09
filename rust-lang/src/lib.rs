pub mod macros;
pub mod parser;
pub mod printer;
pub mod rustfmt;

use crate::parser::RustParser;
use crate::printer::RustPrinter;
use common::Result;
use lang_core::ast::*;
use serde::{Deserialize, Serialize};
use std::fmt::Debug;
use std::path::PathBuf;
use syn::parse_str;
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
    rustfmt: bool,
}
impl RustSerde {
    pub fn new() -> Self {
        Self { rustfmt: false }
    }
    pub fn set_rustfmt(&mut self, rustfmt: bool) {
        self.rustfmt = rustfmt;
    }
    pub fn maybe_rustfmt_token_stream(&self, code: &proc_macro2::TokenStream) -> Result<String> {
        self.maybe_rustfmt(&code.to_string())
    }
    pub fn maybe_rustfmt(&self, code: &str) -> Result<String> {
        if self.rustfmt {
            if let Ok(ok) = rustfmt::format_code(code) {
                return Ok(ok);
            }
        }

        Ok(code.to_string())
    }
}
impl AstSerializer for RustSerde {
    fn serialize_tree(&self, node: &AstTree) -> Result<String> {
        RustPrinter
            .print_tree(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_expr(&self, node: &AstExpr) -> Result<String> {
        RustPrinter
            .print_expr(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_invoke(&self, node: &ExprInvoke) -> Result<String> {
        RustPrinter
            .print_invoke(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_item(&self, node: &AstItem) -> Result<String> {
        RustPrinter
            .print_item(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_block(&self, node: &ExprBlock) -> Result<String> {
        RustPrinter
            .print_block(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_module(&self, node: &Module) -> Result<String> {
        RustPrinter
            .print_module(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_value(&self, node: &Value) -> Result<String> {
        RustPrinter
            .print_value(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_type(&self, node: &AstType) -> Result<String> {
        RustPrinter
            .print_type_value(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_stmt(&self, node: &Statement) -> Result<String> {
        RustPrinter
            .print_statement(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_function(&self, node: &ValueFunction) -> Result<String> {
        RustPrinter
            .print_value_function(node, Visibility::Private)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }
}
impl AstDeserializer for RustSerde {
    fn deserialize_tree(&self, code: &str) -> Result<AstTree> {
        let code: syn::File = parse_str(code)?;
        let path = PathBuf::from("__file__");
        RustParser::new().parse_file(path, code).map(AstTree::File)
    }

    fn deserialize_expr(&self, code: &str) -> Result<AstExpr> {
        let code: syn::Expr = parse_str(code)?;
        RustParser::new().parse_expr(code)
    }

    fn deserialize_item(&self, code: &str) -> Result<AstItem> {
        let code: syn::Item = parse_str(code)?;
        RustParser::new().parse_item(code)
    }

    fn deserialize_file(&self, path: &std::path::Path) -> Result<AstFile> {
        RustParser::new().parse_file_recursively(path.to_owned())
    }
    fn deserialize_type(&self, code: &str) -> Result<AstType> {
        let code: syn::Type = parse_str(code)?;
        RustParser::new().parse_type_value(code)
    }
}
