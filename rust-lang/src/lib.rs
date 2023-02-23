pub mod parser;
pub mod printer;
pub mod rustfmt;

use common::Result;
use common_lang::{Expr, *};

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
pub struct RustSerde;
impl Serializer for RustSerde {
    fn serialize(&self, node: &dyn AnyAst) -> Result<String> {
        self.serialize_expr(node).map(|x| x.to_string())
    }
}
impl Deserializer for RustSerde {
    fn deserialize(&self, code: &str) -> Result<Expr> {
        let code: syn::File = parse_str(code)?;
        self.deserialize_file(code)
    }
}
