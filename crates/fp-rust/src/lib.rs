use std::fmt::Debug;

use serde::{Deserialize, Serialize};

pub mod ast_inspector;
pub mod normalization;
pub mod package;
pub mod parser;
pub mod printer;
pub mod workspace;

pub use workspace::{parse_cargo_workspace, summarize_cargo_workspace};

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
macro_rules! fp {
    ($t: tt) => {};
}
