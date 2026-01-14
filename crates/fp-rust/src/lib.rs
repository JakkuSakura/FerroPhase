use std::fmt::Debug;
use std::time::{SystemTime, UNIX_EPOCH};

use serde::{Deserialize, Serialize};

pub mod ast_inspector;
pub mod frontend;
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
pub struct RawTokenStream {
    pub raw: proc_macro2::TokenStream,
}
unsafe_impl_send_sync!(RawTokenStream);
impl PartialEq for RawTokenStream {
    fn eq(&self, other: &Self) -> bool {
        self.raw.to_string() == other.raw.to_string()
    }
}
impl Eq for RawTokenStream {}
#[macro_export]
macro_rules! fp {
    ($t: tt) => {};
}

/// Type-quoting helper used by generated Rust output.
///
/// `t! { ... }` is treated as a transparent wrapper around the contained tokens.
#[macro_export]
macro_rules! t {
    ($($tt:tt)*) => {
        $($tt)*
    };
}

// Runtime helpers used by generated Rust output.
//
// Note: stable Rust lacks general field/method reflection. These helpers are
// intentionally conservative so that transpiled examples remain compilable.

pub fn intrinsic_type_name<T>() -> &'static str {
    std::any::type_name::<T>()
}

pub fn intrinsic_field_count<T: 'static>() -> usize {
    let _ = std::any::TypeId::of::<T>();
    0
}

pub fn intrinsic_method_count<T: 'static>() -> usize {
    let _ = std::any::TypeId::of::<T>();
    0
}

pub fn intrinsic_has_field<T: 'static>(_name: &str) -> bool {
    let _ = std::any::TypeId::of::<T>();
    false
}

pub fn intrinsic_has_method<T: 'static>(_name: &str) -> bool {
    let _ = std::any::TypeId::of::<T>();
    false
}

pub fn intrinsic_time_now() -> f64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|duration| duration.as_secs_f64())
        .unwrap_or(0.0)
}
