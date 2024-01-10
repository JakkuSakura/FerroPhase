pub mod ast;
pub mod context;
pub mod helper;
pub mod interpreter;
pub mod ops;
pub mod optimizer;
pub mod passes;
mod serialize;
pub mod typing;
pub mod value;

use ast::*;
use common::*;
pub use serialize::*;

use crate::ast::Tree;

use crate::value::TypeValue;
use std::rc::Rc;
/// A macro to generate a common set of derives for a struct.
/// especially Clone, Debug, PartialEq, Eq, Hash
#[macro_export]
macro_rules! common_struct {
    (
        no_debug
        $(#[$attr:meta])*
        pub struct $name:ident { $($t:tt)* }
    ) => {
        #[derive(Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq, Hash)]
        $(#[$attr])*
        pub struct $name {
            $($t)*
        }
    };
    (

        $(#[$attr:meta])*
        pub struct $name:ident { $($t:tt)* }
    ) => {
        crate::common_struct!(
            no_debug
            $(#[$attr])*
            #[derive(Debug)]
            pub struct $name { $($t)* }
        );
    };
}
/// A macro to generate a common enum with a set of common traits.
/// especially From<Variant> for Enum
#[macro_export]
macro_rules! common_enum {
    (
        no_debug
        $(#[$attr:meta])*
        pub enum $name:ident { $($t:tt)* }
    ) => {
        #[derive(Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq, Hash, derive_from_one::FromOne)]
        $(#[$attr])*
        pub enum $name {
            $($t)*
        }

    };
    (
        $(#[$attr:meta])*
        pub enum $name:ident { $($t:tt)* }
    ) => {
        crate::common_enum!(
            no_debug
            $(#[$attr])*
            #[derive(Debug)]
            pub enum $name { $($t)* }
        );
    };
}

pub trait Deserializer {
    fn deserialize_tree(&self, code: &str) -> Result<Tree>;
    fn deserialize_expr(&self, code: &str) -> Result<Expr>;
    fn deserialize_item(&self, code: &str) -> Result<Item>;
    fn deserialize_file(&self, path: &std::path::Path) -> Result<File>;
    fn deserialize_type(&self, code: &str) -> Result<TypeValue>;
}
