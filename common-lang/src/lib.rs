extern crate core;

pub mod ast;
pub mod context;
pub mod cst;
mod deserialize;
pub mod expr;
pub mod id;
pub mod interpreter;
pub mod ops;
pub mod optimizer;
pub mod passes;
pub mod pat;
mod serialize;
pub mod ty;
pub mod utils;
pub mod value;
pub mod vm;

pub use deserialize::*;
pub use serialize::*;

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
