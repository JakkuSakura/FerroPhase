//! Kotlin source target transpilation support
//!
//! Walks the FerroPhase AST and emits idiomatic Kotlin source code.
//! Handles data classes, enum classes, functions, imports, and full expression trees.

pub mod kotlin_materializer;
pub mod package;
pub mod serializer;

pub use kotlin_materializer::KotlinMaterializer;
pub use package::KotlinPackageProvider;
pub use serializer::{
    KotlinSerializer, collect_list_field_names, collect_mutated_field_names,
    collect_string_field_names,
};
