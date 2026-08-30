//! Kotlin source target transpilation support
//!
//! Walks the FerroPhase AST and emits idiomatic Kotlin source code.
//! Handles data classes, enum classes, functions, imports, and full expression trees.

pub mod kt_parser;
pub mod materialize;
pub mod package;
pub mod serializer;

pub use materialize::KotlinMaterializer;
pub use package::KotlinPackageProvider;
pub use serializer::{
    KotlinBackend, KotlinSerializer, KotlinWorkspaceContext, collect_enum_field_names,
    collect_enum_variant_names, collect_list_field_names, collect_mutated_field_names,
    collect_string_field_names,
};

/// What Kotlin can express directly, for `HirLoweringConfig`/`ast_to_hir`'s
/// shared, target-agnostic desugaring passes (closure defunctionalization,
/// `for`-loop index-`while` desugaring) to skip. Returned by
/// `KotlinBackend::capabilities` (`TargetBackend`'s trait method) —
/// `fp-cli` reads it there before compiling.
pub const CAPABILITIES: fp_core::capabilities::LanguageCapabilities =
    fp_core::capabilities::LanguageCapabilities {
        portable_operations: true,
        first_class_closures: true,
        first_class_for_loops: true,
    };
