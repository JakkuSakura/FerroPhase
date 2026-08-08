pub mod frontend;
pub mod js;
pub mod json;
pub mod package;
pub mod resolution;
pub mod ts;

pub use frontend::{
    ImportReference, ImportReferenceKind, TsParseMode, TypeScriptFrontend,
    collect_import_references,
};
pub use package::TypeScriptPackageProvider;
pub use package::{
    PackageJson, default_module_roots, estimate_module_path, estimate_module_path_with_roots,
    read_package_json,
};
pub use resolution::{ResolveOutcome, ResolvedModule, is_typescript_like_source, resolve_imports};
pub use ts::serializer::{JavaScriptSerializer, TypeScriptSerializer};
