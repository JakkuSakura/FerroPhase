pub mod frontend;
pub mod js;
pub mod json;
pub mod package;
pub mod resolution;
pub mod ts;

pub use frontend::{
    collect_import_references, ImportReference, ImportReferenceKind, TsParseMode,
    TypeScriptFrontend,
};
pub use package::{
    PackageJson, default_module_roots, estimate_module_path, estimate_module_path_with_roots,
    read_package_json,
};
pub use package::{TypeScriptModuleProvider, TypeScriptPackageProvider};
pub use resolution::{is_typescript_like_source, resolve_imports, ResolveOutcome, ResolvedModule};
pub use ts::serializer::{JavaScriptSerializer, TypeScriptSerializer};
