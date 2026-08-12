use std::path::Path;
use std::sync::Arc;

use fp_core::package::provider::PackageProvider;
use fp_lang::magnet_provider::MagnetWorkspaceProvider;

/// Factory: maps a source language to a PackageProvider implementation.
pub fn provider_for_language(lang: &str, root: &Path) -> Option<Arc<dyn PackageProvider>> {
    match lang {
        "ferrophase" | "fp" => MagnetWorkspaceProvider::discover(root)
            .ok()
            .map(|p| Arc::new(p) as Arc<dyn PackageProvider>),
        "rust" | "rs" => Some(Arc::new(
            fp_rust::RustPackageProvider::new(root.to_path_buf()),
        )
            as Arc<dyn PackageProvider>),
        "c" | "cil" => Some(Arc::new(fp_c::package::CPackageProvider::new(
            root.to_path_buf(),
        ))
            as Arc<dyn PackageProvider>),
        "goasm" => Some(Arc::new(fp_goasm::package::GoPackageProvider::new(
            root.to_path_buf(),
        ))
            as Arc<dyn PackageProvider>),
        "typescript" | "ts" | "javascript" | "js" => {
            provider_typescript(root)
        }
        "python" | "py" => provider_python(root),
        "kotlin" | "kt" => provider_kotlin(root),
        "sycl" => provider_sycl(root),
        "zig" => provider_zig(root),
        "wit" => provider_wit(root),
        "go" | "golang" => provider_golang(root),
        "flatbuffers" => provider_flatbuffers(root),
        "hcl" => provider_hcl(root),
        "json" => provider_json(root),
        "jsonschema" => provider_jsonschema(root),
        "toml" => provider_toml(root),
        "sql" => provider_sql(root),
        "prql" => provider_prql(root),
        "csharp" | "cs" | "c#" => provider_csharp(root),
        "godot" | "gdscript" | "gd" => provider_godot(root),
        _ => None,
    }
}

#[cfg(feature = "lang-typescript")]
fn provider_typescript(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    Some(Arc::new(fp_typescript::TypeScriptPackageProvider::new(
        root.to_path_buf(),
    ))
        as Arc<dyn PackageProvider>)
}

#[cfg(not(feature = "lang-typescript"))]
fn provider_typescript(_root: &Path) -> Option<Arc<dyn PackageProvider>> {
    None
}

#[cfg(feature = "lang-python")]
fn provider_python(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    Some(Arc::new(fp_python::package::PythonPackageProvider::new(
        root.to_path_buf(),
    ))
        as Arc<dyn PackageProvider>)
}

#[cfg(not(feature = "lang-python"))]
fn provider_python(_root: &Path) -> Option<Arc<dyn PackageProvider>> {
    None
}

#[cfg(feature = "lang-kotlin")]
fn provider_kotlin(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    Some(Arc::new(fp_kotlin::package::KotlinPackageProvider::new(
        root.to_path_buf(),
    ))
        as Arc<dyn PackageProvider>)
}

#[cfg(not(feature = "lang-kotlin"))]
fn provider_kotlin(_root: &Path) -> Option<Arc<dyn PackageProvider>> {
    None
}

#[cfg(feature = "lang-sycl")]
fn provider_sycl(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    Some(Arc::new(fp_sycl::package::SyclPackageProvider::new(
        root.to_path_buf(),
    ))
        as Arc<dyn PackageProvider>)
}

#[cfg(not(feature = "lang-sycl"))]
fn provider_sycl(_root: &Path) -> Option<Arc<dyn PackageProvider>> {
    None
}

#[cfg(feature = "lang-zig")]
fn provider_zig(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    Some(Arc::new(fp_zig::package::ZigPackageProvider::new(
        root.to_path_buf(),
    ))
        as Arc<dyn PackageProvider>)
}

#[cfg(not(feature = "lang-zig"))]
fn provider_zig(_root: &Path) -> Option<Arc<dyn PackageProvider>> {
    None
}

#[cfg(feature = "lang-wit")]
fn provider_wit(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    Some(Arc::new(fp_wit::package::WitPackageProvider::new(
        root.to_path_buf(),
    ))
        as Arc<dyn PackageProvider>)
}

#[cfg(not(feature = "lang-wit"))]
fn provider_wit(_root: &Path) -> Option<Arc<dyn PackageProvider>> {
    None
}

#[cfg(feature = "lang-golang")]
fn provider_golang(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    Some(Arc::new(
        fp_golang::package::GoLangPackageProvider::new(root.to_path_buf()),
    )
        as Arc<dyn PackageProvider>)
}

#[cfg(not(feature = "lang-golang"))]
fn provider_golang(_root: &Path) -> Option<Arc<dyn PackageProvider>> {
    None
}

#[cfg(feature = "lang-flatbuffers")]
fn provider_flatbuffers(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    Some(Arc::new(
        fp_flatbuffers::package::FlatbuffersPackageProvider::new(root.to_path_buf()),
    )
        as Arc<dyn PackageProvider>)
}

#[cfg(not(feature = "lang-flatbuffers"))]
fn provider_flatbuffers(_root: &Path) -> Option<Arc<dyn PackageProvider>> {
    None
}

#[cfg(feature = "lang-hcl")]
fn provider_hcl(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    Some(Arc::new(fp_hcl::package::HclPackageProvider::new(
        root.to_path_buf(),
    ))
        as Arc<dyn PackageProvider>)
}

#[cfg(not(feature = "lang-hcl"))]
fn provider_hcl(_root: &Path) -> Option<Arc<dyn PackageProvider>> {
    None
}

#[cfg(feature = "lang-json")]
fn provider_json(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    Some(Arc::new(fp_json::package::JsonPackageProvider::new(
        root.to_path_buf(),
    ))
        as Arc<dyn PackageProvider>)
}

#[cfg(not(feature = "lang-json"))]
fn provider_json(_root: &Path) -> Option<Arc<dyn PackageProvider>> {
    None
}

#[cfg(feature = "lang-jsonschema")]
fn provider_jsonschema(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    Some(Arc::new(
        fp_jsonschema::package::JsonSchemaPackageProvider::new(root.to_path_buf()),
    )
        as Arc<dyn PackageProvider>)
}

#[cfg(not(feature = "lang-jsonschema"))]
fn provider_jsonschema(_root: &Path) -> Option<Arc<dyn PackageProvider>> {
    None
}

#[cfg(feature = "lang-toml")]
fn provider_toml(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    Some(Arc::new(fp_toml::package::TomlPackageProvider::new(
        root.to_path_buf(),
    ))
        as Arc<dyn PackageProvider>)
}

#[cfg(not(feature = "lang-toml"))]
fn provider_toml(_root: &Path) -> Option<Arc<dyn PackageProvider>> {
    None
}

#[cfg(feature = "lang-sql")]
fn provider_sql(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    Some(Arc::new(fp_sql::package::SqlPackageProvider::new(
        root.to_path_buf(),
    ))
        as Arc<dyn PackageProvider>)
}

#[cfg(not(feature = "lang-sql"))]
fn provider_sql(_root: &Path) -> Option<Arc<dyn PackageProvider>> {
    None
}

#[cfg(feature = "lang-prql")]
fn provider_prql(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    Some(Arc::new(fp_prql::package::PrqlPackageProvider::new(
        root.to_path_buf(),
    ))
        as Arc<dyn PackageProvider>)
}

#[cfg(not(feature = "lang-prql"))]
fn provider_prql(_root: &Path) -> Option<Arc<dyn PackageProvider>> {
    None
}

#[cfg(feature = "lang-csharp")]
fn provider_csharp(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    Some(Arc::new(fp_csharp::package::CSharpPackageProvider::new(
        root.to_path_buf(),
    ))
        as Arc<dyn PackageProvider>)
}

#[cfg(not(feature = "lang-csharp"))]
fn provider_csharp(_root: &Path) -> Option<Arc<dyn PackageProvider>> {
    None
}

#[cfg(feature = "lang-godot")]
fn provider_godot(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    Some(Arc::new(
        fp_godot::package::GodotPackageProvider::new(root.to_path_buf()),
    )
        as Arc<dyn PackageProvider>)
}

#[cfg(not(feature = "lang-godot"))]
fn provider_godot(_root: &Path) -> Option<Arc<dyn PackageProvider>> {
    None
}
