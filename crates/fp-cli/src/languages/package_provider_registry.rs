use std::path::Path;
use std::sync::Arc;

use fp_core::package::provider::PackageProvider;
use fp_lang::magnet_provider::MagnetWorkspaceProvider;

use super::backend_registry::LanguageProviderFactory;

/// Factory: maps a source language to a `PackageProvider` implementation.
/// A thin wrapper over `backend_registry::find_registered_language_provider` — the
/// built-ins below are registered into that exact same registry
/// (`builtin_language_providers`, seeded once), not looked up through a
/// separate match statement; an embedding binary's own
/// `register_language_provider` call sits in the same table, at the same
/// dispatch step.
pub fn provider_for_language(lang: &str, root: &Path) -> Option<Arc<dyn PackageProvider>> {
    super::backend_registry::find_registered_language_provider(lang, root)
}

fn factory<F>(f: F) -> LanguageProviderFactory
where
    F: Fn(&Path) -> Option<Arc<dyn PackageProvider>> + Send + Sync + 'static,
{
    Arc::new(f)
}

/// Every language `fp-cli` itself ships a provider for, as `(name,
/// factory)` pairs — the initial contents of the shared language-provider
/// registry (`backend_registry::language_provider_registry`'s `OnceLock` seed).
/// Feature-gated languages simply aren't pushed when their feature is
/// off, rather than registering a factory that always returns `None`.
pub(crate) fn builtin_language_providers() -> Vec<(&'static str, LanguageProviderFactory)> {
    let mut entries: Vec<(&'static str, LanguageProviderFactory)> = Vec::new();

    let ferrophase = factory(|root: &Path| {
        MagnetWorkspaceProvider::discover(root)
            .ok()
            .map(|p| Arc::new(p) as Arc<dyn PackageProvider>)
    });
    entries.push(("ferrophase", ferrophase.clone()));
    entries.push(("fp", ferrophase));

    let rust = factory(|root: &Path| {
        Some(Arc::new(fp_rust::RustPackageProvider::new(root.to_path_buf())) as Arc<dyn PackageProvider>)
    });
    entries.push(("rust", rust.clone()));
    entries.push(("rs", rust));

    // A native object file is never a manifest-based multi-file project —
    // the package name is just derived from the file itself, the same
    // way every other single-file provider does.
    entries.push((
        "object",
        factory(|root: &Path| {
            let bytes = std::fs::read(root).ok()?;
            let name = root
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("main")
                .to_string();
            fp_native::NativeObjectPackageProvider::new(fp_core::package::PackageId::new(name), &bytes)
                .ok()
                .map(|p| Arc::new(p) as Arc<dyn PackageProvider>)
        }),
    ));

    let c = factory(|root: &Path| {
        Some(Arc::new(fp_c::package::CPackageProvider::new(root.to_path_buf())) as Arc<dyn PackageProvider>)
    });
    entries.push(("c", c.clone()));
    entries.push(("cil", c));

    entries.push((
        "goasm",
        factory(|root: &Path| {
            Some(Arc::new(fp_goasm::package::GoPackageProvider::new(root.to_path_buf()))
                as Arc<dyn PackageProvider>)
        }),
    ));

    #[cfg(feature = "lang-typescript")]
    {
        let typescript = factory(|root: &Path| {
            Some(Arc::new(fp_typescript::TypeScriptPackageProvider::new(root.to_path_buf()))
                as Arc<dyn PackageProvider>)
        });
        entries.push(("typescript", typescript.clone()));
        entries.push(("ts", typescript.clone()));
        entries.push(("javascript", typescript.clone()));
        entries.push(("js", typescript));
    }

    #[cfg(feature = "lang-python")]
    {
        let python = factory(|root: &Path| {
            Some(Arc::new(fp_python::package::PythonPackageProvider::new(root.to_path_buf()))
                as Arc<dyn PackageProvider>)
        });
        entries.push(("python", python.clone()));
        entries.push(("py", python));
    }

    #[cfg(feature = "lang-kotlin")]
    {
        let kotlin = factory(|root: &Path| {
            Some(Arc::new(fp_kotlin::package::KotlinPackageProvider::new(root.to_path_buf()))
                as Arc<dyn PackageProvider>)
        });
        entries.push(("kotlin", kotlin.clone()));
        entries.push(("kt", kotlin));
    }

    #[cfg(feature = "lang-sycl")]
    entries.push((
        "sycl",
        factory(|root: &Path| {
            Some(Arc::new(fp_sycl::package::SyclPackageProvider::new(root.to_path_buf()))
                as Arc<dyn PackageProvider>)
        }),
    ));

    #[cfg(feature = "lang-zig")]
    entries.push((
        "zig",
        factory(|root: &Path| {
            Some(Arc::new(fp_zig::package::ZigPackageProvider::new(root.to_path_buf()))
                as Arc<dyn PackageProvider>)
        }),
    ));

    #[cfg(feature = "lang-wit")]
    entries.push((
        "wit",
        factory(|root: &Path| {
            Some(Arc::new(fp_wit::package::WitPackageProvider::new(root.to_path_buf()))
                as Arc<dyn PackageProvider>)
        }),
    ));

    #[cfg(feature = "lang-golang")]
    {
        let golang = factory(|root: &Path| {
            Some(Arc::new(fp_golang::package::GoLangPackageProvider::new(root.to_path_buf()))
                as Arc<dyn PackageProvider>)
        });
        entries.push(("go", golang.clone()));
        entries.push(("golang", golang));
    }

    #[cfg(feature = "lang-flatbuffers")]
    entries.push((
        "flatbuffers",
        factory(|root: &Path| {
            Some(Arc::new(fp_flatbuffers::package::FlatbuffersPackageProvider::new(
                root.to_path_buf(),
            )) as Arc<dyn PackageProvider>)
        }),
    ));

    #[cfg(feature = "lang-hcl")]
    entries.push((
        "hcl",
        factory(|root: &Path| {
            Some(Arc::new(fp_hcl::package::HclPackageProvider::new(root.to_path_buf()))
                as Arc<dyn PackageProvider>)
        }),
    ));

    #[cfg(feature = "lang-json")]
    entries.push((
        "json",
        factory(|root: &Path| {
            Some(Arc::new(fp_json::package::JsonPackageProvider::new(root.to_path_buf()))
                as Arc<dyn PackageProvider>)
        }),
    ));

    #[cfg(feature = "lang-jsonschema")]
    entries.push((
        "jsonschema",
        factory(|root: &Path| {
            Some(Arc::new(fp_jsonschema::package::JsonSchemaPackageProvider::new(
                root.to_path_buf(),
            )) as Arc<dyn PackageProvider>)
        }),
    ));

    #[cfg(feature = "lang-toml")]
    entries.push((
        "toml",
        factory(|root: &Path| {
            Some(Arc::new(fp_toml::package::TomlPackageProvider::new(root.to_path_buf()))
                as Arc<dyn PackageProvider>)
        }),
    ));

    #[cfg(feature = "lang-sql")]
    entries.push((
        "sql",
        factory(|root: &Path| {
            Some(Arc::new(fp_sql::package::SqlPackageProvider::new(root.to_path_buf()))
                as Arc<dyn PackageProvider>)
        }),
    ));

    #[cfg(feature = "lang-prql")]
    entries.push((
        "prql",
        factory(|root: &Path| {
            Some(Arc::new(fp_prql::package::PrqlPackageProvider::new(root.to_path_buf()))
                as Arc<dyn PackageProvider>)
        }),
    ));

    #[cfg(feature = "lang-csharp")]
    {
        let csharp = factory(|root: &Path| {
            Some(Arc::new(fp_csharp::package::CSharpPackageProvider::new(root.to_path_buf()))
                as Arc<dyn PackageProvider>)
        });
        entries.push(("csharp", csharp.clone()));
        entries.push(("cs", csharp.clone()));
        entries.push(("c#", csharp));
    }

    #[cfg(feature = "lang-godot")]
    {
        let godot = factory(|root: &Path| {
            Some(Arc::new(fp_godot::package::GodotPackageProvider::new(root.to_path_buf()))
                as Arc<dyn PackageProvider>)
        });
        entries.push(("godot", godot.clone()));
        entries.push(("gdscript", godot.clone()));
        entries.push(("gd", godot));
    }

    entries
}
