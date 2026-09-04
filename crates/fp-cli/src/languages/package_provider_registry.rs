use std::path::Path;
use std::sync::Arc;

use fp_core::ast::package::provider::PackageProvider;
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
        Some(
            Arc::new(fp_rust::RustPackageProvider::new(root.to_path_buf()))
                as Arc<dyn PackageProvider>,
        )
    });
    entries.push(("rust", rust.clone()));
    entries.push(("rs", rust));

    entries.push(("object", factory(fp_native::package::object_provider)));
    entries.push(("archive", factory(fp_native::package::archive_provider)));

    entries.push((
        "c",
        factory(|root: &Path| {
            Some(
                Arc::new(fp_c::package::CPackageProvider::new(root.to_path_buf()))
                    as Arc<dyn PackageProvider>,
            )
        }),
    ));

    // Raw asm text has no manifest/project shape either — same one-file,
    // one-package treatment as `object`, just lifted from a parsed
    // `AsmX86_64Program`/`AsmAarch64Program` instead of a binary object.
    let native_asm_auto = factory(|root: &Path| {
        fp_native::package::asm_text_provider(root, fp_native::package::AsmDialect::Auto)
    });
    entries.push(("native-asm", native_asm_auto.clone()));
    entries.push(("asm", native_asm_auto));
    let native_asm_x86_64 = factory(|root: &Path| {
        fp_native::package::asm_text_provider(root, fp_native::package::AsmDialect::X86_64)
    });
    entries.push(("x86_64-asm", native_asm_x86_64.clone()));
    entries.push(("asm-x86_64", native_asm_x86_64.clone()));
    entries.push(("x86asm", native_asm_x86_64.clone()));
    entries.push(("x86_64asm", native_asm_x86_64));
    let native_asm_aarch64 = factory(|root: &Path| {
        fp_native::package::asm_text_provider(root, fp_native::package::AsmDialect::Aarch64)
    });
    entries.push(("aarch64-asm", native_asm_aarch64.clone()));
    entries.push(("asm-aarch64", native_asm_aarch64.clone()));
    entries.push(("arm64-asm", native_asm_aarch64.clone()));
    entries.push(("aarch64asm", native_asm_aarch64));

    entries.push(("goasm", factory(fp_goasm::package::file_provider)));
    entries.push(("urcl", factory(fp_urcl::package::file_provider)));
    entries.push(("jvm-bytecode", factory(fp_jvm::package::bytecode_provider)));
    #[cfg(feature = "lang-cil")]
    entries.push(("cil", factory(fp_cil::package::provider_for_path)));

    #[cfg(feature = "lang-typescript")]
    {
        let typescript = factory(|root: &Path| {
            Some(Arc::new(fp_typescript::TypeScriptPackageProvider::new(
                root.to_path_buf(),
            )) as Arc<dyn PackageProvider>)
        });
        entries.push(("typescript", typescript.clone()));
        entries.push(("ts", typescript.clone()));
        entries.push(("javascript", typescript.clone()));
        entries.push(("js", typescript));
    }

    #[cfg(feature = "lang-python")]
    {
        let python = factory(|root: &Path| {
            Some(Arc::new(fp_python::package::PythonPackageProvider::new(
                root.to_path_buf(),
            )) as Arc<dyn PackageProvider>)
        });
        entries.push(("python", python.clone()));
        entries.push(("py", python));
    }

    #[cfg(feature = "lang-kotlin")]
    {
        let kotlin = factory(|root: &Path| {
            Some(Arc::new(fp_kotlin::package::KotlinPackageProvider::new(
                root.to_path_buf(),
            )) as Arc<dyn PackageProvider>)
        });
        entries.push(("kotlin", kotlin.clone()));
        entries.push(("kt", kotlin));
    }

    #[cfg(feature = "lang-sycl")]
    entries.push((
        "sycl",
        factory(|root: &Path| {
            Some(Arc::new(fp_sycl::package::SyclPackageProvider::new(
                root.to_path_buf(),
            )) as Arc<dyn PackageProvider>)
        }),
    ));

    #[cfg(feature = "lang-zig")]
    entries.push((
        "zig",
        factory(|root: &Path| {
            Some(
                Arc::new(fp_zig::package::ZigPackageProvider::new(root.to_path_buf()))
                    as Arc<dyn PackageProvider>,
            )
        }),
    ));

    #[cfg(feature = "lang-wit")]
    entries.push((
        "wit",
        factory(|root: &Path| {
            Some(
                Arc::new(fp_wit::package::WitPackageProvider::new(root.to_path_buf()))
                    as Arc<dyn PackageProvider>,
            )
        }),
    ));

    #[cfg(feature = "lang-golang")]
    {
        let golang = factory(|root: &Path| {
            Some(Arc::new(fp_golang::package::GoLangPackageProvider::new(
                root.to_path_buf(),
            )) as Arc<dyn PackageProvider>)
        });
        entries.push(("go", golang.clone()));
        entries.push(("golang", golang));
    }

    #[cfg(feature = "lang-flatbuffers")]
    entries.push((
        "flatbuffers",
        factory(|root: &Path| {
            Some(
                Arc::new(fp_flatbuffers::package::FlatbuffersPackageProvider::new(
                    root.to_path_buf(),
                )) as Arc<dyn PackageProvider>,
            )
        }),
    ));

    #[cfg(feature = "lang-hcl")]
    entries.push((
        "hcl",
        factory(|root: &Path| {
            Some(
                Arc::new(fp_hcl::package::HclPackageProvider::new(root.to_path_buf()))
                    as Arc<dyn PackageProvider>,
            )
        }),
    ));

    #[cfg(feature = "lang-json")]
    entries.push((
        "json",
        factory(|root: &Path| {
            Some(Arc::new(fp_json::package::JsonPackageProvider::new(
                root.to_path_buf(),
            )) as Arc<dyn PackageProvider>)
        }),
    ));

    #[cfg(feature = "lang-jsonschema")]
    entries.push((
        "jsonschema",
        factory(|root: &Path| {
            Some(
                Arc::new(fp_jsonschema::package::JsonSchemaPackageProvider::new(
                    root.to_path_buf(),
                )) as Arc<dyn PackageProvider>,
            )
        }),
    ));

    #[cfg(feature = "lang-toml")]
    entries.push((
        "toml",
        factory(|root: &Path| {
            Some(Arc::new(fp_toml::package::TomlPackageProvider::new(
                root.to_path_buf(),
            )) as Arc<dyn PackageProvider>)
        }),
    ));

    #[cfg(feature = "lang-sql")]
    entries.push((
        "sql",
        factory(|root: &Path| {
            Some(
                Arc::new(fp_sql::package::SqlPackageProvider::new(root.to_path_buf()))
                    as Arc<dyn PackageProvider>,
            )
        }),
    ));

    #[cfg(feature = "lang-prql")]
    entries.push((
        "prql",
        factory(|root: &Path| {
            Some(Arc::new(fp_prql::package::PrqlPackageProvider::new(
                root.to_path_buf(),
            )) as Arc<dyn PackageProvider>)
        }),
    ));

    #[cfg(feature = "lang-csharp")]
    {
        let csharp = factory(|root: &Path| {
            Some(Arc::new(fp_csharp::package::CSharpPackageProvider::new(
                root.to_path_buf(),
            )) as Arc<dyn PackageProvider>)
        });
        entries.push(("csharp", csharp.clone()));
        entries.push(("cs", csharp.clone()));
        entries.push(("c#", csharp));
    }

    #[cfg(feature = "lang-godot")]
    {
        let godot = factory(|root: &Path| {
            Some(Arc::new(fp_godot::package::GodotPackageProvider::new(
                root.to_path_buf(),
            )) as Arc<dyn PackageProvider>)
        });
        entries.push(("godot", godot.clone()));
        entries.push(("gdscript", godot.clone()));
        entries.push(("gd", godot));
    }

    #[cfg(feature = "lang-lean")]
    entries.push((
        "lean",
        factory(|root: &Path| {
            Some(Arc::new(fp_lean::package::LeanPackageProvider::new(
                root.to_path_buf(),
            )) as Arc<dyn PackageProvider>)
        }),
    ));

    entries
}
