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

    entries.push((
        "c",
        factory(|root: &Path| {
            Some(Arc::new(fp_c::package::CPackageProvider::new(root.to_path_buf())) as Arc<dyn PackageProvider>)
        }),
    ));

    // Raw asm text has no manifest/project shape either — same one-file,
    // one-package treatment as `object`, just lifted from a parsed
    // `AsmX86_64Program`/`AsmAarch64Program` instead of a binary object.
    let native_asm_auto = factory(|root: &Path| native_asm_provider(root, NativeAsmDialect::Auto));
    entries.push(("native-asm", native_asm_auto.clone()));
    entries.push(("asm", native_asm_auto));
    let native_asm_x86_64 =
        factory(|root: &Path| native_asm_provider(root, NativeAsmDialect::X86_64));
    entries.push(("x86_64-asm", native_asm_x86_64.clone()));
    entries.push(("asm-x86_64", native_asm_x86_64.clone()));
    entries.push(("x86asm", native_asm_x86_64.clone()));
    entries.push(("x86_64asm", native_asm_x86_64));
    let native_asm_aarch64 =
        factory(|root: &Path| native_asm_provider(root, NativeAsmDialect::Aarch64));
    entries.push(("aarch64-asm", native_asm_aarch64.clone()));
    entries.push(("asm-aarch64", native_asm_aarch64.clone()));
    entries.push(("arm64-asm", native_asm_aarch64.clone()));
    entries.push(("aarch64asm", native_asm_aarch64));

    // A standalone `.goasm` file (not a project directory) is Go-style
    // native assembly text — lift it once at construction into a
    // target-independent `LirProgram`, the same one-package-one-item shape
    // `native_asm_provider` uses for `AsmProgram`, so every LIR-consuming
    // target (native/goasm/urcl/cil/dotnet/jvm-bytecode) can retarget it
    // with no backend-specific handling (`ItemKind::PrecompiledLir`).
    // `GoPackageProvider` (a real multi-file project provider, currently
    // unimplemented) still owns the directory case.
    entries.push((
        "goasm",
        factory(|root: &Path| {
            if root.is_file() {
                precompiled_lir_provider(root, |text| {
                    fp_goasm::parse_program(text).map(|(lir, _target)| lir)
                })
            } else {
                Some(Arc::new(fp_goasm::package::GoPackageProvider::new(root.to_path_buf()))
                    as Arc<dyn PackageProvider>)
            }
        }),
    ));

    // URCL has no project/directory shape at all — always a standalone
    // text file, same treatment as goasm above.
    entries.push((
        "urcl",
        factory(|root: &Path| precompiled_lir_provider(root, fp_urcl::parse_program)),
    ));

    // A `.class`/`.jar` file carries both a `PrecompiledArtifact` (raw
    // bytes, for byte-identical passthrough back to `--target
    // jvm-bytecode` — `fp_jvm::JvmBackend` checks for it before its
    // normal MIR-based path) and, best-effort, a `PrecompiledLir` (so
    // retargeting to native/goasm/urcl/cil/dotnet works the same generic
    // way goasm/URCL input already does).
    entries.push(("jvm-bytecode", factory(jvm_bytecode_provider)));

    // CIL text or an assembled `.dll`/`.exe` — same two-item shape:
    // `PrecompiledArtifact` for passthrough (`fp_cil::CilBackend` checks
    // for it, in both its `assemble: false`/`true` modes), plus a
    // best-effort `PrecompiledLir` when the input is text (binary PE
    // input has no lift path today, matching the previous pipeline's own
    // limitation).
    entries.push(("cil", factory(cil_provider)));

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

    #[cfg(feature = "lang-lean")]
    entries.push((
        "lean",
        factory(|root: &Path| {
            Some(Arc::new(fp_lean::package::LeanPackageProvider::new(root.to_path_buf()))
                as Arc<dyn PackageProvider>)
        }),
    ));

    entries
}

/// Which native asm dialect to parse `root`'s text as — `Auto` tries
/// x86_64 first, falling back to aarch64, matching every other
/// extension-detected language's "just figure it out" default.
enum NativeAsmDialect {
    Auto,
    X86_64,
    Aarch64,
}

/// Reads `root` as asm text, parses+lifts it to a target-independent
/// `AsmProgram` (the same `fp_native::asmir` machinery `fp_native::binary::
/// lift_object_to_asmir` uses for binary object files), and wraps it as a
/// one-package provider the same way `NativeObjectPackageProvider::new`
/// does for objects — `NativeEmitter::compile_package`/`emit_precompiled`
/// then retargets and emits it (as text, an object, or an executable,
/// depending on `BackendConfig`) without knowing or caring that it came
/// from text rather than a binary.
fn native_asm_provider(root: &Path, dialect: NativeAsmDialect) -> Option<Arc<dyn PackageProvider>> {
    use fp_native::asm::{aarch64::AsmAarch64Program, x86_64::AsmX86_64Program};
    use fp_native::asmir::{lift_from_aarch64, lift_from_x86_64};

    let text = std::fs::read_to_string(root).ok()?;
    let asm = match dialect {
        NativeAsmDialect::X86_64 => lift_from_x86_64(&AsmX86_64Program::parse_text(&text).ok()?).ok()?,
        NativeAsmDialect::Aarch64 => {
            lift_from_aarch64(&AsmAarch64Program::parse_text(&text).ok()?).ok()?
        }
        NativeAsmDialect::Auto => match AsmX86_64Program::parse_text(&text) {
            Ok(program) => lift_from_x86_64(&program).ok()?,
            Err(_) => lift_from_aarch64(&AsmAarch64Program::parse_text(&text).ok()?).ok()?,
        },
    };
    let name = root
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main")
        .to_string();
    Some(Arc::new(fp_native::NativeObjectPackageProvider::from_asm(
        fp_core::package::PackageId::new(name),
        asm,
    )) as Arc<dyn PackageProvider>)
}

/// Reads `root` as text, lifts it via `parse` into a target-independent
/// `LirProgram`, and wraps it as a one-package, one-item provider
/// (`ItemKind::PrecompiledLir`) — the LIR-shaped counterpart to
/// `native_asm_provider`'s `AsmProgram` shape, for any language whose
/// input already parses straight to LIR (goasm, URCL).
fn precompiled_lir_provider(
    root: &Path,
    parse: impl FnOnce(&str) -> fp_core::error::Result<fp_core::lir::LirProgram>,
) -> Option<Arc<dyn PackageProvider>> {
    let text = std::fs::read_to_string(root).ok()?;
    let lir = parse(&text).ok()?;
    let name = root
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main")
        .to_string();
    let package_id = fp_core::package::PackageId::new(name);
    let mut source = fp_core::package::PackageSource::new(
        package_id.clone(),
        package_id.as_str().to_string(),
        fp_core::package::graph::PackageGraph::new(Vec::new()),
    );
    source.items.push(fp_core::package::PackageItem {
        path: fp_core::ast::path::QualifiedPath::new(Vec::new()),
        item: fp_core::ast::Item::precompiled_lir(lir),
    });
    Some(Arc::new(fp_core::package::provider::FixedPackageProvider::for_source(
        package_id, source,
    )) as Arc<dyn PackageProvider>)
}

fn package_name_for(root: &Path) -> String {
    root.file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main")
        .to_string()
}

/// `.class`/`.jar` input: always carries the raw bytes as a
/// `PrecompiledArtifact` (`JvmBackend`'s passthrough path needs the
/// original bytes verbatim, not a lift-then-relower round trip); also
/// best-effort lifts to a `PrecompiledLir` — a single class parses
/// directly, a jar merges every member class's LIR into one program — so
/// retargeting to any other LIR-consuming backend works too. The lift is
/// best-effort: if it fails, the package still has its `PrecompiledArtifact`
/// item, so `--target jvm-bytecode` (the only thing tested against a
/// non-liftable class today) still works.
fn jvm_bytecode_provider(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    let bytes = std::fs::read(root).ok()?;
    let is_jar = bytes.starts_with(b"PK\x03\x04");
    let package_id = fp_core::package::PackageId::new(package_name_for(root));
    let mut source = fp_core::package::PackageSource::new(
        package_id.clone(),
        package_id.as_str().to_string(),
        fp_core::package::graph::PackageGraph::new(Vec::new()),
    );
    source.items.push(fp_core::package::PackageItem {
        path: fp_core::ast::path::QualifiedPath::new(Vec::new()),
        item: fp_core::ast::Item::precompiled_artifact(bytes.clone()),
    });
    let lir = if is_jar {
        fp_jvm::extract_class_files_from_jar(&bytes).ok().and_then(|classes| {
            let mut merged: Option<fp_core::lir::LirProgram> = None;
            for class in classes {
                let program = fp_jvm::parse_class_to_lir(&class.bytes).ok()?;
                match merged.as_mut() {
                    Some(merged_program) => merged_program.extend(program).ok()?,
                    None => merged = Some(program),
                }
            }
            merged
        })
    } else {
        fp_jvm::parse_class_to_lir(&bytes).ok()
    };
    if let Some(lir) = lir {
        source.items.push(fp_core::package::PackageItem {
            path: fp_core::ast::path::QualifiedPath::new(Vec::new()),
            item: fp_core::ast::Item::precompiled_lir(lir),
        });
    }
    Some(Arc::new(fp_core::package::provider::FixedPackageProvider::for_source(
        package_id, source,
    )) as Arc<dyn PackageProvider>)
}

/// CIL text or an assembled `.dll`/`.exe`: always carries the raw bytes as
/// a `PrecompiledArtifact` (`CilBackend`'s passthrough path, both
/// `assemble: false`/`true`, needs the original text/PE bytes verbatim);
/// text input also best-effort lifts to a `PrecompiledLir` for retargeting
/// to any other LIR-consuming backend. Binary PE input has no lift path —
/// matches the previous bespoke pipeline's own "binary -> native
/// transpilation is not implemented yet" limitation, just without a
/// bespoke error message for it (`merged_lir_program` errors naturally
/// instead when nothing retargets it).
fn cil_provider(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    let bytes = std::fs::read(root).ok()?;
    let is_pe = bytes.starts_with(b"MZ");
    let package_id = fp_core::package::PackageId::new(package_name_for(root));
    let mut source = fp_core::package::PackageSource::new(
        package_id.clone(),
        package_id.as_str().to_string(),
        fp_core::package::graph::PackageGraph::new(Vec::new()),
    );
    source.items.push(fp_core::package::PackageItem {
        path: fp_core::ast::path::QualifiedPath::new(Vec::new()),
        item: fp_core::ast::Item::precompiled_artifact(bytes.clone()),
    });
    if !is_pe {
        if let Ok(text) = String::from_utf8(bytes) {
            if let Ok(lir) = fp_cil::parse_cil_program(&text) {
                source.items.push(fp_core::package::PackageItem {
                    path: fp_core::ast::path::QualifiedPath::new(Vec::new()),
                    item: fp_core::ast::Item::precompiled_lir(lir),
                });
            }
        }
    }
    Some(Arc::new(fp_core::package::provider::FixedPackageProvider::for_source(
        package_id, source,
    )) as Arc<dyn PackageProvider>)
}
