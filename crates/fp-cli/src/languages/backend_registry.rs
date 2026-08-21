//! A runtime extension point for compile targets *and* source-language
//! providers — there's no distinction between "built-in" and
//! "externally registered": every target (including every one fp-cli
//! itself ships, see `builtin_target_backends`) is just one more entry in
//! this same registry, looked up the same way. This is what lets an
//! embedding binary that lives outside `fp-cli`'s own Cargo workspace
//! (e.g. `skln-fp-graph`'s `fp-graph` binary, which can't be a dependency
//! of `fp-cli` without reversing the `FerroPhase` git submodule
//! relationship) add a target or source language fp-cli has no crate
//! dependency on, by registering one more factory before it calls
//! `commands::compile::compile_command`.
//!
//! Registered targets are plain `fp_core::backend::TargetBackend` impls —
//! the exact same trait every target implements, built-in or not — driven
//! through `compile_package`/`write_workspace_files` exactly the same way
//! regardless of where the factory came from. Registered source-language
//! providers are plain `fp_core::package::provider::PackageProvider`
//! factories, looked up the same way by
//! `package_provider_registry::provider_for_language`.

use std::path::Path;
use std::sync::{Arc, Mutex, OnceLock};

use fp_core::backend::{BackendConfig, TargetBackend};
use fp_core::package::provider::PackageProvider;

use crate::error::CliError;
use crate::Result;

/// A target-backend factory — every target needs a fresh `BackendConfig`
/// per compile (output path, target triple, ...), so this is a
/// constructor, not a pre-built value; a caller that wants to share one
/// underlying instance across compiles captures it in the closure itself.
pub type TargetBackendFactory =
    Arc<dyn Fn(BackendConfig) -> Result<Box<dyn TargetBackend>> + Send + Sync>;

static TARGET_BACKEND_REGISTRY: OnceLock<Mutex<Vec<(&'static str, TargetBackendFactory)>>> =
    OnceLock::new();

/// Seeded once with every built-in target's own factory
/// (`builtin_target_backends`) — built-ins and anything an embedding
/// binary later registers live in the exact same table, looked up the
/// exact same way.
fn target_backend_registry() -> &'static Mutex<Vec<(&'static str, TargetBackendFactory)>> {
    TARGET_BACKEND_REGISTRY.get_or_init(|| Mutex::new(builtin_target_backends()))
}

fn factory<F>(f: F) -> TargetBackendFactory
where
    F: Fn(BackendConfig) -> Result<Box<dyn TargetBackend>> + Send + Sync + 'static,
{
    Arc::new(f)
}

/// Appends `ext` to `path` only if `path` has no extension already — fp-cli
/// hands every target a path resolved from pure user intent (verbatim if
/// `-o` was explicit, otherwise a bare stem), so each target's own factory
/// closure calls this with its own default before constructing its backend,
/// rather than fp-cli guessing a target's extension by name up front.
fn fill_missing_extension(path: &std::path::Path, ext: &str) -> std::path::PathBuf {
    if path.extension().is_some() {
        path.to_path_buf()
    } else {
        path.with_extension(ext)
    }
}

/// `true` when `target_triple` (or, absent one, the host) is Windows —
/// needed by every target whose default extension differs between a
/// Windows PE (`.exe`) and everything else (`.out`).
fn is_windows_target(target_triple: Option<&str>) -> bool {
    let triple = match target_triple {
        Some(triple) => triple,
        None => return cfg!(target_os = "windows"),
    };
    triple.contains("windows") || triple.contains("msvc") || triple.contains("mingw")
}

/// Registers a target-backend factory so `--target <name>` resolves to
/// it. Expected to be called by the embedding binary's `main()` before it
/// calls `commands::compile::compile_command`.
pub fn register_target_backend(
    name: &'static str,
    factory: impl Fn(BackendConfig) -> Result<Box<dyn TargetBackend>> + Send + Sync + 'static,
) {
    target_backend_registry()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .push((name, Arc::new(factory)));
}

/// Looks up a previously registered (built-in or externally
/// `register_target_backend`-ed) factory by name, case-insensitively.
pub fn find_registered_target_backend(name: &str) -> Option<TargetBackendFactory> {
    let normalized = name.to_lowercase();
    target_backend_registry()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .iter()
        .find(|(registered_name, _)| registered_name.eq_ignore_ascii_case(&normalized))
        .map(|(_, factory)| factory.clone())
}

/// Constructs the `TargetBackend` for `name` — a plain registry lookup
/// followed by a call, nothing more.
pub fn backend_for_target(name: &str, config: BackendConfig) -> Result<Box<dyn TargetBackend>> {
    let factory = find_registered_target_backend(name)
        .ok_or_else(|| CliError::InvalidInput(format!("Unsupported target: {name}")))?;
    factory(config)
}

/// Whether `name` resolves to a registered target (built-in or external),
/// regardless of whether its crate is actually compiled into this build
/// (a feature-gated built-in still counts — its factory just returns a
/// "feature disabled" error when called).
pub fn is_known_target(name: &str) -> bool {
    find_registered_target_backend(name).is_some()
}

/// Error returned by a target factory whose crate is gated behind a
/// disabled optional `lang-*` feature (see e.g. `lang-typescript` in this
/// crate's `Cargo.toml`).
fn disabled_feature_error(feature: &str, what: &str) -> CliError {
    CliError::InvalidInput(format!(
        "{what} requires the \"{feature}\" feature, which is disabled in this build"
    ))
}

/// Every target `fp-cli` itself ships a backend for, as `(name, factory)`
/// pairs — the initial contents of the shared target-backend registry.
/// Feature-gated targets simply aren't pushed when their feature is off,
/// rather than registering a factory that always errors.
fn builtin_target_backends() -> Vec<(&'static str, TargetBackendFactory)> {
    let mut entries: Vec<(&'static str, TargetBackendFactory)> = Vec::new();

    entries.push((
        "native",
        factory(|config: BackendConfig| {
            // Own default: assembly text when asked to emit text, an
            // executable (`.exe` on Windows, `.out` elsewhere) when linking
            // was requested, otherwise a relocatable object — losing, versus
            // the object-vs-archive distinction fp-cli used to make by
            // sniffing the *input*'s container kind, only for the rare case
            // of an unlinked native re-emission with no explicit `-o<ext>`
            // (both now default to `.o`); every explicit `-o` is untouched
            // regardless.
            let default_ext = if config.emit_text {
                "s"
            } else if config.link_requested {
                if is_windows_target(config.target_triple.as_deref()) {
                    "exe"
                } else {
                    crate::languages::backend::DEFAULT_TARGET_OUTPUT_EXTENSION
                }
            } else {
                "o"
            };
            let output = fill_missing_extension(&config.workspace_root, default_ext);
            let native_target = match config.native_target.as_deref() {
                Some(value) => Some(
                    fp_native::config::NativeTarget::resolve(value, config.target_triple.as_deref())
                        .ok_or_else(|| {
                            CliError::Compilation(format!("Unsupported fp-native target: {value}"))
                        })?,
                ),
                None => None,
            };
            let cfg = if config.emit_text {
                fp_native::config::NativeConfig::assembly(&output)
            } else if config.link_requested {
                fp_native::config::NativeConfig::executable(&output)
            } else {
                fp_native::config::NativeConfig::object(&output)
            }
            .with_target_triple(config.target_triple.clone())
            .with_target_cpu(config.target_cpu.clone())
            .with_native_target(native_target)
            .with_target_features(config.target_features.clone())
            .with_sysroot(config.target_sysroot.clone())
            .with_fuse_ld(config.target_linker.clone())
            .with_linker_driver(Some(config.linker.clone()))
            .with_release(config.release)
            .with_save_intermediates(config.save_intermediates);
            let emitter = fp_native::NativeEmitter::new(cfg);
            Ok(Box::new(emitter) as Box<dyn TargetBackend>)
        }),
    ));

    entries.push((
        "goasm",
        factory(|config: BackendConfig| {
            let output = fill_missing_extension(&config.workspace_root, "s");
            let target = Some(fp_goasm::config::GoAsmTarget::resolve(
                config.target_triple.as_deref(),
            ));
            let cfg = fp_goasm::config::GoAsmConfig::new(&output)
                .with_target(target)
                .with_target_triple(config.target_triple.clone());
            Ok(Box::new(fp_goasm::GoAsmEmitter::new(cfg)) as Box<dyn TargetBackend>)
        }),
    ));

    entries.push((
        "urcl",
        factory(|config: BackendConfig| {
            let output = fill_missing_extension(&config.workspace_root, "urcl");
            Ok(
                Box::new(fp_urcl::UrclEmitter::new(fp_urcl::UrclConfig::new(&output)))
                    as Box<dyn TargetBackend>,
            )
        }),
    ));

    entries.push((
        "llvm-binary",
        factory(|config: BackendConfig| llvm_backend(config, false)),
    ));
    entries.push((
        "llvm-text",
        factory(|config: BackendConfig| llvm_backend(config, true)),
    ));

    entries.push((
        "cranelift",
        factory(|config: BackendConfig| {
            #[cfg(feature = "cranelift")]
            {
                let default_ext = if is_windows_target(config.target_triple.as_deref()) {
                    "exe"
                } else {
                    crate::languages::backend::DEFAULT_TARGET_OUTPUT_EXTENSION
                };
                let output = fill_missing_extension(&config.workspace_root, default_ext);
                Ok(Box::new(fp_cranelift::CraneliftBackend {
                    output,
                    target_triple: config.target_triple.clone(),
                    target_cpu: config.target_cpu.clone(),
                    target_features: config.target_features.clone(),
                    target_sysroot: config.target_sysroot.clone(),
                    linker: Some(config.linker.clone()),
                    target_linker: config.target_linker.clone(),
                    release: config.release,
                    save_intermediates: config.save_intermediates,
                }) as Box<dyn TargetBackend>)
            }
            #[cfg(not(feature = "cranelift"))]
            {
                let _ = config;
                Err(CliError::MissingDependency(
                    "Feature 'cranelift' is disabled; enable it to use the Cranelift backend."
                        .to_string(),
                ))
            }
        }),
    ));

    entries.push((
        "bytecode",
        factory(|config: BackendConfig| {
            Ok(Box::new(fp_stackvm_bytecode::BytecodeBackend {
                output: fill_missing_extension(&config.workspace_root, "fbc"),
                emit_text: false,
                save_intermediates: config.save_intermediates,
            }) as Box<dyn TargetBackend>)
        }),
    ));
    entries.push((
        "text-bytecode",
        factory(|config: BackendConfig| {
            Ok(Box::new(fp_stackvm_bytecode::BytecodeBackend {
                output: fill_missing_extension(&config.workspace_root, "ftbc"),
                // `emit_text` only forces text mode for the explicit
                // "text-bytecode" target name — `compile_package`'s own
                // `wants_text` already falls back to sniffing `.ftbc` off
                // `output`, so fp-cli doesn't need to duplicate that here.
                emit_text: true,
                save_intermediates: config.save_intermediates,
            }) as Box<dyn TargetBackend>)
        }),
    ));

    entries.push((
        "jvm-bytecode",
        factory(|config: BackendConfig| {
            Ok(Box::new(fp_jvm::JvmBackend {
                output: fill_missing_extension(&config.workspace_root, "class"),
                save_intermediates: config.save_intermediates,
            }) as Box<dyn TargetBackend>)
        }),
    ));

    entries.push((
        "wasm",
        factory(|config: BackendConfig| {
            Ok(Box::new(fp_wasm::WasmBackend {
                output: fill_missing_extension(&config.workspace_root, "wasm"),
            }) as Box<dyn TargetBackend>)
        }),
    ));

    entries.push((
        "ebpf",
        factory(|config: BackendConfig| {
            let default_ext = if config.exec_requested { "o" } else { "ebpf" };
            Ok(Box::new(fp_ebpf::EbpfBackend {
                output: fill_missing_extension(&config.workspace_root, default_ext),
            }) as Box<dyn TargetBackend>)
        }),
    ));

    entries.push((
        "cil",
        factory(|config: BackendConfig| {
            Ok(Box::new(fp_dotnet::CilBackend {
                output: fill_missing_extension(&config.workspace_root, "il"),
            }) as Box<dyn TargetBackend>)
        }),
    ));

    entries.push((
        "dotnet",
        factory(|config: BackendConfig| {
            Ok(Box::new(fp_dotnet::DotnetBackend {
                output: fill_missing_extension(&config.workspace_root, "exe"),
                save_intermediates: config.save_intermediates,
            }) as Box<dyn TargetBackend>)
        }),
    ));

    entries.push((
        "interpret",
        factory(|_config: BackendConfig| {
            Ok(Box::new(fp_interpret::InterpreterBackend) as Box<dyn TargetBackend>)
        }),
    ));

    let ferrophase: TargetBackendFactory = factory(|config: BackendConfig| {
        Ok(Box::new(fp_c::FerroPhaseAstBackend::new(config)) as Box<dyn TargetBackend>)
    });
    entries.push(("fp", ferrophase.clone()));
    entries.push(("ferro", ferrophase.clone()));
    entries.push(("ferrophase", ferrophase));

    let typescript: TargetBackendFactory = factory(|config: BackendConfig| {
        #[cfg(feature = "lang-typescript")]
        {
            Ok(Box::new(fp_typescript::TypeScriptBackend::new(config)) as Box<dyn TargetBackend>)
        }
        #[cfg(not(feature = "lang-typescript"))]
        {
            let _ = config;
            Err(disabled_feature_error(
                "lang-typescript",
                "TypeScript package emission",
            ))
        }
    });
    entries.push(("typescript", typescript.clone()));
    entries.push(("ts", typescript));

    let javascript: TargetBackendFactory = factory(|config: BackendConfig| {
        #[cfg(feature = "lang-typescript")]
        {
            Ok(Box::new(fp_typescript::JavaScriptBackend::new(config)) as Box<dyn TargetBackend>)
        }
        #[cfg(not(feature = "lang-typescript"))]
        {
            let _ = config;
            Err(disabled_feature_error(
                "lang-typescript",
                "JavaScript package emission",
            ))
        }
    });
    entries.push(("javascript", javascript.clone()));
    entries.push(("js", javascript));

    let csharp: TargetBackendFactory = factory(|config: BackendConfig| {
        #[cfg(feature = "lang-csharp")]
        {
            Ok(Box::new(fp_csharp::CSharpBackend::new(config)) as Box<dyn TargetBackend>)
        }
        #[cfg(not(feature = "lang-csharp"))]
        {
            let _ = config;
            Err(disabled_feature_error("lang-csharp", "C# package emission"))
        }
    });
    entries.push(("csharp", csharp.clone()));
    entries.push(("cs", csharp.clone()));
    entries.push(("c#", csharp));

    let kotlin: TargetBackendFactory = factory(|config: BackendConfig| {
        #[cfg(feature = "lang-kotlin")]
        {
            Ok(Box::new(fp_kotlin::KotlinBackend::new(config)) as Box<dyn TargetBackend>)
        }
        #[cfg(not(feature = "lang-kotlin"))]
        {
            let _ = config;
            Err(disabled_feature_error("lang-kotlin", "Kotlin package emission"))
        }
    });
    entries.push(("kotlin", kotlin.clone()));
    entries.push(("kt", kotlin));

    let python: TargetBackendFactory = factory(|config: BackendConfig| {
        #[cfg(feature = "lang-python")]
        {
            Ok(Box::new(fp_python::PythonBackend::new(config)) as Box<dyn TargetBackend>)
        }
        #[cfg(not(feature = "lang-python"))]
        {
            let _ = config;
            Err(disabled_feature_error("lang-python", "Python package emission"))
        }
    });
    entries.push(("python", python.clone()));
    entries.push(("py", python));

    let golang: TargetBackendFactory = factory(|config: BackendConfig| {
        #[cfg(feature = "lang-golang")]
        {
            Ok(Box::new(fp_golang::GoBackend::new(config)) as Box<dyn TargetBackend>)
        }
        #[cfg(not(feature = "lang-golang"))]
        {
            let _ = config;
            Err(disabled_feature_error("lang-golang", "Go package emission"))
        }
    });
    entries.push(("go", golang.clone()));
    entries.push(("golang", golang));

    let gdscript: TargetBackendFactory = factory(|config: BackendConfig| {
        #[cfg(feature = "lang-godot")]
        {
            Ok(Box::new(fp_godot::GdscriptBackend::new(config)) as Box<dyn TargetBackend>)
        }
        #[cfg(not(feature = "lang-godot"))]
        {
            let _ = config;
            Err(disabled_feature_error("lang-godot", "GDScript package emission"))
        }
    });
    entries.push(("gdscript", gdscript.clone()));
    entries.push(("gd", gdscript));

    entries.push((
        "zig",
        factory(|config: BackendConfig| {
            #[cfg(feature = "lang-zig")]
            {
                Ok(Box::new(fp_zig::ZigBackend::new(config)) as Box<dyn TargetBackend>)
            }
            #[cfg(not(feature = "lang-zig"))]
            {
                let _ = config;
                Err(disabled_feature_error("lang-zig", "Zig package emission"))
            }
        }),
    ));

    entries.push((
        "sycl",
        factory(|config: BackendConfig| {
            #[cfg(feature = "lang-sycl")]
            {
                Ok(Box::new(fp_sycl::SyclBackend::new(config)) as Box<dyn TargetBackend>)
            }
            #[cfg(not(feature = "lang-sycl"))]
            {
                let _ = config;
                Err(disabled_feature_error("lang-sycl", "SYCL package emission"))
            }
        }),
    ));

    let rust: TargetBackendFactory = factory(|config: BackendConfig| {
        Ok(Box::new(fp_lang::RustBackend::new(config)) as Box<dyn TargetBackend>)
    });
    entries.push(("rust", rust.clone()));
    entries.push(("rs", rust));

    entries.push((
        "wit",
        factory(|config: BackendConfig| {
            #[cfg(feature = "lang-wit")]
            {
                Ok(Box::new(fp_wit::WitBackend::new(config)) as Box<dyn TargetBackend>)
            }
            #[cfg(not(feature = "lang-wit"))]
            {
                let _ = config;
                Err(disabled_feature_error("lang-wit", "WIT package emission"))
            }
        }),
    ));

    entries.push((
        "c",
        factory(|config: BackendConfig| {
            Ok(Box::new(fp_c::codegen::CBackend::new(config)) as Box<dyn TargetBackend>)
        }),
    ));

    entries
}

fn llvm_backend(config: BackendConfig, text_only: bool) -> Result<Box<dyn TargetBackend>> {
    #[cfg(feature = "llvm")]
    {
        let default_ext = if text_only {
            "ll"
        } else if is_windows_target(config.target_triple.as_deref()) {
            "exe"
        } else {
            crate::languages::backend::DEFAULT_TARGET_OUTPUT_EXTENSION
        };
        Ok(Box::new(fp_llvm::LlvmBackend {
            output: fill_missing_extension(&config.workspace_root, default_ext),
            target_triple: config.target_triple.clone(),
            target_cpu: config.target_cpu.clone(),
            target_features: config.target_features.clone(),
            target_sysroot: config.target_sysroot.clone(),
            linker: Some(config.linker.clone()),
            target_linker: config.target_linker.clone(),
            release: config.release,
            debug_info: config.debug_info,
            save_intermediates: config.save_intermediates,
            text_only,
        }) as Box<dyn TargetBackend>)
    }
    #[cfg(not(feature = "llvm"))]
    {
        let _ = (config, text_only);
        Err(CliError::MissingDependency(
            "Feature 'llvm' is disabled; enable it to use the LLVM backend.".to_string(),
        ))
    }
}

/// A source-language provider factory — `root` is either a project
/// directory or a single standalone file (mirroring every built-in
/// per-language provider's own `::new`/`::discover`, e.g.
/// `fp_rust::RustPackageProvider`); returns `None` if `root` isn't
/// something this language's provider can handle. Boxed so a caller can
/// register a plain closure without defining a named type.
pub type LanguageProviderFactory =
    Arc<dyn Fn(&Path) -> Option<Arc<dyn PackageProvider>> + Send + Sync>;

static LANGUAGE_PROVIDER_REGISTRY: OnceLock<Mutex<Vec<(&'static str, LanguageProviderFactory)>>> =
    OnceLock::new();

/// Seeded once with every built-in language's own factory
/// (`package_provider_registry::builtin_language_providers`) — built-ins
/// and anything an embedding binary later registers live in the exact
/// same table, looked up the exact same way.
fn language_provider_registry() -> &'static Mutex<Vec<(&'static str, LanguageProviderFactory)>> {
    LANGUAGE_PROVIDER_REGISTRY
        .get_or_init(|| Mutex::new(super::package_provider_registry::builtin_language_providers()))
}

/// Registers a `PackageProvider` factory for `name` so `--source-language
/// <name>` (or extension-based auto-detection, once also registered in
/// `languages::SUPPORTED_LANGUAGES`) resolves to it — the source-provider
/// analogue of `register_target_backend`, for an embedding binary that
/// wants to add a language `fp-cli` itself has no crate dependency on.
/// Expected to be called from the embedding binary's `main()` before it
/// calls `commands::compile::compile_command`.
pub fn register_language_provider(
    name: &'static str,
    factory: impl Fn(&Path) -> Option<Arc<dyn PackageProvider>> + Send + Sync + 'static,
) {
    language_provider_registry()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .push((name, Arc::new(factory)));
}

/// Looks up a previously `register_language_provider`-ed factory by name,
/// case-insensitively, and invokes it with `root`.
pub fn find_registered_language_provider(
    name: &str,
    root: &Path,
) -> Option<Arc<dyn PackageProvider>> {
    let normalized = name.to_lowercase();
    let factory = language_provider_registry()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .iter()
        .find(|(registered_name, _)| registered_name.eq_ignore_ascii_case(&normalized))
        .map(|(_, factory)| factory.clone())?;
    factory(root)
}
