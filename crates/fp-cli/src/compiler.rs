use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;

use fp_c::CFrontend;
use fp_compiler::{
    CompilerDriver, CompilerExecutor, CompilerSession, ConstValueId, FullyQualifiedPath, LirId,
    PipelineMode,
};
use fp_core::ast::path::QualifiedPath;
use fp_core::package::provider::PackageProvider;
use fp_core::package::{PackageId, PackageSource};
use fp_core::{
    ast::{
        Expr, ExprBlock, File, Ident, Item, ItemDefConst, ItemDefFunction, ItemKind, ScriptBlock,
        Value, Visibility,
    },
    diagnostics::{Diagnostic, DiagnosticDisplayOptions, DiagnosticLevel, DiagnosticManager},
    frontend::{FrontendParseMode, FrontendResult, LanguageFrontend},
    lir::LirDataLayout,
};
use fp_goasm::config::GoAsmTarget;
use fp_lang::FerroFrontend;
use fp_typing::{TypingDiagnostic, TypingDiagnosticLevel};

#[cfg(feature = "lang-flatbuffers")]
use crate::languages::frontend::FlatbuffersFrontend;
#[cfg(feature = "lang-golang")]
use crate::languages::frontend::GoFrontend;
#[cfg(feature = "lang-hcl")]
use crate::languages::frontend::HclFrontend;
#[cfg(feature = "lang-json")]
use crate::languages::frontend::JsonFrontend;
#[cfg(feature = "lang-jsonschema")]
use crate::languages::frontend::JsonSchemaFrontend;
#[cfg(feature = "lang-prql")]
use crate::languages::frontend::PrqlFrontend;
#[cfg(feature = "lang-python")]
use crate::languages::frontend::PythonFrontend;
#[cfg(feature = "lang-sql")]
use crate::languages::frontend::SqlFrontend;
#[cfg(feature = "lang-toml")]
use crate::languages::frontend::TomlFrontend;
#[cfg(feature = "lang-typescript")]
use crate::languages::frontend::TypeScriptFrontend;
#[cfg(feature = "lang-wit")]
use crate::languages::frontend::WitFrontend;
use crate::languages::single_file::{in_memory_provider, single_file_provider};
use crate::languages::{self, detect_source_language};
use crate::{CliError, Result};
#[cfg(feature = "lang-typescript")]
use fp_typescript::frontend::TsParseMode;

fn data_layout() -> LirDataLayout {
    LirDataLayout::new(
        64,
        8,
        vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
    )
    .expect("valid CLI data layout")
}

pub fn check_path(
    path: &Path,
    package: &str,
    syntax_only: bool,
    lossy: LossyCompileOptions,
) -> Result<()> {
    if syntax_only {
        // A pure syntax check never touches the package/compile pipeline at
        // all, so it stays a direct parse rather than resolving a package
        // for work that's about to be thrown away.
        parse_file(path, None, lossy)?;
        return Ok(());
    }

    let language = resolve_source_language(path, None)?;
    let executor = CompilerExecutor::new();
    let identity = CompilerIdentity::for_file(package, path);
    let mut driver = compile_source_file(
        SourceInput::Path(path.to_path_buf()),
        &language,
        &identity,
        lossy,
        &executor,
        PipelineMode::Native,
    )?;
    drain_driver(&mut driver, lossy)
}

pub fn eval_script(script: ScriptBlock) -> Result<Value> {
    let body = ExprBlock::new_stmts(script.stmts);
    let eval_const = ItemDefConst {
        attrs: Vec::new(),
        mutable: None,
        ty_annotation: None,
        visibility: Visibility::Private,
        name: Ident::new("__eval_result"),
        ty: None,
        value: Expr::block(body.clone()).into(),
    };
    let main = ItemDefFunction::new_simple(Ident::new("main"), ExprBlock::new_expr(Expr::unit()));
    let ast = File {
        path: PathBuf::from("<eval>"),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![
            Item::new(ItemKind::DefFunction(main)),
            Item::new(ItemKind::DefConst(eval_const)),
        ],
    };
    let identity = CompilerIdentity::for_script();
    let executor = CompilerExecutor::new();
    let mut driver = compile_source_file(
        SourceInput::InMemory(ast),
        languages::FERROPHASE,
        &identity,
        LossyCompileOptions::default(),
        &executor,
        PipelineMode::Native,
    )?;
    drain_driver(&mut driver, LossyCompileOptions::default())?;
    if let Some((_, value)) = driver
        .state
        .borrow()
        .typing_ctx
        .resolved_consts
        .borrow()
        .iter()
        .find(|(key, _)| key.contains("__eval_result"))
    {
        return Ok(value.clone());
    }
    driver
        .state
        .borrow()
        .const_value(&ConstValueId::new(format!(
            "const_value:{}",
            identity.path.to_key()
        )))
        .map(|value| value.clone())
        .map_err(|error| CliError::Compilation(error.to_string()))
}

pub fn interpret_file(path: &Path, package: &str) -> Result<Value> {
    let language = resolve_source_language(path, None)?;
    execute_ast(
        SourceInput::Path(path.to_path_buf()),
        &language,
        CompilerIdentity::for_file(package, path),
        fp_core::context::ExecutionMode::Runtime,
        LossyCompileOptions::default(),
    )
}

pub struct NativeCompileOptions {
    pub emitter: NativeEmitterKind,
    pub output: PathBuf,
    pub target_triple: Option<String>,
    pub target_cpu: Option<String>,
    pub native_target: Option<String>,
    pub target_features: Option<String>,
    pub target_sysroot: Option<PathBuf>,
    pub linker: Option<String>,
    pub target_linker: Option<PathBuf>,
    pub release: bool,
    pub save_intermediates: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NativeEmitterKind {
    Native,
    GoAsm,
    Urcl,
}

pub struct BytecodeCompileOptions {
    pub output: PathBuf,
    pub emit_text: bool,
    pub save_intermediates: bool,
}

pub struct JvmCompileOptions {
    pub output: PathBuf,
    pub save_intermediates: bool,
    pub class_name_hint: Option<String>,
}

pub struct WasmCompileOptions {
    pub output: PathBuf,
}

pub struct EbpfCompileOptions {
    pub output: PathBuf,
}

pub struct LlvmCompileOptions {
    pub output: PathBuf,
    pub target_triple: Option<String>,
    pub target_cpu: Option<String>,
    pub target_features: Option<String>,
    pub target_sysroot: Option<PathBuf>,
    pub linker: Option<String>,
    pub target_linker: Option<PathBuf>,
    pub release: bool,
    pub debug_info: bool,
    pub module_name: String,
    pub save_intermediates: bool,
}

pub struct CraneliftCompileOptions {
    pub output: PathBuf,
    pub target_triple: Option<String>,
    pub target_cpu: Option<String>,
    pub target_features: Option<String>,
    pub target_sysroot: Option<PathBuf>,
    pub linker: Option<String>,
    pub target_linker: Option<PathBuf>,
    pub release: bool,
    pub save_intermediates: bool,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct LossyCompileOptions {
    pub enabled: bool,
}

#[derive(Debug, Clone)]
pub struct FrontendBundle {
    pub source_language: String,
}

#[derive(Debug, Clone)]
pub struct MirBundle {
    pub frontend: FrontendBundle,
    pub hir_program: fp_core::hir::Program,
    pub mir_program: fp_core::mir::Program,
}

#[derive(Debug, Clone)]
pub struct LirBundle {
    pub frontend: FrontendBundle,
    pub hir_program: fp_core::hir::Program,
    pub mir_program: fp_core::mir::Program,
    pub lir_program: fp_core::lir::LirProgram,
}

pub fn compile_native_file(
    path: &Path,
    package: &str,
    source_language: Option<&str>,
    lossy: LossyCompileOptions,
    options: &NativeCompileOptions,
) -> Result<PathBuf> {
    let lowered = lower_file(path, package, source_language, lossy)?;
    let lir = lowered.lir()?;

    match options.emitter {
        NativeEmitterKind::Native => {
            let native_target = match options.native_target.as_deref() {
                Some(value) => Some(
                    fp_native::config::NativeTarget::resolve(
                        value,
                        options.target_triple.as_deref(),
                    )
                    .ok_or_else(|| {
                        CliError::Compilation(format!("Unsupported fp-native target: {}", value))
                    })?,
                ),
                None => None,
            };

            let mut cfg = fp_native::config::NativeConfig::executable(&options.output)
                .with_target_triple(options.target_triple.clone())
                .with_target_cpu(options.target_cpu.clone())
                .with_native_target(native_target)
                .with_target_features(options.target_features.clone())
                .with_sysroot(options.target_sysroot.clone())
                .with_fuse_ld(options.target_linker.clone())
                .with_linker_driver(options.linker.clone())
                .with_release(options.release);
            if options.save_intermediates {
                cfg = cfg.with_asm_dump(Some(options.output.with_extension("asm")));
            }
            fp_native::NativeEmitter::new(cfg)
                .emit(lir, None)
                .map_err(|err| CliError::Compilation(err.to_string()))
        }
        NativeEmitterKind::GoAsm => {
            let target = Some(GoAsmTarget::resolve(options.target_triple.as_deref()));
            let cfg = fp_goasm::config::GoAsmConfig::new(&options.output)
                .with_target(target)
                .with_target_triple(options.target_triple.clone());
            fp_goasm::GoAsmEmitter::new(cfg)
                .emit(lir, None)
                .map_err(|err| CliError::Compilation(err.to_string()))
        }
        NativeEmitterKind::Urcl => {
            fp_urcl::UrclEmitter::new(fp_urcl::UrclConfig::new(&options.output))
                .emit(lir, None)
                .map_err(|err| CliError::Compilation(err.to_string()))
        }
    }
}

pub fn compile_bytecode_file(
    path: &Path,
    package: &str,
    source_language: Option<&str>,
    lossy: LossyCompileOptions,
    options: &BytecodeCompileOptions,
) -> Result<PathBuf> {
    let mut lowered = lower_file(path, package, source_language, lossy)?;
    let bytecode = lowered
        .executor
        .run(lowered.driver.compile_bytecode(&lowered.package_id))
        .map_err(|err| CliError::Compilation(err.to_string()))?;

    if let Some(parent) = options.output.parent() {
        std::fs::create_dir_all(parent).map_err(CliError::Io)?;
    }

    let wants_text = options.emit_text
        || options.output.extension().and_then(|ext| ext.to_str()) == Some("ftbc");

    if options.save_intermediates || wants_text {
        let rendered = fp_bytecode::format_program(&bytecode);
        let text_path = if wants_text {
            options.output.clone()
        } else {
            options.output.with_extension("ftbc")
        };
        std::fs::write(&text_path, rendered).map_err(CliError::Io)?;
    }

    if !wants_text || options.save_intermediates {
        let bytes = fp_bytecode::encode_file(&bytecode)
            .map_err(|err| CliError::Compilation(format!("Bytecode encoding failed: {}", err)))?;
        let binary_path = if wants_text {
            options.output.with_extension("fbc")
        } else {
            options.output.clone()
        };
        std::fs::write(binary_path, bytes).map_err(CliError::Io)?;
    }

    Ok(options.output.clone())
}

pub fn compile_jvm_file(
    path: &Path,
    package: &str,
    source_language: Option<&str>,
    lossy: LossyCompileOptions,
    options: &JvmCompileOptions,
) -> Result<PathBuf> {
    let lowered = lower_file(path, package, source_language, lossy)?;
    let mir = lowered.mir()?;
    let class_stem = options.class_name_hint.as_deref().unwrap_or("Main");
    let jvm_options = fp_jvm::JvmBackendOptions {
        class_name: fp_jvm::derive_class_name(class_stem),
        emit_java_entrypoint: true,
    };
    let program = fp_jvm::lower_program(&mir, &jvm_options)
        .map_err(|err| CliError::Compilation(format!("MIR→JVM lowering failed: {}", err)))?;
    let mut classes = fp_jvm::emit_class_files(&program)
        .map_err(|err| CliError::Compilation(format!("JVM class emission failed: {}", err)))?;

    if classes.len() != 1 {
        return Err(CliError::Compilation(
            "JVM backend currently expects exactly one emitted class".to_string(),
        ));
    }

    let class = classes.remove(0);
    let wants_jar = options.output.extension().and_then(|ext| ext.to_str()) == Some("jar");
    let output_path = if wants_jar {
        options.output.clone()
    } else {
        options.output.with_extension("class")
    };
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent).map_err(CliError::Io)?;
    }

    let class_bytes = class.bytes.clone();
    let class_path = if wants_jar {
        output_path.with_extension("class")
    } else {
        output_path.clone()
    };

    if wants_jar {
        if options.save_intermediates {
            std::fs::write(&class_path, &class_bytes).map_err(CliError::Io)?;
        }

        let jar = fp_jvm::emit_executable_jar(&[class], &program.class.name)
            .map_err(|err| CliError::Compilation(format!("JAR packaging failed: {}", err)))?;
        std::fs::write(&output_path, jar).map_err(CliError::Io)?;
    } else {
        std::fs::write(&output_path, class_bytes).map_err(CliError::Io)?;
    }

    Ok(output_path)
}

pub fn compile_wasm_file(
    path: &Path,
    package: &str,
    source_language: Option<&str>,
    lossy: LossyCompileOptions,
    options: &WasmCompileOptions,
) -> Result<PathBuf> {
    let lowered = lower_file(path, package, source_language, lossy)?;
    let lir = lowered.lir()?;
    let wasm_bytes = fp_wasm::emit_wasm(&lir)
        .map_err(|err| CliError::Compilation(format!("Failed to emit wasm: {}", err)))?;
    if let Some(parent) = options.output.parent() {
        std::fs::create_dir_all(parent).map_err(CliError::Io)?;
    }
    std::fs::write(&options.output, wasm_bytes).map_err(CliError::Io)?;
    Ok(options.output.clone())
}

pub fn compile_ebpf_file(
    path: &Path,
    package: &str,
    source_language: Option<&str>,
    lossy: LossyCompileOptions,
    options: &EbpfCompileOptions,
) -> Result<PathBuf> {
    let lowered = lower_file(path, package, source_language, lossy)?;
    let lir = lowered.lir()?;
    if let Some(parent) = options.output.parent() {
        std::fs::create_dir_all(parent).map_err(CliError::Io)?;
    }
    if options.output.extension().and_then(|ext| ext.to_str()) == Some("o") {
        let object_bytes = fp_ebpf::emit_object(&lir).map_err(|err| {
            CliError::Compilation(format!("eBPF object emission failed: {}", err))
        })?;
        std::fs::write(&options.output, object_bytes).map_err(CliError::Io)?;
    } else {
        let text = fp_ebpf::emit_assembly(&lir).map_err(|err| {
            CliError::Compilation(format!("eBPF assembly emission failed: {}", err))
        })?;
        std::fs::write(&options.output, text).map_err(CliError::Io)?;
    }
    Ok(options.output.clone())
}

pub fn compile_cil_file(path: &Path) -> Result<String> {
    let ast = parse_file(path, None, LossyCompileOptions::default())?;
    compile_cil_ast(&ast)
}

pub fn compile_dotnet_file(
    path: &Path,
    source_language: Option<&str>,
    lossy: LossyCompileOptions,
    output: &Path,
    save_intermediates: bool,
) -> Result<PathBuf> {
    let ast = parse_file(path, source_language, lossy)?;
    compile_dotnet_ast(&ast, output, save_intermediates)
}

pub fn compile_llvm_file(
    path: &Path,
    package: &str,
    source_language: Option<&str>,
    lossy: LossyCompileOptions,
    options: &LlvmCompileOptions,
) -> Result<PathBuf> {
    #[cfg(feature = "llvm")]
    {
        let lowered = lower_file(path, package, source_language, lossy)?;
        let lir = lowered.lir()?;
        let source_path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        let llvm_output = if options.output.extension().and_then(|ext| ext.to_str()) == Some("ll") {
            options.output.clone()
        } else {
            options.output.with_extension("ll")
        };
        if let Some(parent) = llvm_output.parent() {
            std::fs::create_dir_all(parent).map_err(CliError::Io)?;
        }

        let mut target = if let Some(triple) = options.target_triple.as_deref() {
            fp_llvm::target::TargetConfig::for_triple(triple)
        } else {
            fp_llvm::target::TargetConfig::default()
        };
        if let Some(cpu) = options.target_cpu.as_deref() {
            target = target.with_cpu(cpu);
        }
        if let Some(features) = options.target_features.as_deref() {
            target = target.with_features(features);
        }

        let mut linker = fp_llvm::linking::LinkerConfig::executable(&llvm_output);
        if options.release {
            linker = linker.with_size_optimization();
        }

        let config = fp_llvm::LlvmConfig::new()
            .with_target(target)
            .with_linker(linker)
            .with_debug_info(options.debug_info)
            .with_module_name(options.module_name.clone());

        let compiler = fp_llvm::LlvmCompiler::new(config);
        let (_ir_path, ir_text) = compiler
            .compile_to_string(lir, Some(&source_path))
            .map_err(|err| CliError::Compilation(err.to_string()))?;

        if options.output.extension().and_then(|ext| ext.to_str()) == Some("ll") {
            return Ok(options.output.clone());
        }

        link_llvm_ir_with_clang(
            &llvm_output,
            &options.output,
            &ir_text,
            options.target_triple.as_deref(),
            options.target_sysroot.as_deref(),
            options.linker.as_deref(),
            options.target_linker.as_deref(),
            options.release,
        )?;

        if !options.save_intermediates {
            let _ = std::fs::remove_file(&llvm_output);
        }

        Ok(options.output.clone())
    }
    #[cfg(not(feature = "llvm"))]
    {
        let _ = (path, source_language, options);
        Err(CliError::MissingDependency(
            "Feature 'llvm' is disabled; enable it to use the LLVM emitter.".to_string(),
        ))
    }
}

pub fn compile_cranelift_file(
    path: &Path,
    package: &str,
    source_language: Option<&str>,
    lossy: LossyCompileOptions,
    options: &CraneliftCompileOptions,
) -> Result<PathBuf> {
    #[cfg(feature = "cranelift")]
    {
        let lowered = lower_file(path, package, source_language, lossy)?;
        let lir = lowered.lir()?;
        let object_path =
            options
                .output
                .with_extension(if is_windows_target(options.target_triple.as_deref()) {
                    "obj"
                } else {
                    "o"
                });
        if let Some(parent) = object_path.parent() {
            std::fs::create_dir_all(parent).map_err(CliError::Io)?;
        }
        let mut cfg = fp_cranelift::config::CraneliftConfig::object(&object_path)
            .with_target_triple(options.target_triple.clone())
            .with_target_cpu(options.target_cpu.clone())
            .with_target_features(options.target_features.clone())
            .with_sysroot(options.target_sysroot.clone())
            .with_linker_driver(options.linker.clone())
            .with_fuse_ld(options.target_linker.clone())
            .with_release(options.release)
            .with_keep_object(options.save_intermediates);
        if options.save_intermediates {
            cfg = cfg.with_asm_dump(Some(options.output.with_extension("clif")));
        }

        fp_cranelift::CraneliftEmitter::new(cfg)
            .emit(lir, None)
            .map_err(|err| CliError::Compilation(format!("fp-cranelift failed: {}", err)))?;

        let runtime_path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../crates/fp-cranelift/runtime/fp_cranelift_runtime.c");
        link_object_with_clang(
            &object_path,
            &options.output,
            options.target_triple.as_deref(),
            options.target_sysroot.as_deref(),
            options.linker.as_deref(),
            options.target_linker.as_deref(),
            options.release,
            &[runtime_path],
        )?;

        if !options.save_intermediates {
            let _ = std::fs::remove_file(&object_path);
        }

        Ok(options.output.clone())
    }
    #[cfg(not(feature = "cranelift"))]
    {
        let _ = (path, source_language, options);
        Err(CliError::MissingDependency(
            "Feature 'cranelift' is disabled; enable it to use the Cranelift emitter.".to_string(),
        ))
    }
}

fn compile_cil_ast(ast: &File) -> Result<String> {
    #[cfg(feature = "lang-dotnet")]
    {
        fp_dotnet::emit_cil(ast)
            .map_err(|err| CliError::Compilation(format!("CIL emit failed: {}", err)))
    }
    #[cfg(not(feature = "lang-dotnet"))]
    {
        let _ = ast;
        Err(CliError::MissingDependency(
            "Feature 'lang-dotnet' is disabled; enable it to use CIL emission.".to_string(),
        ))
    }
}

fn compile_dotnet_ast(ast: &File, output: &Path, save_intermediates: bool) -> Result<PathBuf> {
    #[cfg(feature = "lang-dotnet")]
    {
        fp_dotnet::emit_assembly(ast, output, save_intermediates)
            .map_err(|err| CliError::Compilation(format!(".NET assembly emit failed: {}", err)))
    }
    #[cfg(not(feature = "lang-dotnet"))]
    {
        let _ = (ast, output, save_intermediates);
        Err(CliError::MissingDependency(
            "Feature 'lang-dotnet' is disabled; enable it to use .NET assembly emission."
                .to_string(),
        ))
    }
}

fn link_llvm_ir_with_clang(
    llvm_ir_path: &Path,
    binary_path: &Path,
    llvm_ir_text: &str,
    target_triple: Option<&str>,
    sysroot: Option<&Path>,
    linker: Option<&str>,
    target_linker: Option<&Path>,
    release: bool,
) -> Result<()> {
    if let Some(parent) = binary_path.parent() {
        std::fs::create_dir_all(parent).map_err(CliError::Io)?;
    }

    let requires_eh = llvm_ir_text.contains("landingpad") || llvm_ir_text.contains("invoke");
    let default_linker = if requires_eh { "clang++" } else { "clang" };
    let linker = match linker {
        Some("clang") if requires_eh => "clang++",
        Some(other) => other,
        None => default_linker,
    };

    let mut cmd = Command::new(linker);
    cmd.arg(llvm_ir_path);
    if requires_eh {
        let runtime_path =
            Path::new(env!("CARGO_MANIFEST_DIR")).join("../../crates/fp-llvm/runtime/fp_unwind.cc");
        cmd.arg(runtime_path);
        cmd.arg("-fexceptions");
        if is_apple_target(target_triple) {
            cmd.arg("-lc++");
            cmd.arg("-lc++abi");
        }
    }
    if let Some(target_triple) = target_triple {
        cmd.arg("--target").arg(target_triple);
    }
    if let Some(sysroot) = sysroot {
        cmd.arg("--sysroot").arg(sysroot);
    }
    if let Some(linker_path) = target_linker {
        cmd.arg(format!("-fuse-ld={}", linker_path.display()));
    }
    cmd.arg("-o").arg(binary_path);
    if release {
        cmd.arg("-O2");
    }

    let output = cmd.output().map_err(CliError::Io)?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        let mut message = stderr.trim().to_string();
        if message.is_empty() {
            message = stdout.trim().to_string();
        }
        if message.is_empty() {
            message = "clang failed without diagnostics".to_string();
        }
        return Err(CliError::Compilation(format!("clang failed: {}", message)));
    }

    Ok(())
}

fn link_object_with_clang(
    object_path: &Path,
    binary_path: &Path,
    target_triple: Option<&str>,
    sysroot: Option<&Path>,
    linker: Option<&str>,
    target_linker: Option<&Path>,
    release: bool,
    extra_inputs: &[PathBuf],
) -> Result<()> {
    if let Some(parent) = binary_path.parent() {
        std::fs::create_dir_all(parent).map_err(CliError::Io)?;
    }

    let linker = linker.unwrap_or("clang");
    let mut cmd = Command::new(linker);
    cmd.arg(object_path);
    for input in extra_inputs {
        cmd.arg(input);
    }
    if !extra_inputs.is_empty() {
        cmd.arg("-lm");
    }
    if let Some(target_triple) = target_triple {
        cmd.arg("--target").arg(target_triple);
    }
    if let Some(sysroot) = sysroot {
        cmd.arg("--sysroot").arg(sysroot);
    }
    if let Some(linker_path) = target_linker {
        cmd.arg(format!("-fuse-ld={}", linker_path.display()));
    }
    cmd.arg("-o").arg(binary_path);
    if release {
        cmd.arg("-O2");
    }

    let output = cmd.output().map_err(CliError::Io)?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        let mut message = stderr.trim().to_string();
        if message.is_empty() {
            message = stdout.trim().to_string();
        }
        if message.is_empty() {
            message = "clang failed without diagnostics".to_string();
        }
        return Err(CliError::Compilation(format!("clang failed: {}", message)));
    }

    Ok(())
}

fn is_windows_target(target_triple: Option<&str>) -> bool {
    let triple = match target_triple {
        Some(triple) => triple,
        None => return cfg!(target_os = "windows"),
    };
    triple.contains("windows") || triple.contains("msvc") || triple.contains("mingw")
}

fn is_apple_target(target_triple: Option<&str>) -> bool {
    let triple = match target_triple {
        Some(triple) => triple,
        None => return cfg!(any(target_os = "macos", target_os = "ios")),
    };
    triple.contains("apple") || triple.contains("darwin") || triple.contains("macos")
}

fn execute_ast(
    input: SourceInput,
    language: &str,
    identity: CompilerIdentity,
    mode: fp_core::context::ExecutionMode,
    lossy: LossyCompileOptions,
) -> Result<Value> {
    let value_key = identity.path.to_key();
    let executor = CompilerExecutor::new();
    let mut driver = compile_source_file(
        input,
        language,
        &identity,
        lossy,
        &executor,
        PipelineMode::Native,
    )?;
    drain_driver(&mut driver, lossy)?;

    match mode {
        fp_core::context::ExecutionMode::CompileTime => driver
            .state
            .borrow()
            .const_value(&ConstValueId::new(format!("const_value:{value_key}")))
            .map(|value| value.clone())
            .map_err(|err| CliError::Compilation(err.to_string())),
        fp_core::context::ExecutionMode::Runtime => {
            let package_id = PackageId::new(identity.path.path().head().ok_or_else(|| {
                CliError::Compilation("source file has no package identity".to_string())
            })?);
            let lir_id = LirId::new(format!("lir:{}:{}", package_id.as_str(), value_key));
            driver
                .execute_runtime(&lir_id)
                .map_err(|err| CliError::Compilation(err.to_string()))
        }
    }
}

fn lower_file(
    path: &Path,
    package: &str,
    source_language: Option<&str>,
    lossy: LossyCompileOptions,
) -> Result<LoweredProgram> {
    let language = resolve_source_language(path, source_language)?;
    let identity = CompilerIdentity::for_file(package, path);
    let executor = CompilerExecutor::new();
    let mut driver = compile_source_file(
        SourceInput::Path(path.to_path_buf()),
        &language,
        &identity,
        lossy,
        &executor,
        PipelineMode::Native,
    )?;
    drain_driver(&mut driver, lossy)?;
    let package_id =
        PackageId::new(identity.path.path().head().ok_or_else(|| {
            CliError::Compilation("source file has no package identity".to_string())
        })?);
    Ok(LoweredProgram {
        driver,
        package_id,
        module_path: identity.path.path().clone(),
        executor,
    })
}

/// `"std"`/`"libc"` resolve against different providers depending on the
/// active source language: `fp_lang`'s hand-written `.fp` reimplementation
/// for `.fp`-dialect projects, or real rustc source (`fp-rust`'s
/// `RustStdProvider`, see `docs/RustStd.md`) for real `.rs`/Cargo projects.
/// Panics on an unrecognized language rather than silently defaulting —
/// wiring up std resolution for a new source language is a deliberate step,
/// not something to fall through to FerroPhase's `.fp` std by accident.
fn std_provider_for(language: &str) -> Arc<dyn fp_core::package::provider::PackageProvider> {
    match language {
        l if l == languages::FERROPHASE => Arc::new(fp_lang::provider::FerroPhaseProvider),
        l if l == languages::RUST => Arc::new(fp_rust::RustStdProvider),
        other => panic!("std_provider_for: no std/libc provider wired up for language {other:?}"),
    }
}

/// Package/provider discovery shared by every single-file compiler entry
/// point — `resolve_input_package` below, and `commands::compile`'s
/// `provider_and_package_for_input`. A single file is a package with one
/// member — this only kicks in when `input` actually lives inside a
/// discoverable multi-file package (a `Cargo.toml`/`Magnet.toml` manifest
/// somewhere above it *and* a declared package under that manifest whose
/// root actually contains `input`), so sibling modules/imports resolve
/// correctly instead of `input` being (incorrectly) treated as an isolated
/// standalone file. Returns `None` both when no manifest is found at all,
/// and when a manifest is found but doesn't actually cover `input` (e.g. a
/// standalone script sitting under an unrelated workspace root) — either
/// way, callers fall back to wrapping `input` as its own single-member
/// package.
pub fn find_manifest_package(
    input: &Path,
    language: &str,
) -> Result<Option<(Arc<dyn PackageProvider>, PackageId, PathBuf)>> {
    let input_abs = input.canonicalize().unwrap_or_else(|_| input.to_path_buf());
    let Some(root) = fp_lang::project::find_manifest(&input_abs) else {
        return Ok(None);
    };
    let provider = crate::languages::discovery::provider_for_language(language, &root)
        .ok_or_else(|| {
            CliError::Compilation(format!("no package provider for source language: {language}"))
        })?;
    let packages = provider
        .list_packages()
        .map_err(|e| CliError::Compilation(e.to_string()))?;
    let mut found = None;
    for package_id in &packages {
        let metadata = provider
            .load_package_metadata(package_id)
            .map_err(|e| CliError::Compilation(e.to_string()))?;
        let package_root = metadata.root.to_path_buf();
        let package_root_abs = package_root
            .canonicalize()
            .unwrap_or_else(|_| package_root.clone());
        if input_abs.starts_with(&package_root_abs) {
            found = Some((package_id.clone(), package_root_abs));
            break;
        }
    }
    // A manifest existing somewhere above `input` doesn't mean it's *for*
    // `input` — e.g. a standalone `.fp` script can sit anywhere under a
    // large Cargo workspace root without being part of any of that
    // workspace's own crates (this repo's own `examples/*.fp` next to its
    // Rust-workspace `Cargo.toml` is exactly this case). Match the same
    // "no manifest at all" fallback above instead of treating "found a
    // manifest, but no declared package covers this file" as an error —
    // both mean the same thing to the caller: wrap `input` as its own
    // single-member package.
    let Some((package_id, package_root_abs)) = found else {
        return Ok(None);
    };
    Ok(Some((provider, package_id, package_root_abs)))
}

/// Computes the `PackageItem` path tag a package's own provider would tag
/// `input` with, given its package root — the one implementation shared by
/// `resolve_input_package`'s single-file resolution and
/// `commands::compile::provider_and_package_for_input`'s `--target` path,
/// instead of two independent per-language guesses. No fallback: an
/// unsupported language is a real error, not a silent drop to some default
/// estimator.
pub(crate) fn module_path_for_language(
    language: &str,
    package_root: &Path,
    input: &Path,
) -> Result<QualifiedPath> {
    match language {
        "rust" | "rs" => {
            let rel = input.strip_prefix(package_root.join("src")).map_err(|_| {
                CliError::Compilation(format!(
                    "{} is not inside {}'s src/ directory",
                    input.display(),
                    package_root.display()
                ))
            })?;
            Ok(fp_rust::provider::rs_relative_to_module_path(
                &rel.display().to_string(),
            ))
        }
        "ferrophase" | "fp" => {
            let rel = input.strip_prefix(package_root.join("src")).map_err(|_| {
                CliError::Compilation(format!(
                    "{} is not inside {}'s src/ directory",
                    input.display(),
                    package_root.display()
                ))
            })?;
            Ok(fp_lang::magnet_provider::module_path_from_relative(
                &rel.display().to_string(),
            ))
        }
        "typescript" | "ts" | "javascript" | "js" => {
            module_path_for_typescript(package_root, input)
        }
        other => Err(CliError::Compilation(format!(
            "no module-path estimator for source language: {other}"
        ))),
    }
}

#[cfg(feature = "lang-typescript")]
fn module_path_for_typescript(package_root: &Path, input: &Path) -> Result<QualifiedPath> {
    Ok(QualifiedPath::new(fp_typescript::package::estimate_module_path(
        package_root,
        input,
    )))
}

#[cfg(not(feature = "lang-typescript"))]
fn module_path_for_typescript(_package_root: &Path, _input: &Path) -> Result<QualifiedPath> {
    Err(CliError::Compilation(
        "typescript support not compiled into this build".to_string(),
    ))
}

/// A single compiler input: either a real on-disk file (the common case —
/// parsed lazily, once a `PackageProvider` actually asks for its source), or
/// an already-built in-memory `File` with no path to read from (e.g.
/// `eval_script`'s synthetic `"<eval>"` script). Two genuinely different
/// kinds of input, not one file-focused path with a bolted-on exception.
enum SourceInput {
    Path(PathBuf),
    InMemory(File),
}

/// Resolves any compiler input to `(provider, package_id, module_path)` —
/// the real enclosing package if `input` lives inside a discoverable
/// multi-file package (a `Cargo.toml`/`Magnet.toml` manifest above it), else
/// a synthetic one-member package. Either way, everything downstream goes
/// through the same `PackageProvider`-shaped pipeline; parsing only ever
/// happens lazily, inside whichever provider is returned, never eagerly here.
fn resolve_input_package(
    input: SourceInput,
    language: &str,
    identity: &CompilerIdentity,
) -> Result<(Arc<dyn PackageProvider>, PackageId, QualifiedPath)> {
    match input {
        SourceInput::Path(path) => {
            if let Some((provider, package_id, package_root_abs)) =
                find_manifest_package(&path, language)?
            {
                let input_abs = path.canonicalize().unwrap_or_else(|_| path.clone());
                let module_path = module_path_for_language(language, &package_root_abs, &input_abs)?;
                Ok((provider, package_id, module_path))
            } else {
                let package_id =
                    PackageId::new(identity.path.path().head().ok_or_else(|| {
                        CliError::Compilation("source file has no package identity".to_string())
                    })?);
                let module_path = identity.path.path().clone();
                let frontend = frontend_for_language(language)?;
                let provider = single_file_provider(
                    package_id.clone(),
                    module_path.clone(),
                    path,
                    frontend,
                    FrontendParseMode::Strict,
                );
                Ok((provider, package_id, module_path))
            }
        }
        SourceInput::InMemory(source) => {
            let package_id = PackageId::new(identity.path.path().head().ok_or_else(|| {
                CliError::Compilation("source file has no package identity".to_string())
            })?);
            let module_path = identity.path.path().clone();
            let provider = in_memory_provider(package_id.clone(), module_path.clone(), source)
                .map_err(|e| CliError::Compilation(e.to_string()))?;
            Ok((provider, package_id, module_path))
        }
    }
}

/// Resolves a real on-disk `path` to `(provider, package_id, module_path)` —
/// the real enclosing package if discoverable, else a single-member
/// package — for callers outside this module (`commands::compile`'s
/// `--target` pipeline) that need the same resolution `compile_source_file`
/// uses, instead of maintaining a second implementation.
pub fn resolve_source_package(
    path: &Path,
    language: &str,
    package: &str,
) -> Result<(Arc<dyn PackageProvider>, PackageId, QualifiedPath)> {
    let identity = CompilerIdentity::for_file(package, path);
    resolve_input_package(SourceInput::Path(path.to_path_buf()), language, &identity)
}

fn compile_source_file(
    input: SourceInput,
    language: &str,
    identity: &CompilerIdentity,
    lossy: LossyCompileOptions,
    executor: &CompilerExecutor,
    pipeline: PipelineMode,
) -> Result<CompilerDriver> {
    let (input_provider, package_id, module_path) =
        resolve_input_package(input, language, identity)?;

    let std_provider = std_provider_for(language);
    let provider = Arc::new(fp_core::package::provider::CompositeProvider::new(vec![
        std_provider,
        input_provider,
    ]));
    let workspace = std::rc::Rc::new(fp_core::workspace::WorkspaceContext::new(provider));
    let mut session = CompilerSession::new(data_layout(), executor, workspace);
    session.driver().pipeline = pipeline;
    session.driver().state.borrow_mut().set_lossy(lossy.enabled);
    executor
        .run(session.driver().compile_package(&package_id))
        .map_err(|err| CliError::Compilation(err.to_string()))?;
    session
        .driver()
        .focus_package(package_id.clone())
        .map_err(|err| CliError::Compilation(err.to_string()))?;
    // Only evaluate comptime LIR for full native compilation
    if pipeline == PipelineMode::Native {
        executor
            .run(session.driver().compile_package_module_native(
                &package_id,
                &module_path,
                "main",
            ))
            .map_err(|err| CliError::Compilation(err.to_string()))?;
    }
    Ok(session.into_driver())
}

fn drain_driver(driver: &mut CompilerDriver, lossy: LossyCompileOptions) -> Result<()> {
    emit_typing_diagnostics(&driver.state.borrow().typing_ctx.diagnostics.borrow(), lossy)
}

pub fn parse_expr_with_mode(source: &str, parse_mode: FrontendParseMode) -> Result<File> {
    let frontend = FerroFrontend::new();
    frontend.set_parse_mode(parse_mode);
    let FrontendResult {
        ast, diagnostics, ..
    } = frontend
        .parse_expr(source)
        .map_err(|err| CliError::Compilation(err.to_string()))?;
    emit_frontend_diagnostics(
        &diagnostics.get_diagnostics(),
        LossyCompileOptions::default(),
    )?;
    Ok(ast)
}

fn parse_file(
    path: &Path,
    source_language: Option<&str>,
    lossy: LossyCompileOptions,
) -> Result<File> {
    parse_file_with_mode(path, source_language, FrontendParseMode::Strict, lossy)
}

pub fn compile_file_to_lir_bundle(
    path: &Path,
    package: &str,
    source_language: Option<&str>,
    lossy: LossyCompileOptions,
) -> Result<LirBundle> {
    let language = resolve_source_language(path, source_language)?;
    let identity = CompilerIdentity::for_file(package, path);
    let executor = CompilerExecutor::new();
    let mut driver = compile_source_file(
        SourceInput::Path(path.to_path_buf()),
        &language,
        &identity,
        lossy,
        &executor,
        PipelineMode::Native,
    )?;
    drain_driver(&mut driver, lossy)?;
    let lowered = LoweredProgram {
        driver,
        package_id: PackageId::new(identity.path.path().head().ok_or_else(|| {
            CliError::Compilation("source file has no package identity".to_string())
        })?),
        module_path: identity.path.path().clone(),
        executor,
    };
    Ok(LirBundle {
        frontend: FrontendBundle {
            source_language: language,
        },
        hir_program: lowered.hir()?,
        mir_program: lowered.mir()?,
        lir_program: lowered.lir()?,
    })
}

pub fn parse_file_with_mode(
    path: &Path,
    source_language: Option<&str>,
    parse_mode: FrontendParseMode,
    lossy: LossyCompileOptions,
) -> Result<File> {
    parse_file_with_context(path, source_language, parse_mode, lossy)
}

/// Typecheck a whole package by registering its real `PackageProvider` with
/// a fresh `CompilerDriver` under `PipelineMode::TypecheckedTranspile`,
/// instead of flattening the package's items into a single tag-less `File`
/// and routing it through `InputPackageProvider`/`FerroModuleSourceResolver`
/// (which only knows how to *discover* sibling modules from disk — the
/// wrong tool when a real provider has already parsed and tagged every
/// item). `HirGenerator::transform_package` reads each item's real
/// `PackageItem.path` tag to build correct module scoping, so this needs no
/// AST-level module nesting at all.
///
/// Returns the package's items with real resolved types spliced in where
/// typing succeeded (module declarations, which HIR has no representation
/// for, pass through untouched), plus, on `PackageSource.referenced_paths`,
/// the qualified paths each item references — raw facts a target backend
/// can use to compute which imports it actually needs.
pub fn typecheck_package(
    provider: Arc<dyn PackageProvider>,
    package_id: &PackageId,
    lossy: LossyCompileOptions,
    language: &str,
) -> Result<PackageSource> {
    let executor = CompilerExecutor::new();
    let std_provider = std_provider_for(language);
    let combined = Arc::new(fp_core::package::provider::CompositeProvider::new(vec![
        std_provider,
        provider,
    ]));
    let workspace = std::rc::Rc::new(fp_core::workspace::WorkspaceContext::new(combined));
    let mut session = CompilerSession::new(data_layout(), &executor, workspace);
    session.driver().pipeline = PipelineMode::TypecheckedTranspile;
    session.driver().state.borrow_mut().set_lossy(lossy.enabled);
    let package = executor
        .run(session.driver().compile_package(package_id))
        .map_err(|err| CliError::Compilation(err.to_string()))?;

    // `hir_typeck.rs` now records most errors as diagnostics and keeps
    // going (see `HirTypeChecker::record_error`/`error_ty`) instead of
    // aborting `compile_package` on the first one, so a successful
    // `Result` above no longer means "fully, correctly typed" by itself —
    // check what actually got recorded, same as the single-file path
    // already does (`drain_driver`), so a genuinely broken package still
    // falls back to untyped instead of silently carrying `Ty::error()`
    // placeholders through as if nothing were wrong.
    drain_driver(session.driver(), lossy)?;

    let package = package.borrow();
    // Typed/normalized content is already spliced onto `package.items` by
    // `CompilerDriver::compile_package` (qualified-path-keyed, including
    // impl methods) — nothing left to reconcile here.
    let items = package.items.clone();

    let referenced_paths = package
        .referenced_paths_by_path
        .as_ref()
        .map(|by_path| {
            by_path
                .iter()
                .map(|(path, refs)| {
                    let path = path.to_segments();
                    let refs = refs.iter().map(|r| r.to_segments()).collect();
                    (path, refs)
                })
                .collect()
        })
        .unwrap_or_default();
    let source = PackageSource {
        package_id: package_id.clone(),
        name: package.name.clone(),
        graph: package.graph.clone(),
        module_paths: package.module_paths.clone(),
        items,
        referenced_paths,
    };
    Ok(source)
}

fn parse_file_with_context(
    path: &Path,
    source_language: Option<&str>,
    parse_mode: FrontendParseMode,
    lossy: LossyCompileOptions,
) -> Result<File> {
    let frontend = select_frontend(path, source_language)?;
    frontend.set_parse_mode(parse_mode);
    let source = std::fs::read_to_string(path).map_err(CliError::Io)?;
    let FrontendResult { ast, diagnostics, .. } = frontend
        .parse_file(&source, path)
        .map_err(|err| CliError::Compilation(err.to_string()))?;
    emit_frontend_diagnostics(&diagnostics.get_diagnostics(), lossy)?;
    // Frontends leave `collected_items` empty on every nested block/function/
    // const-block; the typer's predeclare pass relies on it being populated
    // (e.g. to know a nested `type X = const { ... }` needs comptime
    // evaluation before the item is fully typed) so compute it here, once,
    // for every language frontend.
    let mut ast = ast;
    fp_core::ast::annotate_collected_items(&mut ast);
    Ok(ast)
}

/// Resolves the effective source language for `path`: an explicit
/// `source_language` override, else extension-based detection. No silent
/// default — an undetectable language (unknown/missing extension, no
/// override) is a real error, not a guess at FerroPhase.
pub(crate) fn resolve_source_language(path: &Path, source_language: Option<&str>) -> Result<String> {
    if let Some(lang) = source_language {
        return Ok(lang.trim().to_ascii_lowercase());
    }
    detect_source_language(path)
        .map(|lang| lang.name.to_ascii_lowercase())
        .ok_or_else(|| {
            CliError::InvalidInput(format!(
                "cannot detect source language for {}: pass --source-language explicitly",
                path.display()
            ))
        })
}

fn select_frontend(
    path: &Path,
    source_language: Option<&str>,
) -> Result<Box<dyn LanguageFrontend>> {
    let language = resolve_source_language(path, source_language)?;
    frontend_for_language(&language)
}

/// Registry lookup: the one place a language name maps to its
/// `LanguageFrontend` implementation. Callers that already know the
/// resolved language (e.g. package/single-file provider construction)
/// should call this directly instead of going through `select_frontend`'s
/// path-based detection.
pub(crate) fn frontend_for_language(language: &str) -> Result<Box<dyn LanguageFrontend>> {
    match language {
        value if value == languages::C => Ok(Box::new(CFrontend::new().map_err(|err| {
            CliError::Compilation(format!("failed to initialize C frontend: {err}"))
        })?)),
        value if value == languages::FERROPHASE => Ok(Box::new(FerroFrontend::new())),
        value if value == languages::RUST => Ok(Box::new(fp_rust::RustFrontend::new())),
        #[cfg(feature = "lang-typescript")]
        value if value == languages::TYPESCRIPT || value == languages::JAVASCRIPT => {
            Ok(Box::new(TypeScriptFrontend::new(TsParseMode::Loose)))
        }
        #[cfg(feature = "lang-wit")]
        value if value == languages::WIT => Ok(Box::new(WitFrontend::new())),
        #[cfg(feature = "lang-python")]
        value if value == languages::PYTHON => Ok(Box::new(PythonFrontend::new())),
        #[cfg(feature = "lang-golang")]
        value if value == languages::GO => Ok(Box::new(GoFrontend::new())),
        #[cfg(feature = "lang-sql")]
        value if value == languages::SQL => Ok(Box::new(SqlFrontend::new())),
        #[cfg(feature = "lang-prql")]
        value if value == languages::PRQL => Ok(Box::new(PrqlFrontend::new())),
        #[cfg(feature = "lang-jsonschema")]
        value if value == languages::JSONSCHEMA => Ok(Box::new(JsonSchemaFrontend::new())),
        #[cfg(feature = "lang-json")]
        value if value == languages::JSON => Ok(Box::new(JsonFrontend::new())),
        #[cfg(feature = "lang-flatbuffers")]
        value if value == languages::FLATBUFFERS => Ok(Box::new(FlatbuffersFrontend::new())),
        #[cfg(feature = "lang-toml")]
        value if value == languages::TOML => Ok(Box::new(TomlFrontend::new())),
        #[cfg(feature = "lang-hcl")]
        value if value == languages::HCL => Ok(Box::new(HclFrontend::new())),
        other => Err(CliError::InvalidInput(format!(
            "Unsupported source language for compiler path: {}",
            other
        ))),
    }
}

fn emit_frontend_diagnostics(diagnostics: &[Diagnostic], lossy: LossyCompileOptions) -> Result<()> {
    DiagnosticManager::emit(
        diagnostics,
        Some("frontend"),
        &DiagnosticDisplayOptions::default(),
    );
    if !lossy.enabled
        && diagnostics
            .iter()
            .any(|diagnostic| diagnostic.level == DiagnosticLevel::Error)
    {
        return Err(CliError::Compilation(
            "frontend stage failed; see diagnostics for details".to_string(),
        ));
    }
    Ok(())
}

fn emit_typing_diagnostics(
    diagnostics: &[TypingDiagnostic],
    lossy: LossyCompileOptions,
) -> Result<()> {
    let rendered: Vec<Diagnostic<String>> = diagnostics.iter().map(as_core_diagnostic).collect();
    DiagnosticManager::emit(
        &rendered,
        Some("typing"),
        &DiagnosticDisplayOptions::default(),
    );
    if !lossy.enabled
        && diagnostics
            .iter()
            .any(|diagnostic| matches!(diagnostic.level, TypingDiagnosticLevel::Error))
    {
        return Err(CliError::Compilation(
            "typing stage failed; see diagnostics for details".to_string(),
        ));
    }
    Ok(())
}

fn as_core_diagnostic(diagnostic: &TypingDiagnostic) -> Diagnostic<String> {
    diagnostic.as_core_diagnostic()
}

struct CompilerIdentity {
    path: FullyQualifiedPath,
}

struct LoweredProgram {
    driver: CompilerDriver,
    package_id: PackageId,
    module_path: QualifiedPath,
    executor: CompilerExecutor,
}


impl LoweredProgram {
    fn hir(&self) -> Result<fp_core::hir::Program> {
        let package = self.compiled_package()?;
        let package = package.borrow();
        package.hir_program.clone().ok_or_else(|| {
            CliError::Compilation(format!(
                "compiled package `{}` contains no HIR program",
                self.package_id
            ))
        })
    }

    fn mir(&self) -> Result<fp_core::mir::Program> {
        let package = self.compiled_package()?;
        let package = package.borrow();
        package.mir_program.clone().ok_or_else(|| {
            CliError::Compilation(format!(
                "compiled package `{}` contains no MIR program",
                self.package_id
            ))
        })
    }

    fn lir(&self) -> Result<fp_core::lir::LirProgram> {
        let package = self.compiled_package()?;
        let package = package.borrow();
        if package.lir_workspace.artifacts().is_empty() {
            return Err(CliError::Compilation(format!(
                "compiled package `{}` contains no LIR artifacts",
                self.package_id
            )));
        }
        // Native/LLVM/Cranelift emitters all consume a single flattened
        // `LirProgram` built from just this package's own workspace — a
        // cross-package call (e.g. `std::json::parse`) type-checks and
        // lowers fine (its *signature* is predeclared into this package's
        // generator, see `predeclare_dependency_function_signatures`), but
        // without folding dependency workspaces in here too, the callee's
        // actual function *body* never reaches the emitted binary, leaving
        // an unresolved external symbol at load time. Merge every
        // dependency's compiled LIR workspace in before this package's own,
        // mirroring the same merge `evaluate_comptime_lir` already does for
        // comptime execution.
        let mut combined = fp_core::lir::LirWorkspace::new(package.lir_workspace.data_layout.clone());
        let state = self.driver.state.borrow();
        for (dependency_id, dep_package) in state.typing_ctx.env_ctx.crates().iter() {
            if *dependency_id == self.package_id {
                continue;
            }
            combined
                .add_workspace(&dep_package.borrow().lir_workspace)
                .map_err(|error| CliError::Compilation(error.to_string()))?;
        }
        combined
            .add_workspace(&package.lir_workspace)
            .map_err(|error| CliError::Compilation(error.to_string()))?;
        let mut lir = combined.to_program();
        // Native/asm emitters locate the process entry point by its final,
        // bare symbol name (see `CompilerDriver::rename_lir_function`).
        // This path builds its own `LirProgram` straight from the
        // workspace rather than going through `select_entrypoint`, so
        // resolve and rename the entrypoint here too — otherwise a
        // module-nested `main` keeps its qualified, mangled name and
        // native emission can't find it.
        if let Ok(def_id) =
            self.driver
                .resolve_entrypoint_def_id(&self.package_id, &self.module_path, "main")
        {
            CompilerDriver::rename_lir_function(&mut lir, def_id, "main");
        }
        Ok(lir)
    }

    fn compiled_package(
        &self,
    ) -> Result<std::rc::Rc<std::cell::RefCell<fp_core::package::CompiledPackage>>> {
        self.driver
            .state
            .borrow()
            .typing_ctx
            .env_ctx
            .compiled_package(&self.package_id)
            .ok_or_else(|| {
                CliError::Compilation(format!(
                    "compiled package `{}` is unavailable",
                    self.package_id
                ))
            })
    }

    #[allow(dead_code)]
    fn bytecode(&mut self) -> Result<fp_bytecode::BytecodeProgram> {
        self.executor
            .run(self.driver.compile_bytecode(&self.package_id))
            .map_err(|err| CliError::Compilation(err.to_string()))
    }
}

impl CompilerIdentity {
    fn for_script() -> Self {
        Self::new(vec!["cli".to_string(), "eval_script".to_string()])
    }

    fn for_file(package: &str, path: &Path) -> Self {
        let module = path
            .file_stem()
            .and_then(|stem| stem.to_str())
            .map(str::to_owned)
            .unwrap_or_else(|| "module".to_string());
        Self::new(vec![package.to_string(), module])
    }

    fn new(segments: Vec<String>) -> Self {
        let path = FullyQualifiedPath::from_segments(segments);
        Self { path }
    }
}
