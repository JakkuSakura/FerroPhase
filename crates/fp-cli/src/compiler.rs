use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;

use fp_c::CFrontend;
use fp_compiler::{
    CompilerDriver, CompilerExecutor, CompilerSession, ConstValueId, FullyQualifiedPath, LirId,
    PipelineMode,
};
use fp_core::intrinsics::{IntrinsicMaterializer, IntrinsicNormalizer};
use fp_core::module::path::QualifiedPath;
use fp_core::package::provider::{PackageProvider, ProviderError, ProviderResult};
use fp_core::package::{PackageDescriptor, PackageId, PackageSource};
use fp_core::vfs::{UnixFileSystem, VirtualPath};
use fp_core::{
    ast::{
        Expr, ExprBlock, File, Ident, Item, ItemDefConst, ItemDefFunction, ItemKind, ScriptBlock,
        Value, Visibility,
    },
    diagnostics::{Diagnostic, DiagnosticDisplayOptions, DiagnosticLevel, DiagnosticManager},
    frontend::{FrontendParseMode, FrontendResult, FrontendSnapshot, LanguageFrontend},
    lir::LirDataLayout,
};
use fp_goasm::config::GoAsmTarget;
use fp_lang::FerroFrontend;
use fp_lang::module_source::FerroModuleSourceResolver;
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
    let ast = parse_file(path, None, lossy)?;
    if syntax_only {
        return Ok(());
    }

    let executor = CompilerExecutor::new();
    let identity = CompilerIdentity::for_file(package, path);
    let mut driver = compile_source_file(ast, &identity, compiler_workspace(), lossy, &executor, PipelineMode::Native)?;
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
        ast,
        &identity,
        compiler_workspace(),
        LossyCompileOptions::default(),
        &executor,
        PipelineMode::Native,
    )?;
    drain_driver(&mut driver, LossyCompileOptions::default())?;
    if let Some((_, value)) = driver
        .state
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
        .const_value(&ConstValueId::new(format!(
            "const_value:{}",
            identity.path.to_key()
        )))
        .map(|value| value.clone())
        .map_err(|error| CliError::Compilation(error.to_string()))
}

pub fn interpret_file(path: &Path, package: &str) -> Result<Value> {
    let ast = parse_file_with_mode(
        path,
        None,
        FrontendParseMode::Strict,
        LossyCompileOptions::default(),
    )?;
    execute_ast(
        ast,
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
    pub ast: File,
    pub frontend_snapshot: Option<FrontendSnapshot>,
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
    ast: File,
    identity: CompilerIdentity,
    mode: fp_core::context::ExecutionMode,
    lossy: LossyCompileOptions,
) -> Result<Value> {
    let value_key = identity.path.to_key();
    let executor = CompilerExecutor::new();
    let mut driver = compile_source_file(ast, &identity, compiler_workspace(), lossy, &executor, PipelineMode::Native)?;
    drain_driver(&mut driver, lossy)?;

    match mode {
        fp_core::context::ExecutionMode::CompileTime => driver
            .state
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
    let ast = parse_file(path, source_language, lossy)?;
    let identity = CompilerIdentity::for_file(package, path);
    let executor = CompilerExecutor::new();
    let mut driver = compile_source_file(ast, &identity, compiler_workspace(), lossy, &executor, PipelineMode::Native)?;
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

fn compiler_workspace() -> std::rc::Rc<fp_core::workspace::WorkspaceContext> {
    compiler_workspace_for(languages::FERROPHASE)
}

/// `"std"`/`"libc"` resolve against different providers depending on the
/// active source language: `fp_lang`'s hand-written `.fp` reimplementation
/// for `.fp`-dialect projects, or real rustc source (`fp-rust`'s
/// `RustStdProvider`, see `docs/RustStd.md`) for real `.rs`/Cargo projects.
/// Panics on an unrecognized language rather than silently defaulting —
/// wiring up std resolution for a new source language is a deliberate step,
/// not something to fall through to FerroPhase's `.fp` std by accident.
fn compiler_workspace_for(language: &str) -> std::rc::Rc<fp_core::workspace::WorkspaceContext> {
    let workspace = fp_core::workspace::WorkspaceContext::new();
    let std_provider: Arc<dyn fp_core::package::provider::PackageProvider> = match language {
        l if l == languages::FERROPHASE => Arc::new(fp_lang::provider::FerroPhaseProvider),
        l if l == languages::RUST => Arc::new(fp_rust::RustStdProvider),
        other => panic!(
            "compiler_workspace_for: no std/libc provider wired up for language {other:?}"
        ),
    };
    workspace.register_provider(std_provider);
    std::rc::Rc::new(workspace)
}

struct InputPackageProvider {
    package_id: PackageId,
    descriptor: Arc<PackageDescriptor>,
    source: PackageSource,
}

impl InputPackageProvider {
    fn new(
        package_id: PackageId,
        module_path: QualifiedPath,
        source: File,
    ) -> ProviderResult<Self> {
        let descriptor = PackageDescriptor {
            id: package_id.clone(),
            name: package_id.as_str().to_owned(),
            version: None,
            manifest_path: VirtualPath::from_path(&source.path),
            root: VirtualPath::from_path(source.path.parent().unwrap_or(Path::new("."))),
            metadata: Default::default(),
            modules: Vec::new(),
        };
        let resolver = FerroModuleSourceResolver::new(Arc::new(UnixFileSystem::new("/")));
        let package_source =
            resolver.resolve_package_source(descriptor.clone(), module_path, source)?;
        Ok(Self {
            package_id,
            descriptor: Arc::new(descriptor),
            source: package_source,
        })
    }
}

impl PackageProvider for InputPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(vec![self.package_id.clone()])
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        if id != &self.package_id {
            return Err(ProviderError::PackageNotFound(id.clone()));
        }
        Ok(self.descriptor.clone())
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<PackageSource> {
        if id != &self.package_id {
            return Err(ProviderError::PackageNotFound(id.clone()));
        }
        Ok(self.source.clone())
    }

    fn refresh(&self) -> ProviderResult<()> {
        Ok(())
    }
}

/// Wraps an already-parsed single file as a one-member `PackageProvider`,
/// via the existing `InputPackageProvider` (disk-based sibling-module
/// discovery through `FerroModuleSourceResolver` — the correct mechanism
/// for a genuinely standalone file with no enclosing package/manifest).
/// Used by `compile_emit_target`'s single-file pipeline when no real
/// package can be discovered for the input file.
pub fn single_file_provider(
    package_id: PackageId,
    module_path: QualifiedPath,
    source: File,
) -> Result<Arc<dyn PackageProvider>> {
    let provider = InputPackageProvider::new(package_id, module_path, source)
        .map_err(|e| CliError::Compilation(e.to_string()))?;
    Ok(Arc::new(provider))
}

fn compile_source_file(
    ast: File,
    identity: &CompilerIdentity,
    workspace: std::rc::Rc<fp_core::workspace::WorkspaceContext>,
    lossy: LossyCompileOptions,
    executor: &CompilerExecutor,
    pipeline: PipelineMode,
) -> Result<CompilerDriver> {
    let mut session = CompilerSession::new(data_layout(), executor, workspace);
    session.driver().pipeline = pipeline;
    let package_id =
        PackageId::new(identity.path.path().head().ok_or_else(|| {
            CliError::Compilation("source file has no package identity".to_string())
        })?);
    let input_provider = InputPackageProvider::new(
        package_id.clone(),
        identity.path.path().clone(),
        ast.clone(),
    )
    .map_err(|error| CliError::Compilation(error.to_string()))?;
    session.register_provider(Arc::new(input_provider));
    session.driver().state.set_lossy(lossy.enabled);
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
                identity.path.path(),
                "main",
            ))
            .map_err(|err| CliError::Compilation(err.to_string()))?;
    }
    Ok(session.into_driver())
}

fn drain_driver(driver: &mut CompilerDriver, lossy: LossyCompileOptions) -> Result<()> {
    emit_typing_diagnostics(&driver.state.typing_ctx.diagnostics.borrow(), lossy)
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

pub fn parse_language_target_file(path: &Path, source_language: Option<&str>) -> Result<File> {
    parse_file_with_context(
        path,
        source_language,
        FrontendParseMode::Strict,
        LossyCompileOptions::default(),
    )
    .map(|parsed| parsed.ast)
}

/// Like `parse_language_target_file`, but also returns the frontend's
/// `AstSerializer` — needed by callers that construct a `PackageProvider`
/// directly from the parsed `File` (e.g. `single_file_provider`) rather
/// than going through a path that already registers it internally (real
/// providers like `RustPackageProvider` call
/// `register_threadlocal_serializer` themselves during `load_package_source`).
pub fn parse_language_target_file_with_serializer(
    path: &Path,
    source_language: Option<&str>,
) -> Result<(File, Arc<dyn fp_core::ast::AstSerializer>)> {
    let parsed = parse_file_with_context(
        path,
        source_language,
        FrontendParseMode::Strict,
        LossyCompileOptions::default(),
    )?;
    Ok((parsed.ast, parsed.serializer))
}

pub fn compile_file_to_lir_bundle(
    path: &Path,
    package: &str,
    source_language: Option<&str>,
    lossy: LossyCompileOptions,
) -> Result<LirBundle> {
    let parsed = parse_file_with_context(path, source_language, FrontendParseMode::Strict, lossy)?;
    let frontend = FrontendBundle {
        source_language: parsed.source_language.clone(),
        ast: parsed.ast.clone(),
        frontend_snapshot: parsed.frontend_snapshot.clone(),
    };
    let identity = CompilerIdentity::for_file(package, path);
    let executor = CompilerExecutor::new();
    let mut driver = compile_source_file(
        parsed.ast,
        &identity,
        compiler_workspace(),
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
        frontend,
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
    parse_file_with_context(path, source_language, parse_mode, lossy).map(|parsed| parsed.ast)
}

/// Wraps a real, already-discovered `PackageProvider` (e.g. `RustPackageProvider`,
/// covering an entire workspace) and applies the target-language materialize
/// + source-normalize transforms to every item `load_package_source` returns.
///
/// Exists so whole-package typechecking (`typecheck_package` below) can
/// register a provider that does genuine resolution for *any* package id —
/// including `std`/dependencies the driver asks about internally — instead
/// of a one-off shim that only knows how to answer for a single pre-baked
/// `PackageSource` snapshot. `list_packages`/`load_package_metadata`/`refresh`
/// delegate straight through; only `load_package_source` adds work.
pub struct MaterializingPackageProvider {
    inner: Arc<dyn PackageProvider>,
    materializer: Option<Arc<dyn IntrinsicMaterializer>>,
    normalizer: Arc<dyn IntrinsicNormalizer>,
}

impl MaterializingPackageProvider {
    pub fn new(
        inner: Arc<dyn PackageProvider>,
        materializer: Option<Arc<dyn IntrinsicMaterializer>>,
        normalizer: Arc<dyn IntrinsicNormalizer>,
    ) -> Self {
        Self {
            inner,
            materializer,
            normalizer,
        }
    }
}

impl PackageProvider for MaterializingPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        self.inner.list_packages()
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        self.inner.load_package_metadata(id)
    }

    fn refresh(&self) -> ProviderResult<()> {
        self.inner.refresh()
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<PackageSource> {
        let mut source = self.inner.load_package_source(id)?;

        if let Some(ref mat) = self.materializer {
            for pkg_item in &mut source.items {
                let file = File {
                    path: PathBuf::new(),
                    attrs: vec![],
                    collected_items: vec![],
                    items: vec![pkg_item.item.clone()],
                };
                let file = crate::materialize::materialize_file(file, mat.as_ref())
                    .map_err(|e| ProviderError::other(e.to_string()))?;
                if let Some(item) = file.items.into_iter().next() {
                    pkg_item.item = item;
                }
            }
        }

        for pkg_item in &mut source.items {
            fp_lang::normalization::normalize_items(
                std::slice::from_mut(&mut pkg_item.item),
                self.normalizer.as_ref(),
            )
            .map_err(|e| ProviderError::other(e.to_string()))?;
        }

        Ok(source)
    }
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
/// for, pass through untouched).
pub fn typecheck_package(
    provider: Arc<dyn PackageProvider>,
    package_id: &PackageId,
    lossy: LossyCompileOptions,
    language: &str,
) -> Result<PackageSource> {
    let executor = CompilerExecutor::new();
    let workspace = compiler_workspace_for(language);
    workspace.register_provider(provider);
    let mut session = CompilerSession::new(data_layout(), &executor, workspace);
    session.driver().pipeline = PipelineMode::TypecheckedTranspile;
    session.driver().state.set_lossy(lossy.enabled);
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
    let mut items = package.items.clone();
    if let Some(lifted) = &package.lifted_ast {
        let mut typed_iter = lifted.items.iter().cloned();
        let mut mismatched = false;
        let mut typed_items = Vec::with_capacity(items.len());
        for pkg_item in &items {
            if matches!(pkg_item.item.kind(), ItemKind::Module(_)) {
                typed_items.push(pkg_item.item.clone());
                continue;
            }
            match typed_iter.next() {
                Some(typed) => typed_items.push(typed),
                None => {
                    mismatched = true;
                    break;
                }
            }
        }
        if !mismatched && typed_iter.next().is_none() {
            for (pkg_item, typed) in items.iter_mut().zip(typed_items) {
                pkg_item.item = typed;
            }
        } else {
            return Err(CliError::Compilation(format!(
                "typecheck for {} produced a mismatched item count",
                package_id.as_str()
            )));
        }
    }

    let source = PackageSource {
        package_id: package_id.clone(),
        name: package.name.clone(),
        graph: package.graph.clone(),
        module_paths: package.module_paths.clone(),
        items,
    };
    Ok(source)
}

fn parse_file_with_context(
    path: &Path,
    source_language: Option<&str>,
    parse_mode: FrontendParseMode,
    lossy: LossyCompileOptions,
) -> Result<ParsedAst> {
    let frontend = select_frontend(path, source_language)?;
    frontend.set_parse_mode(parse_mode);
    let source = std::fs::read_to_string(path).map_err(CliError::Io)?;
    let FrontendResult {
        ast,
        snapshot,
        serializer,
        diagnostics,
        ..
    } = frontend
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
    Ok(ParsedAst {
        ast,
        source_language: frontend.language().to_string(),
        frontend_snapshot: snapshot,
        serializer,
    })
}

fn select_frontend(
    path: &Path,
    source_language: Option<&str>,
) -> Result<Box<dyn LanguageFrontend>> {
    let language = source_language
        .map(|lang| lang.trim().to_ascii_lowercase())
        .or_else(|| detect_source_language(path).map(|lang| lang.name.to_ascii_lowercase()))
        .unwrap_or_else(|| languages::FERROPHASE.to_string());

    match language.as_str() {
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
    let mut rendered = match diagnostic.level {
        TypingDiagnosticLevel::Error => Diagnostic::error(diagnostic.message.clone()),
        TypingDiagnosticLevel::Warning => Diagnostic::warning(diagnostic.message.clone()),
    }
    .with_source_context("typing".to_string());

    if let Some(span) = diagnostic.span {
        rendered = rendered.with_span(span);
    }

    rendered
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

struct ParsedAst {
    ast: File,
    source_language: String,
    frontend_snapshot: Option<FrontendSnapshot>,
    serializer: Arc<dyn fp_core::ast::AstSerializer>,
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
        for (dependency_id, dep_package) in self.driver.state.typing_ctx.env_ctx.crates().iter() {
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
