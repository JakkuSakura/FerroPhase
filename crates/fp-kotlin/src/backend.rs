#[cfg(test)]
use crate::materialize::KotlinMaterializer;
use crate::materialize::materialize_kotlin_item;
use crate::serializer::*;
use fp_core::ast::package::AstPackage;
#[cfg(test)]
use fp_core::ast::{BlockStmt, Expr, ExprKind, Item, ItemKind, Name, Ty};
use fp_core::ast::{Ident, Path};
use fp_core::backend::{BackendConfig, PackageWriter, TargetBackend};
#[cfg(test)]
use fp_core::intrinsics::{IntrinsicMaterializer, MaterializeOutcome};
use std::collections::{HashMap, HashSet};
use std::path::{Path as FsPath, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

struct KotlinScan {
    ctx: KotlinWorkspaceContext,
    workspace_packages: HashSet<String>,
    /// Every package selected for Kotlin emission, sorted — used by
    /// `write_workspace_files` for `settings.gradle.kts`'s `include(...)`
    /// lines.
    package_names: Vec<String>,
    kotlin_packages: HashMap<String, String>,
}

const RUNTIME_PROJECT: &str = "fp-kotlin-runtime";

fn collect_kotlin_operation_decls(
    declarations: &[crate::kt_parser::KtDecl],
    module_path: &[String],
    registry: &mut fp_core::lang::LangItemRegistry,
) {
    for declaration in declarations {
        if let Some(op_name) = declaration.op_func.as_deref() {
            let mut path = module_path.to_vec();
            path.push(declaration.name.clone());
            registry.insert_op(
                op_name,
                Path::plain(path.into_iter().map(Ident::new).collect()),
            );
        }
        if let (Some(op_class), Some(op_method)) = (
            declaration.op_class.as_deref(),
            declaration.op_method.as_deref(),
        ) {
            let mut path = module_path.to_vec();
            path.push(declaration.name.clone());
            registry.insert_method_declaration(
                op_class,
                op_method,
                declaration.params.len(),
                fp_core::intrinsics::ResultTypeRule::NotStaticallyKnowable,
                Path::plain(path.into_iter().map(Ident::new).collect()),
            );
        }
        if let Some(op_class) = declaration.op_class.as_deref() {
            for member in &declaration.members {
                let Some(op_method) = member.op_method.as_deref() else {
                    continue;
                };
                let mut path = module_path.to_vec();
                path.push(declaration.name.clone());
                path.push(member.name.clone());
                registry.insert_method_declaration(
                    op_class,
                    op_method,
                    member.params.len(),
                    fp_core::intrinsics::ResultTypeRule::NotStaticallyKnowable,
                    Path::plain(path.into_iter().map(Ident::new).collect()),
                );
            }
        }
        let mut nested_path = module_path.to_vec();
        nested_path.push(declaration.name.clone());
        collect_kotlin_operation_decls(&declaration.members, &nested_path, registry);
    }
}

fn kotlin_operation_registry() -> Option<fp_core::lang::LangItemRegistry> {
    let diagnostics = fp_core::diagnostics::DiagnosticManager::new();
    let mut registry = fp_core::lang::LangItemRegistry::default();
    for (relative, declarations) in crate::kt_parser::load_std_declarations(&diagnostics)? {
        let mut module_path = vec!["kotlin".to_string()];
        if let Some(parent) = relative.parent() {
            module_path.extend(
                parent
                    .iter()
                    .filter_map(|segment| segment.to_str().map(str::to_owned)),
            );
        }
        collect_kotlin_operation_decls(&declarations, &module_path, &mut registry);
    }
    Some(registry)
}

/// `TargetBackend` wrapper around [`KotlinSerializer`]. Kotlin needs
/// workspace-wide context beyond what `BackendConfig` carries — the
/// workspace-wide `KotlinScan` is read lazily from `&AstProgram` on
/// first `emit_package_artifact`/`write_workspace_files` call, same as every
/// other backend gets its input. `config.root_name` (the *source* project
/// directory's name, not `config.workspace_root`, the output directory)
/// is read straight off `self.config` — `AstProgram` has no way to
/// reconstruct it, it isn't package data at all.
pub struct KotlinBackend {
    serializer: KotlinSerializer,
    config: BackendConfig,
    scan: std::sync::OnceLock<KotlinScan>,
    staging_root: std::sync::OnceLock<PathBuf>,
    output_published: std::sync::OnceLock<()>,
}

impl KotlinBackend {
    pub fn new(config: BackendConfig) -> Self {
        Self {
            serializer: KotlinSerializer,
            config,
            scan: std::sync::OnceLock::new(),
            staging_root: std::sync::OnceLock::new(),
            output_published: std::sync::OnceLock::new(),
        }
    }

    /// Builds and caches the workspace-wide scan from `&AstProgram`
    /// on first call. Safe to call from any package's `emit_package_artifact` —
    /// including the very first — since `run_named_target`'s typecheck
    /// phase already ran for every package in the workspace before any
    /// `emit_package_artifact` call happens.
    fn ensure_scan(
        &self,
        workspace: &fp_core::ast::program::AstProgram,
    ) -> fp_core::error::Result<&KotlinScan> {
        if let Some(scan) = self.scan.get() {
            return Ok(scan);
        }
        // The compiler loads dependencies for resolution, but only selected
        // roots become Kotlin projects. Keeping this set separate prevents
        // Rust sysroot crates such as `core` and `libc` from becoming Gradle
        // project dependencies without emitted Kotlin artifacts.
        let workspace_packages: HashSet<String> = self
            .config
            .emitted_packages
            .iter()
            .map(|package_id| package_id.as_str().to_owned())
            .collect();
        let kotlin_packages = workspace_packages
            .iter()
            .flat_map(|name| {
                let package =
                    kotlin_package_name(self.config.kotlin_package_prefix.as_deref(), name);
                [
                    (name.clone(), package.clone()),
                    (name.replace('-', "_"), package),
                ]
            })
            .collect::<HashMap<_, _>>();
        let sources: Vec<AstPackage> = workspace_packages
            .iter()
            .map(|name| workspace.package_source(&fp_core::ast::package::PackageId::new(name)))
            .collect::<fp_core::error::Result<_>>()?;
        let ctx = KotlinWorkspaceContext::collect(sources.iter());
        let mut package_names: Vec<String> = sources.iter().map(|s| s.name.clone()).collect();
        package_names.sort();
        let _ = self.scan.set(KotlinScan {
            ctx,
            workspace_packages,
            package_names,
            kotlin_packages,
        });
        Ok(self.scan.get().expect("just set above"))
    }

    fn initialize_output(&self) -> fp_core::error::Result<()> {
        if self.staging_root.get().is_none() {
            let staging_root = create_staging_directory(&self.config.workspace_root)?;
            self.staging_root
                .set(staging_root)
                .map_err(|_| fp_core::error::Error::from("Kotlin output initialization raced"))?;
        }
        Ok(())
    }

    fn output_root(&self) -> fp_core::error::Result<&FsPath> {
        self.initialize_output()?;
        self.staging_root
            .get()
            .map(PathBuf::as_path)
            .ok_or_else(|| fp_core::error::Error::from("Kotlin output staging was not initialized"))
    }

    fn publish_output(&self) -> fp_core::error::Result<()> {
        if self.output_published.get().is_some() {
            return Ok(());
        }
        publish_staged_workspace(self.output_root()?, &self.config.workspace_root)?;
        self.output_published
            .set(())
            .map_err(|_| fp_core::error::Error::from("Kotlin output publication raced"))?;
        Ok(())
    }
}

static STAGING_SEQUENCE: AtomicU64 = AtomicU64::new(0);

fn create_staging_directory(workspace_root: &FsPath) -> fp_core::error::Result<PathBuf> {
    let parent = workspace_root.parent().unwrap_or_else(|| FsPath::new("."));
    std::fs::create_dir_all(parent)?;
    let name = workspace_root
        .file_name()
        .and_then(|name| name.to_str())
        .filter(|name| !name.is_empty())
        .unwrap_or("workspace");

    for _ in 0..100 {
        let sequence = STAGING_SEQUENCE.fetch_add(1, Ordering::Relaxed);
        let staging_root = parent.join(format!(
            ".{name}.fp-kotlin-staging-{}-{sequence}",
            std::process::id()
        ));
        match std::fs::create_dir(&staging_root) {
            Ok(()) => return Ok(staging_root),
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(error) => return Err(error.into()),
        }
    }

    Err(fp_core::error::Error::from(format!(
        "could not create Kotlin staging directory beside {}",
        workspace_root.display()
    )))
}

fn publish_staged_workspace(
    staging_root: &FsPath,
    workspace_root: &FsPath,
) -> fp_core::error::Result<()> {
    match std::fs::symlink_metadata(workspace_root) {
        Ok(_) => {}
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            std::fs::rename(staging_root, workspace_root)?;
            return Ok(());
        }
        Err(error) => return Err(error.into()),
    }

    let backup_root = create_staging_directory(workspace_root)?;
    std::fs::remove_dir(&backup_root)?;
    std::fs::rename(workspace_root, &backup_root)?;
    if let Err(publish_error) = std::fs::rename(staging_root, workspace_root) {
        return match std::fs::rename(&backup_root, workspace_root) {
            Ok(()) => Err(publish_error.into()),
            Err(restore_error) => Err(fp_core::error::Error::from(format!(
                "failed to publish Kotlin output ({publish_error}) and restore the previous workspace ({restore_error})"
            ))),
        };
    }

    remove_path(&backup_root)?;
    Ok(())
}

fn remove_path(path: &FsPath) -> std::io::Result<()> {
    if std::fs::symlink_metadata(path)?.file_type().is_dir() {
        std::fs::remove_dir_all(path)
    } else {
        std::fs::remove_file(path)
    }
}

fn kotlin_package_name(prefix: Option<&str>, package: &str) -> String {
    let crate_name = package.replace('-', "_");
    match prefix
        .map(|prefix| prefix.trim_end_matches('.'))
        .filter(|prefix| !prefix.is_empty())
    {
        Some(prefix) => format!("{prefix}.{crate_name}"),
        None => crate_name,
    }
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::{AtomicU64, Ordering};

    use fp_core::ast::path::PathPrefix;
    use fp_core::ast::{
        Expr, ExprBlock, ExprKind, File, Ident, Item, ItemDefFunction, ItemKind, Name,
        ParameterPath, ParameterPathSegment, Path, StmtLet, Ty, TypeArray, TypeInt, TypePrimitive,
        TypeSlice, TypeVec,
    };

    use super::{
        collect_kotlin_operation_decls, create_staging_directory, kotlin_operation_registry,
        kotlin_package_name, kotlin_runtime_source, materialize_io_error_constructor,
        materialize_kotlin_ty, materialize_kotlin_types, publish_staged_workspace, remove_path,
    };

    static TEST_SEQUENCE: AtomicU64 = AtomicU64::new(0);

    #[test]
    fn kotlin_operation_declarations_become_attribute_paths() {
        let member = crate::kt_parser::KtDecl {
            kind: crate::kt_parser::KtDeclKind::Function,
            name: "unwrapOr".to_string(),
            type_params: Vec::new(),
            receiver: None,
            params: Vec::new(),
            return_type: None,
            supertypes: Vec::new(),
            is_mutable: false,
            members: Vec::new(),
            op_class: None,
            op_method: Some("unwrap_or".to_string()),
            op_func: None,
        };
        let class = crate::kt_parser::KtDecl {
            kind: crate::kt_parser::KtDeclKind::Class,
            name: "OptionBox".to_string(),
            type_params: Vec::new(),
            receiver: None,
            params: Vec::new(),
            return_type: None,
            supertypes: Vec::new(),
            is_mutable: false,
            members: vec![member],
            op_class: Some("Option".to_string()),
            op_method: None,
            op_func: None,
        };
        let mut registry = fp_core::lang::LangItemRegistry::default();
        collect_kotlin_operation_decls(&[class], &["kotlin".to_string()], &mut registry);
        assert!(
            registry
                .resolve_operation(fp_core::lang::OperationSelector::DeclarationKey(
                    "Option.unwrap_or"
                ))
                .is_some()
        );
        assert!(registry.get_op_path("unwrap_or").is_none());
    }

    #[test]
    fn vendored_kotlin_std_registers_native_portable_operations() {
        let registry = kotlin_operation_registry().expect("load Kotlin std operations");
        for key in [
            "Any.to_string",
            "str.to_string",
            "str.starts_with",
            "str.ends_with",
            "str.trim",
            "str.trim_start",
            "str.trim_end",
        ] {
            assert!(
                registry
                    .resolve_operation(fp_core::lang::OperationSelector::DeclarationKey(key))
                    .is_some(),
                "missing Kotlin std operation declaration: {key}"
            );
        }
    }

    fn test_workspace_root(name: &str) -> std::path::PathBuf {
        std::env::temp_dir().join(format!(
            "fp-kotlin-{name}-{}-{}",
            std::process::id(),
            TEST_SEQUENCE.fetch_add(1, Ordering::Relaxed)
        ))
    }

    #[test]
    fn incomplete_generation_leaves_existing_workspace_untouched() {
        let workspace_root = test_workspace_root("incomplete-output");
        std::fs::create_dir_all(&workspace_root).expect("create old workspace");
        let old_file = workspace_root.join("previous.kt");
        std::fs::write(&old_file, "previous generation").expect("write old workspace");

        let staging_root = create_staging_directory(&workspace_root).expect("create staging");
        std::fs::write(staging_root.join("partial.kt"), "partial generation")
            .expect("write staged output");

        assert_eq!(
            std::fs::read_to_string(&old_file).expect("read old workspace"),
            "previous generation"
        );
        assert!(!workspace_root.join("partial.kt").exists());

        remove_path(&staging_root).expect("remove abandoned staging");
        remove_path(&workspace_root).expect("remove test workspace");
    }

    #[test]
    fn completed_generation_replaces_existing_workspace() {
        let workspace_root = test_workspace_root("published-output");
        std::fs::create_dir_all(&workspace_root).expect("create old workspace");
        std::fs::write(workspace_root.join("previous.kt"), "previous generation")
            .expect("write old workspace");

        let staging_root = create_staging_directory(&workspace_root).expect("create staging");
        std::fs::write(staging_root.join("current.kt"), "current generation")
            .expect("write staged output");
        publish_staged_workspace(&staging_root, &workspace_root).expect("publish staging");

        assert!(!workspace_root.join("previous.kt").exists());
        assert_eq!(
            std::fs::read_to_string(workspace_root.join("current.kt")).expect("read output"),
            "current generation"
        );

        remove_path(&workspace_root).expect("remove test workspace");
    }

    #[test]
    fn kotlin_package_name_normalizes_cargo_names_under_prefix() {
        assert_eq!(
            kotlin_package_name(Some("com.example.generated"), "skln-git"),
            "com.example.generated.skln_git"
        );
    }

    #[test]
    fn kotlin_package_name_has_no_leading_dot_without_prefix() {
        assert_eq!(kotlin_package_name(None, "skln-git"), "skln_git");
    }

    #[test]
    fn backend_materializes_rust_result_type_before_serialization() {
        let result_ty = Ty::name(Name::parameter_path(ParameterPath::new(
            PathPrefix::Plain,
            vec![ParameterPathSegment::new(
                Ident::new("Result"),
                vec![
                    Ty::ident(Ident::new("String")),
                    Ty::ident(Ident::new("CoreError")),
                ],
            )],
        )));
        let mut function = ItemDefFunction::new_simple(Ident::new("load"), ExprBlock::new());
        function.sig.ret_ty = Some(result_ty);
        let mut item = Item::new(ItemKind::DefFunction(function));

        materialize_kotlin_types(&mut item);

        let ItemKind::DefFunction(function) = item.kind() else {
            panic!("expected function");
        };
        let Ty::Expr(expr) = function.sig.ret_ty.as_ref().expect("return type") else {
            panic!("expected expression type");
        };
        let ExprKind::Name(Name::ParameterPath(path)) = expr.kind() else {
            panic!("expected parameterized Result type");
        };
        assert_eq!(path.segments.last().expect("Result segment").args.len(), 1);
    }

    #[test]
    fn backend_materializes_process_types_to_runtime_types() {
        let mut ty = Ty::name(Name::path(Path::plain(vec![Ident::new("Command")])));
        materialize_kotlin_ty(&mut ty);

        let Ty::Expr(expr) = ty else {
            panic!("expected expression type");
        };
        let ExprKind::Name(Name::Path(path)) = expr.kind() else {
            panic!("expected runtime-qualified type");
        };
        assert_eq!(path.join("."), "RustKotlinRuntime.Command");
    }

    #[test]
    fn runtime_template_provides_portable_operation_adapters() {
        let runtime = kotlin_runtime_source(None);
        for helper in [
            "fun <T> listPush",
            "fun <T> listExtend",
            "fun <T> mutableListFromIterable",
            "fun bytesFromIterable(values: ByteArray)",
            "fun <T> filterIterable",
            "fun splitWhitespace",
            "fun splitString(value: String, delimiter: Char)",
            "fun splitString(value: String, delimiter: String)",
            "fun stringLines",
            "fun <T> resultSuccess",
            "fun <T> resultFailure",
            "fun <T, R> mapResult",
            "fun <T> resultIsSuccess",
            "fun <T> resultException",
            "fun <T> resultDefault",
            "fun pathExists",
            "fun pathResolve",
            "fun commandArg",
            "fun commandArgs",
            "fun commandCurrentDir",
            "fun commandStdin",
            "fun commandStdout",
            "fun commandStderr",
            "fun commandSpawn",
            "fun commandOutput",
            "fun commandStatus",
            "fun childKill",
            "fun childWait",
            "fun childTryWait",
            "fun childWaitWithOutput",
            "fun exitStatusSuccess",
            "class ChildStdinSlot",
            "fun take(): ChildStdin?",
            "fun write(bytes: Iterable<*>)",
        ] {
            assert!(runtime.contains(helper), "missing runtime helper: {helper}");
        }
    }

    #[test]
    fn runtime_template_uses_portable_result_and_error_adapters() {
        let runtime = kotlin_runtime_source(None);

        for unsupported in [
            ".isSuccess",
            ".isFailure",
            ".getOrThrow()",
            ".getOrDefault(",
        ] {
            assert!(
                !runtime.contains(unsupported),
                "runtime must not use unsupported Result member `{unsupported}`:\n{runtime}"
            );
        }
        for helper in [
            "fun <T> resultIsSuccess(result: Result<T>): Boolean = result.exceptionOrNull() == null",
            "fun <T> resultIsFailure(result: Result<T>): Boolean = result.exceptionOrNull() != null",
            "fun <T> resultSuccess(value: T): Result<T> = when (value)",
            "fun <T> resultUnwrap(result: Result<T>): T = result.getOrNull() ?: throw resultException(result)",
            "fun <T> resultDefault(result: Result<T>, defaultValue: T): T = result.getOrElse { defaultValue }",
            "fun ioError(error: Any?): java.io.IOException = when (error)",
            "fun normalizeError(error: Any?): Throwable = error as? Throwable ?: IllegalStateException(error?.toString() ?: \"unknown error\")",
            "fun createDirectory(path: java.nio.file.Path): Result<Unit> = runCatching<Unit>",
            "fun createDirectories(path: java.nio.file.Path): Result<Unit> = runCatching<Unit>",
            "fun writeAll(stream: java.io.OutputStream, bytes: ByteArray): Result<Unit> = runCatching<Unit>",
            "suspend fun tcpWriteAll(stream: Socket, bytes: ByteArray): Result<Unit> = runCatching<Unit>",
            "fun childKill(child: Child): Result<Unit> = runCatching<Unit>",
        ] {
            assert!(
                runtime.contains(helper),
                "missing runtime contract: {helper}"
            );
        }
    }

    #[test]
    fn backend_wraps_typed_io_error_constructor_payloads() {
        let mut invoke = fp_core::ast::ExprInvoke {
            span: Default::default(),
            target: fp_core::ast::ExprInvokeTarget::Function(Name::path(Path::plain(vec![
                Ident::new("CoreError"),
                Ident::new("Io"),
            ]))),
            args: vec![Expr::name(Name::ident("error"))],
            kwargs: Vec::new(),
        };

        materialize_io_error_constructor(&mut invoke);

        let [argument] = invoke.args.as_slice() else {
            panic!("expected one Io constructor argument");
        };
        let ExprKind::Invoke(runtime_call) = argument.kind() else {
            panic!("expected ioError runtime invocation");
        };
        let fp_core::ast::ExprInvokeTarget::Function(Name::Path(path)) = &runtime_call.target
        else {
            panic!("expected runtime function path");
        };
        assert_eq!(path.join("."), "RustKotlinRuntime.ioError");
        assert!(matches!(runtime_call.args[0].kind(), ExprKind::Name(_)));
    }

    #[test]
    fn backend_materializes_json_value_to_jackson_json_node() {
        let mut ty = Ty::name(Name::path(Path::plain(vec![
            Ident::new("serde_json"),
            Ident::new("Value"),
        ])));

        materialize_kotlin_ty(&mut ty);

        let Ty::Expr(expr) = ty else {
            panic!("expected JsonNode expression type");
        };
        let ExprKind::Name(Name::Path(path)) = expr.kind() else {
            panic!("expected qualified JsonNode name");
        };
        assert_eq!(path.join("."), "com.fasterxml.jackson.databind.JsonNode");
    }

    #[test]
    fn backend_materializes_byte_vectors_to_byte_arrays() {
        let mut ty = Ty::Vec(TypeVec {
            ty: Box::new(Ty::Primitive(TypePrimitive::Int(TypeInt::U8))),
        });

        materialize_kotlin_ty(&mut ty);

        let Ty::Expr(expr) = ty else {
            panic!("expected ByteArray expression type");
        };
        let ExprKind::Name(Name::Ident(name)) = expr.kind() else {
            panic!("expected ByteArray name");
        };
        assert_eq!(name.as_str(), "ByteArray");
    }

    #[test]
    fn backend_materializes_byte_slices_and_arrays_to_byte_arrays() {
        let byte = Ty::Primitive(TypePrimitive::Int(TypeInt::U8));
        let mut slice = Ty::Slice(TypeSlice {
            elem: Box::new(byte.clone()),
        });
        let mut array = Ty::Array(TypeArray {
            elem: Box::new(byte),
            len: Box::new(Expr::value(fp_core::ast::Value::int(4))),
        });

        materialize_kotlin_ty(&mut slice);
        materialize_kotlin_ty(&mut array);

        for ty in [&slice, &array] {
            let Ty::Expr(expr) = ty else {
                panic!("expected ByteArray expression type");
            };
            let ExprKind::Name(Name::Ident(name)) = expr.kind() else {
                panic!("expected ByteArray name");
            };
            assert_eq!(name.as_str(), "ByteArray");
        }
    }

    #[test]
    fn backend_materializes_nominal_vec_types_to_kotlin_collections() {
        let mut ty = Ty::name(Name::parameter_path(ParameterPath::new(
            PathPrefix::Plain,
            vec![
                ParameterPathSegment::from_ident(Ident::new("alloc")),
                ParameterPathSegment::from_ident(Ident::new("vec")),
                ParameterPathSegment::new(Ident::new("Vec"), vec![Ty::ident(Ident::new("Entry"))]),
            ],
        )));

        materialize_kotlin_ty(&mut ty);

        let Ty::Expr(expr) = ty else {
            panic!("expected Kotlin collection type");
        };
        let ExprKind::Name(Name::ParameterPath(path)) = expr.kind() else {
            panic!("expected parameterized MutableList type");
        };
        let segment = path.last().expect("MutableList segment");
        assert_eq!(segment.ident.as_str(), "MutableList");
        let [Ty::Expr(element)] = segment.args.as_slice() else {
            panic!("expected one element type");
        };
        let ExprKind::Name(Name::Ident(element)) = element.kind() else {
            panic!("expected Entry element type");
        };
        assert_eq!(element.as_str(), "Entry");
    }

    #[test]
    fn backend_materializes_nominal_byte_vec_types_to_byte_arrays() {
        let mut ty = Ty::name(Name::parameter_path(ParameterPath::new(
            PathPrefix::Plain,
            vec![ParameterPathSegment::new(
                Ident::new("Vec"),
                vec![Ty::Primitive(TypePrimitive::Int(TypeInt::U8))],
            )],
        )));

        materialize_kotlin_ty(&mut ty);

        let Ty::Expr(expr) = ty else {
            panic!("expected ByteArray expression type");
        };
        let ExprKind::Name(Name::Ident(name)) = expr.kind() else {
            panic!("expected ByteArray name");
        };
        assert_eq!(name.as_str(), "ByteArray");
    }

    #[test]
    fn backend_materializes_path_and_os_string_types_to_jvm_types() {
        for (source, expected) in [
            ("Path", "Path"),
            ("PathBuf", "Path"),
            ("OsStr", "String"),
            ("OsString", "String"),
        ] {
            let mut ty = Ty::ident(Ident::new(source));
            materialize_kotlin_ty(&mut ty);

            let Ty::Expr(expr) = ty else {
                panic!("expected target type for {source}");
            };
            let ExprKind::Name(name) = expr.kind() else {
                panic!("expected target name for {source}");
            };
            let actual = match name {
                Name::Ident(ident) => ident.as_str(),
                Name::Path(path) => path.last().as_str(),
                Name::ParameterPath(path) => path.last().expect("path segment").ident.as_str(),
            };
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn backend_materializes_typed_local_vec_annotations() {
        let vec_ty = Ty::name(Name::parameter_path(ParameterPath::new(
            PathPrefix::Plain,
            vec![ParameterPathSegment::new(
                Ident::new("Vec"),
                vec![Ty::ident(Ident::new("Entry"))],
            )],
        )));
        let mut function = ItemDefFunction::new_simple(Ident::new("load"), ExprBlock::new());
        function
            .body
            .stmts
            .push(fp_core::ast::BlockStmt::Let(StmtLet::new_typed(
                Ident::new("entries"),
                vec_ty,
                fp_core::ast::Expr::new(ExprKind::IntrinsicContainer(
                    fp_core::ast::ExprIntrinsicContainer::VecElements {
                        elements: Vec::new(),
                    },
                )),
            )));
        let mut item = Item::new(ItemKind::DefFunction(function));

        materialize_kotlin_types(&mut item);

        let ItemKind::DefFunction(function) = item.kind() else {
            panic!("expected function");
        };
        let fp_core::ast::BlockStmt::Let(local) = &function.body.stmts[0] else {
            panic!("expected local declaration");
        };
        let fp_core::ast::PatternKind::Type(pattern) = local.pat.kind() else {
            panic!("expected typed local pattern");
        };
        let Ty::Expr(expr) = &pattern.ty else {
            panic!("expected Kotlin collection type");
        };
        let ExprKind::Name(Name::ParameterPath(path)) = expr.kind() else {
            panic!("expected parameterized MutableList type");
        };
        assert_eq!(
            path.last().expect("collection segment").ident.as_str(),
            "MutableList"
        );
    }

    #[test]
    fn backend_lets_kotlin_infer_placeholder_local_types() {
        let mut function = ItemDefFunction::new_simple(Ident::new("load"), ExprBlock::new());
        for (name, ty) in [
            ("status", Ty::ident(Ident::new("NonZero"))),
            ("kind", Ty::ident(Ident::new("ErrorKind"))),
            ("value", Ty::ANY),
        ] {
            function
                .body
                .stmts
                .push(fp_core::ast::BlockStmt::Let(StmtLet::new_typed(
                    Ident::new(name),
                    ty,
                    Expr::name(Name::ident("source")),
                )));
        }
        let mut item = Item::new(ItemKind::DefFunction(function));

        materialize_kotlin_types(&mut item);

        let ItemKind::DefFunction(function) = item.kind() else {
            panic!("expected function");
        };
        for statement in &function.body.stmts {
            let fp_core::ast::BlockStmt::Let(local) = statement else {
                panic!("expected local declaration");
            };
            assert!(
                matches!(local.pat.kind(), fp_core::ast::PatternKind::Ident(_)),
                "placeholder type must be inferred by Kotlin"
            );
        }
    }

    #[test]
    fn backend_materializes_unparameterized_vec_annotations() {
        let mut function = ItemDefFunction::new_simple(Ident::new("load"), ExprBlock::new());
        function.sig.params.push(fp_core::ast::FunctionParam::new(
            Ident::new("entries"),
            Ty::ident(Ident::new("Vec")),
        ));
        let mut item = Item::new(ItemKind::DefFunction(function));

        materialize_kotlin_types(&mut item);

        let rendered = super::KotlinSerializer
            .serialize_file(&File {
                path: Default::default(),
                attrs: Vec::new(),
                collected_items: Vec::new(),
                items: vec![item],
            })
            .expect("serialize materialized Vec parameter");
        assert!(
            rendered.contains("entries: MutableList<Any>"),
            "rendered Kotlin:\n{rendered}"
        );
        assert!(!rendered.contains("Vec"), "rendered Kotlin:\n{rendered}");
    }

    #[test]
    fn backend_materializes_rust_aliases_to_kotlin_jvm_types() {
        let named_ty = |name, args| {
            Ty::name(Name::parameter_path(ParameterPath::new(
                PathPrefix::Plain,
                vec![ParameterPathSegment::new(Ident::new(name), args)],
            )))
        };
        let mut function =
            ItemDefFunction::new_simple(Ident::new("materialized"), ExprBlock::new());
        function.sig.params = vec![
            fp_core::ast::FunctionParam::new(
                Ident::new("values"),
                named_ty("to_vec_in", vec![Ty::ident(Ident::new("str"))]),
            ),
            fp_core::ast::FunctionParam::new(
                Ident::new("optional"),
                named_ty("Option", vec![Ty::ident(Ident::new("str"))]),
            ),
            fp_core::ast::FunctionParam::new(Ident::new("generic"), Ty::ident(Ident::new("T"))),
        ];
        function.sig.ret_ty = Some(named_ty(
            "Result",
            vec![
                Ty::ident(Ident::new("T")),
                Ty::ident(Ident::new("CoreError")),
            ],
        ));
        let mut item = Item::new(ItemKind::DefFunction(function));

        materialize_kotlin_types(&mut item);

        let rendered = super::KotlinSerializer
            .serialize_file(&File {
                path: Default::default(),
                attrs: Vec::new(),
                collected_items: Vec::new(),
                items: vec![item],
            })
            .expect("serialize materialized Kotlin types");
        assert!(
            rendered.contains("values: MutableList<String>"),
            "rendered Kotlin:\n{rendered}"
        );
        assert!(
            rendered.contains("optional: String?"),
            "rendered Kotlin:\n{rendered}"
        );
        assert!(
            rendered.contains("generic: Any"),
            "rendered Kotlin:\n{rendered}"
        );
        assert!(
            rendered.contains(": Result<Any>"),
            "rendered Kotlin:\n{rendered}"
        );
        for rust_type in ["to_vec_in", "str", "Result<Any,", "Option<", ": T"] {
            assert!(
                !rendered.contains(rust_type),
                "unexpected Rust type `{rust_type}` in Kotlin:\n{rendered}"
            );
        }
    }
}

impl TargetBackend for KotlinBackend {
    fn capabilities(&self) -> fp_core::capabilities::LanguageCapabilities {
        crate::CAPABILITIES
    }

    fn intrinsic_materializer(
        &self,
    ) -> Option<std::sync::Arc<dyn fp_core::intrinsics::IntrinsicMaterializer>> {
        Some(std::sync::Arc::new(crate::KotlinMaterializer))
    }

    fn portable_operation_registry(&self) -> Option<fp_core::lang::LangItemRegistry> {
        kotlin_operation_registry()
    }

    fn emit_package_artifact(
        &self,
        workspace: &fp_core::ast::program::AstProgram,
        package_id: &fp_core::ast::package::PackageId,
        mir: &fp_core::mir::MirCodeUnit,
        lir: Option<&fp_core::lir::LirBlob>,
    ) -> fp_core::error::Result<()> {
        let output_root = self.output_root()?;
        let scan = self.ensure_scan(workspace)?;
        // Materialize the central portable-operation representation into
        // Kotlin constructs before serialization. `package_source` derives
        // from this compiled package, so the materialized AST is what the
        // serializer consumes.
        {
            let compiled = workspace.compiled_package(package_id).ok_or_else(|| {
                fp_core::error::Error::from(format!(
                    "package `{package_id}` is unavailable for materialization"
                ))
            })?;
            let mut compiled = compiled.borrow_mut();
            for pkg_item in &mut compiled.items {
                pkg_item.item = materialize_kotlin_item(pkg_item.item.clone())?;
            }
        }
        let package = workspace.package_source(package_id)?;
        let package = &package;
        let files = self.serializer.serialize_package(
            package,
            &scan.workspace_packages,
            &scan.kotlin_packages,
            &scan.ctx,
        )?;
        let writer = PackageWriter::new(output_root.join(&package.name));
        for (mod_path, code) in files {
            let rel = if mod_path.contains('.') {
                mod_path
            } else {
                format!("{}.kt", mod_path)
            };
            writer.write_file(&rel, code)?;
        }
        Ok(())
    }

    fn write_workspace_files(
        &self,
        workspace: &fp_core::ast::program::AstProgram,
    ) -> fp_core::error::Result<()> {
        let output_root = self.output_root()?;
        let scan = self.ensure_scan(workspace)?;
        let root_name = self.config.root_name.replace('-', "_");
        let mut projects = vec![format!("include(\":{RUNTIME_PROJECT}\")")];
        projects.extend(
            scan.package_names
                .iter()
                .map(|name| format!("include(\":{name}\")")),
        );
        let settings = format!(
            "rootProject.name = \"{root_name}\"\n\n{}\n",
            projects.join("\n")
        );
        let writer = PackageWriter::new(output_root.to_path_buf());
        writer.write_file("settings.gradle.kts", settings)?;
        writer.write_file(
            "build.gradle.kts",
            "plugins {\n    kotlin(\"jvm\") version \"2.1.0\" apply false\n}\n\n\
             allprojects {\n    repositories { mavenCentral() }\n}\n",
        )?;
        writer.write_file(
            &format!("{RUNTIME_PROJECT}/build.gradle.kts"),
            runtime_build_gradle(),
        )?;
        writer.write_file(
            &format!("{RUNTIME_PROJECT}/src/main/kotlin/runtime.kt"),
            kotlin_runtime_source(self.config.kotlin_package_prefix.as_deref()),
        )?;
        self.publish_output()
    }
}

fn runtime_build_gradle() -> &'static str {
    "plugins {\n    kotlin(\"jvm\") version \"2.1.0\"\n    kotlin(\"plugin.serialization\") version \"2.1.0\"\n}\n\n\
     repositories {\n    mavenCentral()\n}\n\n\
     dependencies {\n\
         implementation(\"org.jetbrains.kotlinx:kotlinx-coroutines-core:1.9.0\")\n\
         implementation(\"org.jetbrains.kotlinx:kotlinx-serialization-json:1.7.3\")\n\
         implementation(\"org.tomlj:tomlj:1.1.1\")\n\
         implementation(\"com.fasterxml.jackson.module:jackson-module-kotlin:2.18.2\")\n\
     }\n\n\
     kotlin {\n    jvmToolchain(21)\n}\n"
}

fn kotlin_runtime_source(prefix: Option<&str>) -> String {
    let package = match prefix
        .map(|prefix| prefix.trim_end_matches('.'))
        .filter(|prefix| !prefix.is_empty())
    {
        Some(prefix) => format!("{prefix}.runtime"),
        None => "runtime".to_owned(),
    };
    format!(
        "package {package}\n\n\
         import com.fasterxml.jackson.core.type.TypeReference\n\
         import com.fasterxml.jackson.databind.ObjectMapper\n\
         import com.fasterxml.jackson.module.kotlin.jacksonObjectMapper\n\
         import java.net.Socket\n\
         import java.nio.charset.StandardCharsets\n\
         import kotlinx.coroutines.Dispatchers\n\
         import kotlinx.coroutines.delay\n\
         import kotlinx.coroutines.withContext\n\n\
         object RustKotlinRuntime {{\n\
             @PublishedApi internal val mapper: ObjectMapper = jacksonObjectMapper()\n\
             fun decodeUtf8(bytes: ByteArray): String = bytes.toString(StandardCharsets.UTF_8)\n\
             fun encodeUtf8(value: String): ByteArray = value.toByteArray(StandardCharsets.UTF_8)\n\
             fun appendByte(bytes: ByteArray, byte: Byte): ByteArray = bytes + byte\n\
             fun appendBytes(bytes: ByteArray, suffix: ByteArray): ByteArray = bytes + suffix\n\
             fun repeatByte(byte: Byte, count: Int): ByteArray = ByteArray(count) {{ byte }}\n\
             fun <T> listPush(values: MutableList<T>, value: T): MutableList<T> = values.apply {{ add(value) }}\n\
             fun <T> listExtend(values: MutableList<T>, suffix: Iterable<T>): MutableList<T> = values.apply {{ addAll(suffix) }}\n\
             fun <T> mutableListFromIterable(values: Iterable<T>): MutableList<T> = values.toMutableList()\n\
             fun bytesFromIterable(values: Iterable<Byte>): ByteArray = values.toList().toByteArray()\n\
             fun bytesFromIterable(values: ByteArray): ByteArray = values.copyOf()\n\
             fun <T> filterIterable(values: Iterable<T>, predicate: (T) -> Boolean): MutableList<T> = values.filter(predicate).toMutableList()\n\
             fun splitWhitespace(value: String): MutableList<String> = value.trim().split(Regex(\"\\\\s+\")).filter {{ it.isNotEmpty() }}.toMutableList()\n\
             fun splitString(value: String, delimiter: Char): MutableList<String> = value.split(delimiter).toMutableList()\n\
             fun splitString(value: String, delimiter: String): MutableList<String> = value.split(delimiter).toMutableList()\n\
             fun stringLines(value: String): MutableList<String> = value.lines().toMutableList()\n\
             fun charIndices(value: String): MutableList<Pair<Int, Char>> = value.withIndex().map {{ Pair(it.index, it.value) }}.toMutableList()\n\
             fun splitAt(value: String, index: Long): Pair<String, String> = Pair(value.substring(0, index.toInt()), value.substring(index.toInt()))\n\
             fun stripPrefix(value: String, prefix: String): String? = value.takeIf {{ it.startsWith(prefix) }}?.removePrefix(prefix)\n\
             fun <T> thenSome(condition: Boolean, value: T): T? = if (condition) value else null\n\
             fun <T, R> findMap(values: Iterable<T>, transform: (T) -> R?): R? = values.firstNotNullOfOrNull(transform)\n\
             fun rangeInclusiveContains(range: Any?, value: Long): Boolean = when (range) {{ is ClosedRange<*> -> (range.start as? Long)?.let {{ start -> (range.endInclusive as? Long)?.let {{ end -> value in start..end }} }} ?: false; else -> false }}\n\
             fun readDirectory(path: java.nio.file.Path): Result<List<DirEntry>> = runCatching {{\n\
                 java.nio.file.Files.list(path).use {{ entries -> entries.map(::DirEntry).toList() }}\n\
             }}\n\
             fun createDirectory(path: java.nio.file.Path): Result<Unit> = runCatching<Unit> {{ java.nio.file.Files.createDirectory(path); Unit }}\n\
             fun createDirectories(path: java.nio.file.Path): Result<Unit> = runCatching<Unit> {{ java.nio.file.Files.createDirectories(path); Unit }}\n\
             fun createFile(path: java.nio.file.Path): Result<java.io.OutputStream> = runCatching {{ java.nio.file.Files.newOutputStream(path, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING, java.nio.file.StandardOpenOption.WRITE) }}\n\
             fun canonicalize(path: java.nio.file.Path): Result<java.nio.file.Path> = runCatching {{ path.toRealPath() }}\n\
             fun writeAll(stream: java.io.OutputStream, bytes: ByteArray): Result<Unit> = runCatching<Unit> {{ stream.write(bytes); Unit }}\n\
             inline fun <reified T> jsonFromString(input: String): Result<T> = runCatching {{ mapper.readValue(input, object : TypeReference<T>() {{}}) }}\n\
             fun jsonToString(value: Any?): Result<String> = runCatching {{ mapper.writeValueAsString(value) }}\n\
             inline fun <reified T> tomlFromString(input: String): Result<T> = runCatching {{ mapper.convertValue(org.tomlj.Toml.parse(input).toMap(), object : TypeReference<T>() {{}}) }}\n\
             suspend fun tcpConnect(address: String): Result<Socket> = runCatching {{ withContext(Dispatchers.IO) {{ val separator = address.lastIndexOf(':'); require(separator > 0) {{ \"TCP address must be host:port\" }}; Socket(address.substring(0, separator), address.substring(separator + 1).toInt()) }} }}\n\
             suspend fun tcpWriteAll(stream: Socket, bytes: ByteArray): Result<Unit> = runCatching<Unit> {{ withContext(Dispatchers.IO) {{ stream.getOutputStream().write(bytes); Unit }} }}\n\
             suspend fun sleep(duration: java.time.Duration) {{ delay(duration.toMillis()) }}\n\
             fun normalizeError(error: Any?): Throwable = error as? Throwable ?: IllegalStateException(error?.toString() ?: \"unknown error\")\n\
             fun ioError(error: Any?): java.io.IOException = when (error) {{\n\
                 is java.io.IOException -> error\n\
                 is Throwable -> java.io.IOException(error.message, error)\n\
                 else -> java.io.IOException(error?.toString() ?: \"unknown I/O error\")\n\
             }}\n\
             fun <T : Any> optionUnwrap(value: T?): T = requireNotNull(value)\n\
             @Suppress(\"UNCHECKED_CAST\")\n\
             fun <T> resultSuccess(value: T): Result<T> = when (value) {{\n\
                 is Result<*> -> value as Result<T>\n\
                 else -> Result.success(value)\n\
             }}\n\
             fun <T> resultFailure(error: Any?): Result<T> = Result.failure(normalizeError(error))\n\
             fun <T, R> mapResult(result: Result<T>, transform: (T) -> R): Result<R> = result.map(transform)\n\
             fun <T> resultIsSuccess(result: Result<T>): Boolean = result.exceptionOrNull() == null\n\
             fun <T> resultIsFailure(result: Result<T>): Boolean = result.exceptionOrNull() != null\n\
             fun <T> resultOkValue(result: Result<T>): T? = result.getOrNull()\n\
             fun <T> resultErrValue(result: Result<T>): Throwable? = result.exceptionOrNull()\n\
             fun <T> resultException(result: Result<T>): Throwable = requireNotNull(result.exceptionOrNull())\n\
             fun <T> resultUnwrap(result: Result<T>): T = result.getOrNull() ?: throw resultException(result)\n\
             fun <T> resultDefault(result: Result<T>, defaultValue: T): T = result.getOrElse {{ defaultValue }}\n\
             inline fun <reified T> parse(input: String): Result<T> = runCatching {{ when (T::class) {{ Int::class -> input.toInt(); Long::class -> input.toLong(); Short::class -> input.toShort(); Byte::class -> input.toByte(); Double::class -> input.toDouble(); Float::class -> input.toFloat(); Boolean::class -> input.toBooleanStrict(); String::class -> input; else -> error(\"unsupported Rust FromStr target: ${{T::class.qualifiedName}}\") }} as T }}\n\
             fun <T> unwrapOr(value: T?, defaultValue: T): T = value ?: defaultValue\n\
             fun <T, R> mapOr(value: T?, defaultValue: R, transform: (T) -> R): R = value?.let(transform) ?: defaultValue\n\
             fun <T> mapError(result: Result<T>, transform: (Throwable) -> Throwable): Result<T> {{\n\
                 val error = result.exceptionOrNull() ?: return result\n\
                 return Result.failure(transform(error))\n\
             }}\n\
             fun pathExists(path: java.nio.file.Path): Boolean = java.nio.file.Files.exists(path)\n\
             fun pathResolve(path: java.nio.file.Path, other: Any?): java.nio.file.Path = path.resolve(other.toString())\n\
             fun deleteRecursively(path: java.nio.file.Path) {{\n\
                 if (!java.nio.file.Files.exists(path)) return\n\
                 java.nio.file.Files.walk(path).use {{ paths ->\n\
                     paths.sorted(java.util.Comparator.reverseOrder()).forEach(java.nio.file.Files::delete)\n\
                 }}\n\
             }}\n\
             enum class Stdio {{ PIPED, INHERIT, NULL }}\n\
             fun pipedStdio(): Stdio = Stdio.PIPED\n\
             fun inheritStdio(): Stdio = Stdio.INHERIT\n\
             fun nullStdio(): Stdio = Stdio.NULL\n\
             fun command(program: String): Command = Command(program)\n\
             fun commandArg(command: Command, value: Any?): Command = command.arg(value)\n\
             fun commandArgs(command: Command, values: Iterable<*>): Command = command.args(values)\n\
             fun commandCurrentDir(command: Command, path: java.nio.file.Path): Command = command.currentDir(path)\n\
             fun commandStdin(command: Command, value: Stdio): Command = command.stdin(value)\n\
             fun commandStdout(command: Command, value: Stdio): Command = command.stdout(value)\n\
             fun commandStderr(command: Command, value: Stdio): Command = command.stderr(value)\n\
             fun commandSpawn(command: Command): Result<Child> = runCatching {{ command.spawn() }}\n\
             fun commandOutput(command: Command): Result<Output> = runCatching {{ command.output() }}\n\
             fun commandStatus(command: Command): Result<ExitStatus> = runCatching {{ command.status() }}\n\
             fun childKill(child: Child): Result<Unit> = runCatching<Unit> {{ child.kill(); Unit }}\n\
             fun childWait(child: Child): Result<ExitStatus> = runCatching {{ child.waitForStatus() }}\n\
             fun childTryWait(child: Child): Result<ExitStatus?> = runCatching {{ child.tryWait() }}\n\
             fun childWaitWithOutput(child: Child): Result<Output> = runCatching {{ child.waitWithOutput() }}\n\
             fun exitStatusSuccess(status: ExitStatus): Boolean = status.success()\n\
             class ExitStatus(private val code: Int) {{\n\
                 fun success(): Boolean = code == 0\n\
                 override fun toString(): String = code.toString()\n\
             }}\n\
             class FileType(private val attributes: java.nio.file.attribute.BasicFileAttributes) {{\n\
                 fun isDirectory(): Boolean = attributes.isDirectory\n\
             }}\n\
             class DirEntry(private val value: java.nio.file.Path) {{\n\
                 fun path(): java.nio.file.Path = value\n\
                 fun fileType(): FileType = FileType(java.nio.file.Files.readAttributes(value, java.nio.file.attribute.BasicFileAttributes::class.java))\n\
             }}\n\
             data class Output(val status: ExitStatus, val stdout: ByteArray, val stderr: ByteArray)\n\
             class ChildStdin(private val stream: java.io.OutputStream) {{\n\
                 fun write(bytes: ByteArray): Result<Int> = runCatching {{ stream.write(bytes); bytes.size }}\n\
                 fun write(bytes: Iterable<*>): Result<Int> = runCatching {{\n\
                     val materialized = bytes.map {{ (it as Number).toByte() }}.toByteArray()\n\
                     stream.write(materialized)\n\
                     materialized.size\n\
                 }}\n\
                 fun close() {{ stream.close() }}\n\
             }}\n\
             class ChildStdinSlot(stream: java.io.OutputStream?) {{\n\
                 private var stream: java.io.OutputStream? = stream\n\
                 fun take(): ChildStdin? = stream?.let {{ ChildStdin(it).also {{ stream = null }} }}\n\
                 fun close() {{ stream?.close(); stream = null }}\n\
             }}\n\
             class Child(private val process: Process) {{\n\
                 val stdin = ChildStdinSlot(process.outputStream)\n\
                 fun kill(): Unit = process.destroyForcibly().let {{ }}\n\
                 fun waitForStatus(): ExitStatus {{ stdin.close(); return ExitStatus(process.waitFor()) }}\n\
                 fun wait(): ExitStatus = waitForStatus()\n\
                 fun tryWait(): ExitStatus? = if (process.isAlive) null else ExitStatus(process.exitValue())\n\
                 fun waitWithOutput(): Output {{\n\
                     stdin.close()\n\
                     val stdout = process.inputStream.readBytes()\n\
                     val stderr = process.errorStream.readBytes()\n\
                     return Output(ExitStatus(process.waitFor()), stdout, stderr)\n\
                 }}\n\
             }}\n\
             class Command(private val program: String) {{\n\
                 private val builder = ProcessBuilder(program)\n\
                 fun arg(value: Any?): Command = apply {{ builder.command().add(value.toString()) }}\n\
                 fun args(values: Iterable<*>): Command = apply {{ values.forEach {{ builder.command().add(it.toString()) }} }}\n\
                 fun currentDir(path: java.nio.file.Path): Command = apply {{ builder.directory(path.toFile()) }}\n\
                 fun stdin(value: Stdio): Command = apply {{ builder.redirectInput(value.redirect()) }}\n\
                 fun stdout(value: Stdio): Command = apply {{ builder.redirectOutput(value.redirect()) }}\n\
                 fun stderr(value: Stdio): Command = apply {{ builder.redirectError(value.redirect()) }}\n\
                 fun spawn(): Child = Child(builder.start())\n\
                 fun output(): Output {{\n\
                     builder.redirectInput(ProcessBuilder.Redirect.PIPE)\n\
                     builder.redirectOutput(ProcessBuilder.Redirect.PIPE)\n\
                     builder.redirectError(ProcessBuilder.Redirect.PIPE)\n\
                     val process = builder.start()\n\
                     val stdout = process.inputStream.readBytes()\n\
                     val stderr = process.errorStream.readBytes()\n\
                     return Output(ExitStatus(process.waitFor()), stdout, stderr)\n\
                 }}\n\
                 fun status(): ExitStatus = ExitStatus(builder.start().waitFor())\n\
             }}\n\
             private fun Stdio.redirect(): ProcessBuilder.Redirect = when (this) {{\n\
                 Stdio.PIPED -> ProcessBuilder.Redirect.PIPE\n\
                 Stdio.INHERIT -> ProcessBuilder.Redirect.INHERIT\n\
                 Stdio.NULL -> ProcessBuilder.Redirect.DISCARD\n\
             }}\n\
         }}\n"
    )
}

/// Normalizes Rust type syntax into Kotlin's type model before serialization.
/// The serializer receives only target-shaped types and prints them verbatim.
#[cfg(test)]
fn materialize_kotlin_types(item: &mut Item) {
    match item.kind_mut() {
        ItemKind::Module(module) => {
            for item in &mut module.items {
                materialize_kotlin_types(item);
            }
        }
        ItemKind::DefStruct(def) => materialize_struct_fields(&mut def.value.fields),
        ItemKind::DefStructural(def) => materialize_struct_fields(&mut def.value.fields),
        ItemKind::DefEnum(def) => {
            for variant in &mut def.value.variants {
                materialize_kotlin_ty(&mut variant.value);
            }
        }
        ItemKind::DefType(def) => materialize_kotlin_ty(&mut def.value),
        ItemKind::DefFunction(def) => {
            materialize_signature(&mut def.sig);
            materialize_block(&mut def.body.stmts);
        }
        ItemKind::DefConst(def) => {
            if let Some(ty) = &mut def.ty_annotation {
                materialize_kotlin_ty(ty);
            }
        }
        ItemKind::DefStatic(def) => {
            if let Some(ty) = &mut def.ty_annotation {
                materialize_kotlin_ty(ty);
            }
        }
        ItemKind::DefTrait(def) => {
            for item in &mut def.items {
                materialize_kotlin_types(item);
            }
        }
        ItemKind::Impl(def) => {
            for item in &mut def.items {
                materialize_kotlin_types(item);
            }
        }
        ItemKind::DeclFunction(def) => materialize_signature(&mut def.sig),
        ItemKind::DeclConst(def) => {
            if let Some(ty) = &mut def.ty_annotation {
                materialize_kotlin_ty(ty);
            }
        }
        ItemKind::DeclStatic(def) => {
            if let Some(ty) = &mut def.ty_annotation {
                materialize_kotlin_ty(ty);
            }
        }
        ItemKind::Expr(_)
        | ItemKind::Import(_)
        | ItemKind::OpaqueType(_)
        | ItemKind::DeclType(_)
        | ItemKind::Macro(_)
        | ItemKind::ConstBlock(_)
        | ItemKind::PrecompiledAsm(_)
        | ItemKind::PrecompiledLir(_)
        | ItemKind::PrecompiledArtifact(_) => {}
    }
}

#[cfg(test)]
fn materialize_struct_fields(fields: &mut [fp_core::ast::StructuralField]) {
    for field in fields {
        materialize_kotlin_ty(&mut field.value);
    }
}
#[cfg(test)]
fn materialize_signature(sig: &mut fp_core::ast::FunctionSignature) {
    for param in &mut sig.params {
        materialize_kotlin_ty(&mut param.ty);
        if let Some(ty) = &mut param.ty_annotation {
            materialize_kotlin_ty(ty);
        }
    }
    if let Some(ty) = &mut sig.ret_ty {
        materialize_kotlin_ty(ty);
    }
}
#[cfg(test)]
fn materialize_block(stmts: &mut [BlockStmt]) {
    for stmt in stmts {
        match stmt {
            BlockStmt::Let(let_stmt) => {
                materialize_inferred_local_type(&mut let_stmt.pat);
                materialize_pattern(&mut let_stmt.pat);
                if let Some(init) = &mut let_stmt.init {
                    materialize_expr_types(init);
                }
                if let Some(diverge) = &mut let_stmt.diverge {
                    materialize_expr_types(diverge);
                }
            }
            BlockStmt::Item(item) => materialize_kotlin_types(item),
            BlockStmt::Expr(stmt) => materialize_expr_types(&mut stmt.expr),
            BlockStmt::Defer(stmt) => materialize_expr_types(&mut stmt.expr),
            BlockStmt::Noop => {}
        }
    }
}

/// The Rust lifter sometimes attaches a sentinel type to a local when a
/// standard-library generic cannot be recovered. It is only a type-checking
/// placeholder, not a Kotlin declaration type; retain the initializer and let
/// Kotlin infer its concrete runtime type instead of emitting `NonZero` or
/// `ErrorKind` into generated source.
#[cfg(test)]
fn materialize_inferred_local_type(pattern: &mut fp_core::ast::Pattern) {
    let fp_core::ast::PatternKind::Type(typed) = pattern.kind() else {
        return;
    };
    if !requires_kotlin_inference(&typed.ty) {
        return;
    }
    let replacement = (*typed.pat).clone();
    *pattern = replacement;
}

#[cfg(test)]
fn requires_kotlin_inference(ty: &Ty) -> bool {
    match ty {
        Ty::Any(_) | Ty::InferVar(_) | Ty::Wildcard(_) | Ty::Unknown(_) => true,
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Name(name) => matches!(kotlin_type_name(name), Some("NonZero" | "ErrorKind")),
            _ => false,
        },
        _ => false,
    }
}

#[cfg(test)]
fn materialize_pattern(pattern: &mut fp_core::ast::Pattern) {
    use fp_core::ast::PatternKind;

    match pattern.kind_mut() {
        PatternKind::Ident(_) | PatternKind::Wildcard(_) => {}
        PatternKind::Bind(bind) => materialize_pattern(&mut bind.pattern),
        PatternKind::Tuple(tuple) => {
            for pattern in &mut tuple.patterns {
                materialize_pattern(pattern);
            }
        }
        PatternKind::TupleStruct(tuple) => {
            for pattern in &mut tuple.patterns {
                materialize_pattern(pattern);
            }
        }
        PatternKind::Struct(pattern) => {
            for field in &mut pattern.fields {
                if let Some(pattern) = &mut field.rename {
                    materialize_pattern(pattern);
                }
            }
        }
        PatternKind::Structural(pattern) => {
            for field in &mut pattern.fields {
                if let Some(pattern) = &mut field.rename {
                    materialize_pattern(pattern);
                }
            }
        }
        PatternKind::Box(pattern) => materialize_pattern(&mut pattern.pattern),
        PatternKind::Ref(pattern) => materialize_pattern(&mut pattern.pattern),
        PatternKind::Variant(pattern) => {
            if let Some(pattern) = &mut pattern.pattern {
                materialize_pattern(pattern);
            }
        }
        PatternKind::Quote(pattern) => {
            for field in &mut pattern.fields {
                if let Some(pattern) = &mut field.rename {
                    materialize_pattern(pattern);
                }
            }
        }
        PatternKind::QuotePlural(pattern) => {
            for pattern in &mut pattern.patterns {
                materialize_pattern(pattern);
            }
        }
        PatternKind::Or(pattern) => {
            for pattern in &mut pattern.patterns {
                materialize_pattern(pattern);
            }
        }
        PatternKind::Type(pattern) => {
            materialize_pattern(&mut pattern.pat);
            materialize_kotlin_ty(&mut pattern.ty);
        }
    }
}

#[cfg(test)]
fn materialize_expr_types(expr: &mut Expr) {
    match expr.kind_mut() {
        ExprKind::Block(block) => {
            materialize_block(&mut block.stmts);
            for item in &mut block.collected_items {
                materialize_kotlin_types(item);
            }
        }
        ExprKind::If(expr_if) => {
            materialize_expr_types(&mut expr_if.cond);
            materialize_expr_types(&mut expr_if.then);
            if let Some(elze) = &mut expr_if.elze {
                materialize_expr_types(elze);
            }
        }
        ExprKind::Loop(expr_loop) => materialize_expr_types(&mut expr_loop.body),
        ExprKind::While(expr_while) => {
            materialize_expr_types(&mut expr_while.cond);
            materialize_expr_types(&mut expr_while.body);
        }
        ExprKind::With(expr_with) => {
            materialize_expr_types(&mut expr_with.context);
            materialize_expr_types(&mut expr_with.body);
        }
        ExprKind::Return(expr_return) => {
            if let Some(value) = &mut expr_return.value {
                materialize_expr_types(value);
            }
        }
        ExprKind::Break(expr_break) => {
            if let Some(value) = &mut expr_break.value {
                materialize_expr_types(value);
            }
        }
        ExprKind::ConstBlock(block) => {
            materialize_expr_types(&mut block.expr);
            for item in &mut block.collected_items {
                materialize_kotlin_types(item);
            }
        }
        ExprKind::Match(expr_match) => {
            if let Some(scrutinee) = &mut expr_match.scrutinee {
                materialize_expr_types(scrutinee);
            }
            for case in &mut expr_match.cases {
                if let Some(pattern) = &mut case.pat {
                    materialize_pattern(pattern);
                }
                materialize_expr_types(&mut case.cond);
                if let Some(guard) = &mut case.guard {
                    materialize_expr_types(guard);
                }
                materialize_expr_types(&mut case.body);
            }
        }
        ExprKind::Let(expr_let) => {
            materialize_pattern(&mut expr_let.pat);
            materialize_expr_types(&mut expr_let.expr);
        }
        ExprKind::Assign(assign) => {
            materialize_expr_types(&mut assign.target);
            materialize_expr_types(&mut assign.value);
        }
        ExprKind::Cast(cast) => {
            materialize_expr_types(&mut cast.expr);
            materialize_kotlin_ty(&mut cast.ty);
        }
        ExprKind::Invoke(invoke) => {
            match &mut invoke.target {
                fp_core::ast::ExprInvokeTarget::Expr(expr) => materialize_expr_types(expr),
                fp_core::ast::ExprInvokeTarget::Method(select) => {
                    materialize_expr_types(&mut select.obj)
                }
                fp_core::ast::ExprInvokeTarget::Closure(closure) => {
                    materialize_expr_types(&mut closure.body)
                }
                fp_core::ast::ExprInvokeTarget::Function(_)
                | fp_core::ast::ExprInvokeTarget::Type(_)
                | fp_core::ast::ExprInvokeTarget::BinOp(_) => {}
            }
            for arg in &mut invoke.args {
                materialize_expr_types(arg);
            }
            for kwarg in &mut invoke.kwargs {
                materialize_expr_types(&mut kwarg.value);
            }
            materialize_io_error_constructor(invoke);
        }
        ExprKind::Await(await_expr) => materialize_expr_types(&mut await_expr.base),
        ExprKind::Select(select) => materialize_expr_types(&mut select.obj),
        ExprKind::Index(index) => {
            materialize_expr_types(&mut index.obj);
            materialize_expr_types(&mut index.index);
        }
        ExprKind::Struct(struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = &mut field.value {
                    materialize_expr_types(value);
                }
            }
        }
        ExprKind::Structural(struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = &mut field.value {
                    materialize_expr_types(value);
                }
            }
        }
        ExprKind::Array(array) => {
            for value in &mut array.values {
                materialize_expr_types(value);
            }
        }
        ExprKind::ArrayRepeat(array) => {
            materialize_expr_types(&mut array.elem);
            materialize_expr_types(&mut array.len);
        }
        ExprKind::Tuple(tuple) => {
            for value in &mut tuple.values {
                materialize_expr_types(value);
            }
        }
        ExprKind::BinOp(binop) => {
            materialize_expr_types(&mut binop.lhs);
            materialize_expr_types(&mut binop.rhs);
        }
        ExprKind::UnOp(unop) => materialize_expr_types(&mut unop.val),
        ExprKind::Reference(reference) => materialize_expr_types(&mut reference.referee),
        ExprKind::Dereference(deref) => materialize_expr_types(&mut deref.referee),
        ExprKind::Splat(splat) => materialize_expr_types(&mut splat.iter),
        ExprKind::SplatDict(splat) => materialize_expr_types(&mut splat.dict),
        ExprKind::Try(expr_try) => {
            materialize_expr_types(&mut expr_try.expr);
            for catch in &mut expr_try.catches {
                if let Some(pattern) = &mut catch.pat {
                    materialize_pattern(pattern);
                }
                materialize_expr_types(&mut catch.body);
            }
            if let Some(elze) = &mut expr_try.elze {
                materialize_expr_types(elze);
            }
            if let Some(finally) = &mut expr_try.finally {
                materialize_expr_types(finally);
            }
        }
        ExprKind::Async(async_expr) => materialize_expr_types(&mut async_expr.expr),
        ExprKind::Closure(closure) => {
            for pattern in &mut closure.params {
                materialize_pattern(pattern);
            }
            if let Some(ret_ty) = &mut closure.ret_ty {
                materialize_kotlin_ty(ret_ty);
            }
            materialize_expr_types(&mut closure.body);
        }
        ExprKind::Closured(closured) => materialize_expr_types(&mut closured.expr),
        ExprKind::Paren(paren) => materialize_expr_types(&mut paren.expr),
        ExprKind::For(expr_for) => {
            materialize_pattern(&mut expr_for.pat);
            materialize_expr_types(&mut expr_for.iter);
            materialize_expr_types(&mut expr_for.body);
        }
        ExprKind::Item(item) => materialize_kotlin_types(item),
        ExprKind::IntrinsicCall(call) => {
            for arg in &mut call.args {
                materialize_expr_types(arg);
            }
            for kwarg in &mut call.kwargs {
                materialize_expr_types(&mut kwarg.value);
            }
        }
        ExprKind::IntrinsicContainer(container) => {
            container.for_each_expr_mut(materialize_expr_types)
        }
        ExprKind::Range(range) => {
            if let Some(start) = &mut range.start {
                materialize_expr_types(start);
            }
            if let Some(end) = &mut range.end {
                materialize_expr_types(end);
            }
            if let Some(step) = &mut range.step {
                materialize_expr_types(step);
            }
        }
        ExprKind::Quote(quote) => {
            materialize_block(&mut quote.block.stmts);
            for item in &mut quote.block.collected_items {
                materialize_kotlin_types(item);
            }
        }
        ExprKind::Splice(splice) => materialize_expr_types(&mut splice.token),
        ExprKind::SplicePending(pending) => materialize_expr_types(&mut pending.token),
        ExprKind::Id(_)
        | ExprKind::Name(_)
        | ExprKind::Value(_)
        | ExprKind::Continue(_)
        | ExprKind::FormatString(_)
        | ExprKind::Macro(_) => {}
    }
}

/// Rust's typed `Io` error variants carry `std::io::Error`, while Kotlin
/// filesystem calls expose `Throwable`. Convert only the constructor payload
/// at the backend boundary so generated sealed variants retain their declared
/// `java.io.IOException` field type.
#[cfg(test)]
fn materialize_io_error_constructor(invoke: &mut fp_core::ast::ExprInvoke) {
    let fp_core::ast::ExprInvokeTarget::Function(Name::Path(path)) = &invoke.target else {
        return;
    };
    if path.last().as_str() != "Io" || invoke.args.len() != 1 {
        return;
    }

    let error = invoke.args.pop().expect("one Io constructor argument");
    invoke
        .args
        .push(kotlin_runtime_call("ioError", vec![error]));
}

#[cfg(test)]
fn kotlin_runtime_call(name: &str, args: Vec<fp_core::ast::Expr>) -> fp_core::ast::Expr {
    fp_core::ast::Expr::new(ExprKind::Invoke(fp_core::ast::ExprInvoke {
        span: Default::default(),
        target: fp_core::ast::ExprInvokeTarget::Function(Name::path(Path::plain(vec![
            Ident::new("RustKotlinRuntime"),
            Ident::new(name),
        ]))),
        args,
        kwargs: Vec::new(),
    }))
}

#[cfg(test)]
fn materialize_kotlin_ty(ty: &mut Ty) {
    if let Ok(MaterializeOutcome::Replaced(materialized)) =
        KotlinMaterializer.materialize_type_mapping(ty)
    {
        *ty = materialized;
    }
    match ty {
        Ty::Expr(expr) => {
            if let ExprKind::Name(name) = expr.kind_mut() {
                materialize_kotlin_type_arguments(name);
                if let Some(kotlin_ty) = materialize_rust_type_alias(name) {
                    *ty = kotlin_ty;
                    return;
                }
                materialize_process_type(name);
                materialize_external_type(name);
                materialize_jvm_path_type(name);
            }
        }
        Ty::Reference(reference) => materialize_kotlin_ty(&mut reference.ty),
        Ty::RawPtr(pointer) => materialize_kotlin_ty(&mut pointer.ty),
        Ty::Slice(slice) => {
            materialize_kotlin_ty(&mut slice.elem);
            if is_u8_type(&slice.elem) {
                *ty = Ty::Expr(Box::new(Expr::name(Name::ident("ByteArray"))));
            }
        }
        Ty::Vec(vector) => {
            materialize_kotlin_ty(&mut vector.ty);
            let kotlin_ty = kotlin_vector_ty(&vector.ty);
            *ty = kotlin_ty;
        }
        Ty::Array(array) => {
            materialize_kotlin_ty(&mut array.elem);
            if is_u8_type(&array.elem) {
                *ty = Ty::Expr(Box::new(Expr::name(Name::ident("ByteArray"))));
            }
        }
        Ty::Tuple(tuple) => {
            for ty in &mut tuple.types {
                materialize_kotlin_ty(ty);
            }
        }
        Ty::Struct(structure) => materialize_struct_fields(&mut structure.fields),
        Ty::Structural(structure) => materialize_struct_fields(&mut structure.fields),
        Ty::Enum(en) => {
            for variant in &mut en.variants {
                materialize_kotlin_ty(&mut variant.value);
            }
        }
        Ty::Function(function) => {
            for ty in &mut function.params {
                materialize_kotlin_ty(ty);
            }
            if let Some(ty) = &mut function.ret_ty {
                materialize_kotlin_ty(ty);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
fn materialize_kotlin_type_arguments(name: &mut Name) {
    let Name::ParameterPath(path) = name else {
        return;
    };
    for segment in &mut path.segments {
        for arg in &mut segment.args {
            materialize_kotlin_ty(arg);
        }
    }
}

/// Rewrites Rust-only type names before the Kotlin serializer sees them.
///
/// The serializer deliberately renders the target-shaped AST verbatim. Keep
/// Rust aliases and generic conventions out of it so annotations generated by
/// desugared standard-library calls remain valid Kotlin/JVM declarations.
#[cfg(test)]
fn materialize_rust_type_alias(name: &Name) -> Option<Ty> {
    let (last, args) = match name {
        Name::Ident(ident) => (ident.as_str(), Vec::new()),
        Name::Path(path) => (path.last().as_str(), Vec::new()),
        Name::ParameterPath(path) => {
            let segment = path.last()?;
            (segment.ident.as_str(), segment.args.clone())
        }
    };

    match last {
        "str" => Some(Ty::ident(Ident::new("String"))),
        "bool" => Some(Ty::Primitive(fp_core::ast::TypePrimitive::Bool)),
        "i8" => Some(Ty::Primitive(fp_core::ast::TypePrimitive::Int(
            fp_core::ast::TypeInt::I8,
        ))),
        "u8" => Some(Ty::Primitive(fp_core::ast::TypePrimitive::Int(
            fp_core::ast::TypeInt::U8,
        ))),
        "i16" => Some(Ty::Primitive(fp_core::ast::TypePrimitive::Int(
            fp_core::ast::TypeInt::I16,
        ))),
        "u16" => Some(Ty::Primitive(fp_core::ast::TypePrimitive::Int(
            fp_core::ast::TypeInt::U16,
        ))),
        "i32" => Some(Ty::Primitive(fp_core::ast::TypePrimitive::Int(
            fp_core::ast::TypeInt::I32,
        ))),
        "u32" => Some(Ty::Primitive(fp_core::ast::TypePrimitive::Int(
            fp_core::ast::TypeInt::U32,
        ))),
        "i64" | "isize" => Some(Ty::Primitive(fp_core::ast::TypePrimitive::Int(
            fp_core::ast::TypeInt::I64,
        ))),
        "u64" | "usize" => Some(Ty::Primitive(fp_core::ast::TypePrimitive::Int(
            fp_core::ast::TypeInt::U64,
        ))),
        "i128" => Some(Ty::Primitive(fp_core::ast::TypePrimitive::Int(
            fp_core::ast::TypeInt::I128,
        ))),
        "u128" => Some(Ty::Primitive(fp_core::ast::TypePrimitive::Int(
            fp_core::ast::TypeInt::U128,
        ))),
        "f16" | "f32" => Some(Ty::Primitive(fp_core::ast::TypePrimitive::Decimal(
            fp_core::ast::DecimalType::F32,
        ))),
        "f64" | "f128" => Some(Ty::Primitive(fp_core::ast::TypePrimitive::Decimal(
            fp_core::ast::DecimalType::F64,
        ))),
        // The Kotlin emitter does not declare Rust generic parameters. A
        // source-level `T` that survives into a type annotation must become a
        // concrete Kotlin type rather than an unresolved identifier.
        "T" => Some(Ty::ANY),
        "Result" => Some(kotlin_parameterized_ty(
            "Result",
            args.into_iter().next().unwrap_or(Ty::ANY),
        )),
        "Option" => Some(kotlin_parameterized_ty(
            "Option",
            args.into_iter().next().unwrap_or(Ty::ANY),
        )),
        "Error" if is_std_io_error(name) => Some(Ty::path(Path::plain(vec![
            Ident::new("java"),
            Ident::new("io"),
            Ident::new("IOException"),
        ]))),
        "Error" => Some(Ty::path(Path::plain(vec![Ident::new("Throwable")]))),
        // `to_vec_in` is the allocator-aware slice-clone implementation name
        // that can survive Rust desugaring in an inferred annotation.
        "Vec" | "to_vec" | "to_vec_in" | "slice_to_vec" | "slice_to_vec_in" => Some(
            kotlin_vector_ty(&args.into_iter().next().unwrap_or(Ty::ANY)),
        ),
        _ => None,
    }
}

#[cfg(test)]
fn is_std_io_error(name: &Name) -> bool {
    let segments: Vec<&str> = match name {
        Name::Path(path) => path.segments.iter().map(Ident::as_str).collect(),
        Name::ParameterPath(path) => path
            .segments
            .iter()
            .map(|segment| segment.ident.as_str())
            .collect(),
        Name::Ident(_) => return false,
    };
    segments.len() >= 3
        && segments[segments.len() - 3] == "std"
        && segments[segments.len() - 2] == "io"
        && segments[segments.len() - 1] == "Error"
}

#[cfg(test)]
fn kotlin_type_name(name: &Name) -> Option<&str> {
    match name {
        Name::Ident(ident) => Some(ident.as_str()),
        Name::Path(path) => Some(path.last().as_str()),
        Name::ParameterPath(path) => path.last().map(|segment| segment.ident.as_str()),
    }
}

#[cfg(test)]
fn kotlin_parameterized_ty(name: &str, arg: Ty) -> Ty {
    Ty::Expr(Box::new(Expr::name(Name::parameter_path(
        fp_core::ast::ParameterPath::new(
            fp_core::ast::path::PathPrefix::Plain,
            vec![fp_core::ast::ParameterPathSegment::new(
                Ident::new(name),
                vec![arg],
            )],
        ),
    ))))
}

/// Rust vectors reach the target AST in two equivalent forms: the structural
/// `Ty::Vec` form and a resolved nominal `alloc::vec::Vec<T>` path. Normalize
/// both forms here so the serializer only ever receives Kotlin types.
#[cfg(test)]
fn kotlin_vector_ty(element_ty: &Ty) -> Ty {
    if is_u8_type(element_ty) {
        return Ty::Expr(Box::new(Expr::name(Name::ident("ByteArray"))));
    }
    Ty::Expr(Box::new(Expr::name(Name::parameter_path(
        fp_core::ast::ParameterPath::new(
            fp_core::ast::path::PathPrefix::Plain,
            vec![fp_core::ast::ParameterPathSegment::new(
                Ident::new("MutableList"),
                vec![element_ty.clone()],
            )],
        ),
    ))))
}

#[cfg(test)]
fn is_u8_type(ty: &Ty) -> bool {
    matches!(
        ty,
        Ty::Primitive(fp_core::ast::TypePrimitive::Int(fp_core::ast::TypeInt::U8))
    )
}

#[cfg(test)]
fn materialize_process_type(name: &mut Name) {
    let last = match name {
        Name::Ident(ident) => ident.as_str(),
        Name::Path(path) => path.last().as_str(),
        Name::ParameterPath(path) => match path.last() {
            Some(segment) => segment.ident.as_str(),
            None => return,
        },
    };
    let runtime_type = match last {
        "Command" => "Command",
        "Child" => "Child",
        "Output" => "Output",
        "DirEntry" => "DirEntry",
        "FileType" => "FileType",
        "ExitStatus" => "ExitStatus",
        "Stdio" => "Stdio",
        _ => return,
    };
    *name = Name::path(Path::plain(vec![
        Ident::new("RustKotlinRuntime"),
        Ident::new(runtime_type),
    ]));
}

/// External APIs are represented by typed Rust declarations. Retain their
/// target-native type identity before syntax serialization; calls themselves
/// continue to lower through their registered intrinsic identities.
#[cfg(test)]
fn materialize_external_type(name: &mut Name) {
    let is_json_value = match name {
        Name::ParameterPath(path) => {
            let mut segments = path.segments.iter();
            matches!(
                (segments.next(), segments.next(), segments.next()),
                (Some(package), Some(value), None)
                    if package.ident.as_str() == "serde_json" && value.ident.as_str() == "Value"
            )
        }
        Name::Path(path) => {
            let segments = &path.segments;
            segments.len() == 2 && segments[0].name == "serde_json" && segments[1].name == "Value"
        }
        Name::Ident(_) => false,
    };
    if is_json_value {
        *name = Name::path(Path::plain(vec![
            Ident::new("com"),
            Ident::new("fasterxml"),
            Ident::new("jackson"),
            Ident::new("databind"),
            Ident::new("JsonNode"),
        ]));
    }
}

/// Rust paths and OS strings are represented by JVM path/string values.  This
/// is type materialization, so calls continue to lower exclusively through
/// their portable operation identities.
#[cfg(test)]
fn materialize_jvm_path_type(name: &mut Name) {
    let last = match name {
        Name::Ident(ident) => ident.as_str(),
        Name::Path(path) => path.last().as_str(),
        Name::ParameterPath(path) => match path.last() {
            Some(segment) => segment.ident.as_str(),
            None => return,
        },
    };
    let target = match last {
        "Path" | "PathBuf" => ["java", "nio", "file", "Path"].as_slice(),
        "OsStr" | "OsString" => ["String"].as_slice(),
        _ => return,
    };
    *name = Name::path(Path::plain(
        target.iter().map(|part| Ident::new(*part)).collect(),
    ));
}
