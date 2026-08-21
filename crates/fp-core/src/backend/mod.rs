//! `TargetBackend`: the trait every target-emitting backend crate (Kotlin,
//! TypeScript, ..., native/goasm/urcl) implements so `fp-cli` can dispatch
//! through one interface instead of hand-calling each backend's own
//! inherent methods and hand-rolling file writes itself. See the design doc
//! this module was introduced for: the backend owns *where* it writes
//! (resolved once at construction time via [`BackendConfig`]) and *writes
//! its own files* — `fp-cli` never threads an output path through a trait
//! method call, and never sees the generated content to write itself.

use std::path::PathBuf;

use crate::error::Result;
use crate::package::PackageId;
use crate::workspace::WorkspaceContext;

/// Resolved once by `fp-cli` from `CompileArgs`, then handed to each
/// backend's constructor — never threaded through trait methods afterward.
/// What `workspace_root` means is backend-specific: for AST-emitting
/// backends it's the workspace-rooted output directory (per-package
/// subdirectories are derived from it); for single-artifact backends
/// (native/goasm/urcl) it's the artifact's own output path.
///
/// The remaining fields are every construction-time codegen/emission
/// option a built-in backend reads — deliberately not `CompileArgs`
/// itself (fp-cli's whole CLI-flag surface, including things like `exec`/
/// `link`/`opt_level` that have nothing to do with constructing a
/// backend): this is the one value a backend constructor should ever
/// need, so fp-cli's own dispatch (`backend_for_target`) never has to
/// reach past it into `CompileArgs`.
pub struct BackendConfig {
    pub workspace_root: PathBuf,
    pub target_triple: Option<String>,
    pub target_cpu: Option<String>,
    pub native_target: Option<String>,
    pub target_features: Option<String>,
    pub target_sysroot: Option<PathBuf>,
    pub linker: String,
    pub target_linker: Option<PathBuf>,
    pub release: bool,
    pub debug_info: bool,
    pub save_intermediates: bool,
    pub type_defs: bool,
    pub single_world: bool,
    /// The workspace's own project name (Kotlin's `settings.gradle.kts`
    /// `rootProject.name`) — derived by the caller from the *source*
    /// project directory's name, not `workspace_root` (the output
    /// directory).
    pub root_name: String,
}

impl BackendConfig {
    pub fn new(workspace_root: PathBuf) -> Self {
        Self {
            workspace_root,
            target_triple: None,
            target_cpu: None,
            native_target: None,
            target_features: None,
            target_sysroot: None,
            linker: "clang".to_string(),
            target_linker: None,
            release: false,
            debug_info: false,
            save_intermediates: false,
            type_defs: false,
            single_world: false,
            root_name: "workspace".to_string(),
        }
    }

    pub fn with_target_triple(mut self, target_triple: Option<String>) -> Self {
        self.target_triple = target_triple;
        self
    }

    pub fn with_target_cpu(mut self, target_cpu: Option<String>) -> Self {
        self.target_cpu = target_cpu;
        self
    }

    pub fn with_native_target(mut self, native_target: Option<String>) -> Self {
        self.native_target = native_target;
        self
    }

    pub fn with_target_features(mut self, target_features: Option<String>) -> Self {
        self.target_features = target_features;
        self
    }

    pub fn with_target_sysroot(mut self, target_sysroot: Option<PathBuf>) -> Self {
        self.target_sysroot = target_sysroot;
        self
    }

    pub fn with_linker(mut self, linker: String) -> Self {
        self.linker = linker;
        self
    }

    pub fn with_target_linker(mut self, target_linker: Option<PathBuf>) -> Self {
        self.target_linker = target_linker;
        self
    }

    pub fn with_release(mut self, release: bool) -> Self {
        self.release = release;
        self
    }

    pub fn with_debug_info(mut self, debug_info: bool) -> Self {
        self.debug_info = debug_info;
        self
    }

    pub fn with_save_intermediates(mut self, save_intermediates: bool) -> Self {
        self.save_intermediates = save_intermediates;
        self
    }

    pub fn with_type_defs(mut self, type_defs: bool) -> Self {
        self.type_defs = type_defs;
        self
    }

    pub fn with_single_world(mut self, single_world: bool) -> Self {
        self.single_world = single_world;
        self
    }

    pub fn with_root_name(mut self, root_name: String) -> Self {
        self.root_name = root_name;
        self
    }
}

/// One backend's interface for turning a compiled unit into on-disk output.
///
/// The two families of backend this trait covers operate at different IR
/// levels by convention (AST-emitting backends read a
/// `fp_core::package::PackageSource`; the native/goasm/urcl codegen
/// backends read a merged `fp_core::lir::LirProgram`) — both are just
/// different views a `WorkspaceContext` can produce for a given
/// `PackageId`, so passing the workspace itself (rather than fixing a
/// per-family associated `Package` type) lets every backend share one
/// non-generic trait `fp-cli` can dispatch through as `Box<dyn TargetBackend>`.
pub trait TargetBackend: Send + Sync {
    /// Emit `package_id`'s output. The backend already knows where to
    /// write (captured from `BackendConfig` at construction) and does so
    /// itself; it derives whichever view of `package_id` it needs
    /// (`workspace.package_source(..)` or `workspace.merged_lir_program(..)`)
    /// from `workspace`.
    fn compile_package(&self, workspace: &WorkspaceContext, package_id: &PackageId) -> Result<()>;

    /// Workspace-level side files not tied to a single package (e.g.
    /// Kotlin's `settings.gradle.kts`/`build.gradle.kts`). Default: no-op.
    fn write_workspace_files(&self, workspace: &WorkspaceContext) -> Result<()> {
        let _ = workspace;
        Ok(())
    }

    /// Runs whatever `compile_package`/`write_workspace_files` just
    /// produced (`--exec`) — a backend that writes a native executable
    /// spawns it, one that writes bytecode drives its own VM, and so on.
    /// Each backend already knows its own output shape, so this stays
    /// colocated with the backend rather than fp-cli sniffing the output
    /// path's extension/header to guess how to run it. Default: unsupported.
    fn exec(&self) -> Result<()> {
        Err(crate::error::Error::from(
            "--exec is not supported for this target".to_string(),
        ))
    }
}

/// Plain helper — not part of the trait. A backend constructs one
/// internally (typically `PackageWriter::new(some_dir_derived_from_its_own_config)`)
/// when it needs to write several named text files; single-artifact
/// backends (native/goasm/urcl) don't need it at all.
pub struct PackageWriter {
    root: PathBuf,
}

impl PackageWriter {
    pub fn new(root: PathBuf) -> Self {
        Self { root }
    }

    /// Joins `relative_path` under `root` (creating parent dirs) and writes
    /// `content`. Generic over bytes so both text and binary output go
    /// through the same call.
    pub fn write_file(&self, relative_path: &str, content: impl AsRef<[u8]>) -> Result<PathBuf> {
        let path = self.root.join(relative_path);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&path, content.as_ref())?;
        Ok(path)
    }
}
