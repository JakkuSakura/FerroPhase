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
use crate::ast::package::PackageId;
use crate::ast::program::AstProgram;

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
    /// Whether the backend should produce a fully-linked, runnable
    /// artifact rather than a relocatable object — `true` for every
    /// ordinary source compile (which always wants an executable
    /// regardless of `--link`); only a container-input compile (e.g. a
    /// native object file given directly as `fp compile`'s input) can
    /// legitimately set this `false` to just retarget the object without
    /// linking it.
    pub link_requested: bool,
    /// Whether the backend should prefer emitting human-readable target
    /// assembly text over an object/executable — only meaningful for a
    /// container-input compile that's already assembly text itself (`fp
    /// compile foo.s`) and wasn't asked to `--link`/`--exec`; `false` for
    /// every ordinary source compile.
    pub emit_text: bool,
    /// Whether `--exec` was requested — only meaningful to a backend whose
    /// default output shape differs between "write an artifact" and "write
    /// something immediately runnable" (e.g. eBPF's `.o` vs `.ebpf`);
    /// `false` for every backend that doesn't care.
    pub exec_requested: bool,
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
            link_requested: true,
            emit_text: false,
            exec_requested: false,
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

    pub fn with_link_requested(mut self, link_requested: bool) -> Self {
        self.link_requested = link_requested;
        self
    }

    pub fn with_emit_text(mut self, emit_text: bool) -> Self {
        self.emit_text = emit_text;
        self
    }

    pub fn with_exec_requested(mut self, exec_requested: bool) -> Self {
        self.exec_requested = exec_requested;
        self
    }
}

/// A target's interface for turning a compiled package into on-disk
/// output. Different backends read different views of the package from
/// `AstProgram` (AST, MIR, LIR, ...) — passing the workspace itself,
/// rather than a fixed view type, lets every backend share this one
/// non-generic trait as `Box<dyn TargetBackend>`.
pub trait TargetBackend: Send + Sync {
    /// What this backend's target language can express directly — see
    /// `crate::capabilities::LanguageCapabilities`. Read once by `fp-cli`
    /// (via the already-constructed backend, before compiling) to seed
    /// `CompilerState`'s backend capabilities so HIR lowering
    /// (`HirLoweringConfig.capabilities`) can branch on them. No default:
    /// every backend states its own capabilities explicitly (most return
    /// `LanguageCapabilities::NATIVE` — the conservative "nothing first-
    /// class" baseline — a handful, like Kotlin, return more).
    fn capabilities(&self) -> crate::capabilities::LanguageCapabilities;

    /// Writes `package_id`'s artifact to the path fixed at construction,
    /// reading whichever view of it the backend needs from `workspace`
    /// (`package_source`, `merged_lir_program`, ...).
    fn emit_package_artifact(&self, workspace: &AstProgram, package_id: &PackageId) -> Result<()>;

    /// Workspace-level side files not tied to a single package (e.g.
    /// Kotlin's `settings.gradle.kts`/`build.gradle.kts`). Default: no-op.
    fn write_workspace_files(&self, workspace: &AstProgram) -> Result<()> {
        let _ = workspace;
        Ok(())
    }

    /// Runs whatever `emit_package_artifact`/`write_workspace_files` just
    /// produced (`--exec`). Each backend knows its own output shape, so
    /// this stays colocated with it. Default: unsupported.
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
