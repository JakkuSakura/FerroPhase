pub mod abi;
pub mod archive;
pub mod asm;
pub mod asmir;
pub mod binary;
pub mod config;
pub mod container;
pub mod emit;
pub mod ffi;
pub mod intrinsic_materializer;
pub mod jit;
pub mod libc;
pub mod link;
pub mod package;
pub mod system_api;

use crate::config::{EmitKind, NativeConfig};
use crate::emit::{detect_target, resolve_native_target};
use fp_core::asmir::AsmProgram;
use fp_core::container::ContainerReader as _;
use fp_core::error::{Error, Result};
use fp_core::lir::LirBlob;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::process::Command;

pub use crate::intrinsic_materializer::NativeIntrinsicMaterializer;
pub use crate::jit::{
    HostScalar, JitEngine, JitModule, validate_host_program, validate_native_program,
};

/// Native (LLVM-free) compiler entry point.
///
/// Current scope: minimal native backend that can emit a tiny binary stub for
/// Mach-O/ELF/PE targets, then link it into an executable in-process.
///
/// This is intended as an incremental replacement for `fp-llvm`.
pub struct NativeEmitter {
    config: NativeConfig,
}

impl NativeEmitter {
    pub fn new(config: NativeConfig) -> Self {
        Self { config }
    }

    /// Emit LIR into an object or executable.
    pub fn emit(&self, lir_program: LirBlob, source_file: Option<&Path>) -> Result<PathBuf> {
        let _ = source_file;

        // Ensure output directory exists.
        if let Some(parent) = self.config.output_path.parent() {
            std::fs::create_dir_all(parent).map_err(fp_core::error::Error::from)?;
        }

        self.emit_impl(&lir_program)
    }

    /// Back-compat for older callers.
    pub fn compile(&self, lir_program: LirBlob, source_file: Option<&Path>) -> Result<PathBuf> {
        self.emit(lir_program, source_file)
    }
}

/// `NativeEmitter` already carries its own fully-resolved output path via
/// `NativeConfig` (constructed by the caller with the exact artifact path
/// before the emitter itself), so unlike the AST-emitting backends this
/// doesn't need a separate `BackendConfig` — the existing config already is
/// the "where to write" state `TargetBackend`'s design calls for.
impl fp_core::backend::TargetBackend for NativeEmitter {
    fn capabilities(&self) -> fp_core::capabilities::LanguageCapabilities {
        fp_core::capabilities::LanguageCapabilities::NATIVE
    }

    fn emit_package_artifact(
        &self,
        workspace: &fp_core::ast::program::AstProgram,
        package_id: &fp_core::ast::package::PackageId,
        mir: &fp_core::mir::MirCodeUnit,
        lir: Option<&fp_core::lir::LirBlob>,
    ) -> Result<()> {
        if let Ok(source) = workspace.package_source(package_id) {
            // A native archive (`NativeObjectPackageProvider::from_archive`)
            // tags each member with its own name as a non-empty
            // `QualifiedPath`. Every other precompiled provider (plain
            // object/asm, or a foreign artifact like CIL/JVM that also
            // carries a best-effort `PrecompiledLir` alongside its
            // `PrecompiledArtifact`) always uses an empty path on every
            // item — `items.len() > 1` alone isn't a safe signal, since
            // those two-item CIL/JVM packages aren't archives at all.
            let is_archive = source
                .items
                .iter()
                .any(|pkg_item| !pkg_item.module_path.is_empty());
            if is_archive {
                let members: Vec<(fp_core::ast::path::QualifiedPath, PrecompiledMember)> = source
                    .items
                    .iter()
                    .filter_map(|pkg_item| match pkg_item.item.kind() {
                        fp_core::ast::ItemKind::PrecompiledAsm(asm) => Some((
                            pkg_item.module_path.clone(),
                            PrecompiledMember::Asm(asm.clone()),
                        )),
                        fp_core::ast::ItemKind::PrecompiledArtifact(bytes) => Some((
                            pkg_item.module_path.clone(),
                            PrecompiledMember::Bytes(bytes.clone()),
                        )),
                        _ => None,
                    })
                    .collect();
                if !members.is_empty() {
                    self.emit_precompiled_archive(members)?;
                    return Ok(());
                }
            }
            let asm = source
                .items
                .iter()
                .find_map(|pkg_item| match pkg_item.item.kind() {
                    fp_core::ast::ItemKind::PrecompiledAsm(asm) => Some(asm.clone()),
                    _ => None,
                });
            if let Some(asm) = asm {
                self.emit_precompiled(asm)?;
                return Ok(());
            }
        }
        let _ = mir;
        let lir = lir
            .ok_or_else(|| {
                fp_core::error::Error::from(format!("package `{package_id}` has no compiled LIR"))
            })?
            .clone();
        self.emit(lir, None)?;
        Ok(())
    }

    fn exec(&self) -> Result<()> {
        let path = &self.config.output_path;
        let status = std::process::Command::new(path).status().map_err(|e| {
            fp_core::error::Error::from(format!("failed to execute '{}': {e}", path.display()))
        })?;
        if !status.success() {
            let code = status.code().unwrap_or(-1);
            return Err(fp_core::error::Error::from(format!(
                "process exited with status {code}"
            )));
        }
        Ok(())
    }
}

/// One `NativeObjectPackageProvider::from_archive` member: either a
/// recognized object lifted to `AsmProgram` (retargeted like any other
/// precompiled object) or opaque raw bytes (a non-object member, e.g. a
/// symbol table, repacked verbatim).
enum PrecompiledMember {
    Asm(AsmProgram),
    Bytes(Vec<u8>),
}

impl NativeEmitter {
    /// Retargets and repacks a native archive's members — always writes a
    /// plain retargeted archive regardless of `self.config.emit`
    /// (`--link`/`--exec` don't apply to an archive; there's no single
    /// entry point to link against), matching exactly what the bespoke
    /// `container/pipeline.rs` archive transpile used to do.
    fn emit_precompiled_archive(
        &self,
        members: Vec<(fp_core::ast::path::QualifiedPath, PrecompiledMember)>,
    ) -> Result<PathBuf> {
        let (format, arch) = detect_target(self.config.target_triple.as_deref())?;
        let mut out_members = Vec::with_capacity(members.len());
        for (path, payload) in members {
            let name = path.head().unwrap_or("member").to_string();
            let data = match payload {
                PrecompiledMember::Asm(asm) => {
                    let plan = emit::emit_plan_from_asmir(asm, format, arch)?;
                    emit::write_object_bytes(&plan)?
                }
                PrecompiledMember::Bytes(bytes) => bytes,
            };
            out_members.push(crate::archive::ArchiveMember { name, data });
        }
        let archive_bytes = crate::archive::write_gnu_archive(&out_members)?;
        let out = self.config.output_path.clone();
        if let Some(parent) = out.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&out, archive_bytes)?;
        Ok(out)
    }

    /// Emits an already-lifted object file's `AsmProgram` (see
    /// `crate::binary::lift_object_to_asmir`) — the object-transpile
    /// counterpart to `emit_impl`, which starts from a `LirBlob`
    /// instead. Retargets via `emit::emit_plan_from_asmir` rather than
    /// `emit::emit_plan` (no LIR lowering involved — there's no LIR here),
    /// then writes/links exactly the same way `emit_impl` does.
    fn emit_precompiled(&self, asmir: AsmProgram) -> Result<PathBuf> {
        let out = self.config.output_path.clone();
        if let Some(parent) = out.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let (format, arch) = detect_target(self.config.target_triple.as_deref())?;
        if self.config.emit == EmitKind::AssemblyText {
            let text = match arch {
                emit::TargetArch::X86_64 => crate::asmir::lower_to_x86_64(&asmir).to_text(),
                emit::TargetArch::Aarch64 => crate::asmir::lower_to_aarch64(&asmir).to_text(),
            };
            std::fs::write(&out, text)?;
            return Ok(out);
        }
        let plan = emit::emit_plan_from_asmir(asmir, format, arch)?;
        if let Some(path) = self.config.asm_dump.as_ref() {
            emit::dump_asm(path, &plan)?;
        }

        match self.config.emit {
            EmitKind::Executable => {
                let needs_external_link = format == emit::TargetFormat::MachO
                    && plan_has_undefined_symbols(&plan)
                    && !self
                        .config
                        .linker_driver
                        .as_deref()
                        .unwrap_or_default()
                        .is_empty();
                if needs_external_link {
                    self.link_with_clang(&out, &plan, format, arch)?;
                } else if let Err(err) = emit::write_executable(&out, &plan) {
                    if format == emit::TargetFormat::MachO {
                        self.link_with_clang(&out, &plan, format, arch)?;
                    } else {
                        return Err(Error::from(format!(
                            "Failed to write executable output: {err}"
                        )));
                    }
                }
            }
            EmitKind::Object => emit::write_object(&out, &plan)?,
            EmitKind::AssemblyText => {
                std::fs::write(&out, asm_program_to_text(&plan.asmir, arch))?;
            }
        }
        Ok(out)
    }

    /// Links `plan` into `output_path` via an external `cc`/`clang` driver
    /// (`self.config.linker_driver`, `--sysroot`/`-fuse-ld` from
    /// `self.config.sysroot`/`fuse_ld`) — the fallback for object-transpile
    /// inputs the in-process linker can't handle on its own (e.g. Mach-O
    /// with relocations against undefined symbols the OS's dynamic linker
    /// resolves at load time), and for a lifted Linux SysV `main` that
    /// needs a small native wrapper to run under Darwin's CRT entrypoint.
    fn link_with_clang(
        &self,
        output_path: &Path,
        plan: &emit::EmitPlan,
        format: emit::TargetFormat,
        arch: emit::TargetArch,
    ) -> Result<()> {
        const DARWIN_LINUX_MAIN_WRAPPER: &str = r#"
#include <stdint.h>

// Minimal wrapper for lifted Linux SysV entrypoints.
//
// The lifted `fp_lifted_main` may contain x86_64 stack-realignment prologues
// that read from the incoming stack pointer. Touch the stack in native code
// first to ensure the mapping is fault-free.

extern int fp_lifted_main(int argc, char **argv, char **envp);

int main(int argc, char **argv, char **envp) {
  volatile uint8_t probe[4096];
  probe[0] = 0;
  return fp_lifted_main(argc, argv, envp);
}
"#;

        let linker = self.config.linker_driver.as_deref().unwrap_or("clang");
        let tmp_dir = std::env::temp_dir().join(format!("fp-link-{}", std::process::id()));
        std::fs::create_dir_all(&tmp_dir)?;
        let object_path = tmp_dir.join("input.o");
        let wrapper_c_path = tmp_dir.join("wrapper.c");
        let wrapper_object_path = tmp_dir.join("wrapper.o");

        // Prefer reusing the emitted plan (it already includes relocations).
        emit::write_object(&object_path, plan).map_err(|err| {
            Error::from(format!(
                "Failed to write temporary object for linking: {err}"
            ))
        })?;

        if matches!(format, emit::TargetFormat::MachO)
            && matches!(arch, emit::TargetArch::Aarch64 | emit::TargetArch::X86_64)
        {
            let needs_main_wrapper =
                !plan.symbols.contains_key("main") && plan.symbols.contains_key("fp_lifted_main");
            if needs_main_wrapper {
                std::fs::write(&wrapper_c_path, DARWIN_LINUX_MAIN_WRAPPER)?;

                let mut cc = Command::new(linker);
                if let Some(sysroot) = &self.config.sysroot {
                    cc.arg(format!("--sysroot={}", sysroot.display()));
                }
                if let Some(ld) = &self.config.fuse_ld {
                    cc.arg(format!("-fuse-ld={}", ld.display()));
                }
                match arch {
                    emit::TargetArch::Aarch64 => cc.args(["-arch", "arm64"]),
                    emit::TargetArch::X86_64 => cc.args(["-arch", "x86_64"]),
                };
                cc.args(["-c", "-x", "c"]);
                cc.arg(&wrapper_c_path);
                cc.arg("-o").arg(&wrapper_object_path);

                let output = cc.output().map_err(|err| {
                    Error::from(format!("Failed to invoke compiler '{linker}': {err}"))
                })?;
                if !output.status.success() {
                    let stderr = String::from_utf8_lossy(&output.stderr);
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    return Err(Error::from(format!(
                        "Failed to compile Darwin main wrapper (status {:?}).\nstdout:\n{stdout}\nstderr:\n{stderr}",
                        output.status.code()
                    )));
                }
            }
        }

        let mut cmd = Command::new(linker);
        if let Some(sysroot) = &self.config.sysroot {
            cmd.arg(format!("--sysroot={}", sysroot.display()));
        }
        if let Some(ld) = &self.config.fuse_ld {
            cmd.arg(format!("-fuse-ld={}", ld.display()));
        }

        match (format, arch) {
            (emit::TargetFormat::MachO, emit::TargetArch::Aarch64) => {
                cmd.args(["-arch", "arm64"]);
                cmd.arg("-Wl,-undefined,dynamic_lookup");
                cmd.arg("-Wl,-no_dead_strip_inits_and_terms");
            }
            (emit::TargetFormat::MachO, emit::TargetArch::X86_64) => {
                cmd.args(["-arch", "x86_64"]);
                cmd.arg("-Wl,-undefined,dynamic_lookup");
                cmd.arg("-Wl,-no_dead_strip_inits_and_terms");
            }
            _ => {}
        }

        // Use the platform CRT entrypoint so constructors and runtime init run.
        // The transpiled object is expected to provide `_main`.
        cmd.arg("-o").arg(output_path);
        if wrapper_object_path.exists() {
            cmd.arg(&wrapper_object_path);
        }
        cmd.arg(&object_path);

        let output = cmd
            .output()
            .map_err(|err| Error::from(format!("Failed to invoke linker '{linker}': {err}")))?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);
            return Err(Error::from(format!(
                "External linker failed (status {:?}).\nstdout:\n{stdout}\nstderr:\n{stderr}",
                output.status.code()
            )));
        }

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            if let Ok(metadata) = std::fs::metadata(output_path) {
                let mut perms = metadata.permissions();
                perms.set_mode(0o755);
                let _ = std::fs::set_permissions(output_path, perms);
            }
        }

        // Best-effort cleanup.
        let _ = std::fs::remove_file(&object_path);
        let _ = std::fs::remove_file(&wrapper_object_path);
        let _ = std::fs::remove_file(&wrapper_c_path);
        let _ = std::fs::remove_dir(&tmp_dir);

        Ok(())
    }

    fn emit_impl(&self, lir_program: &LirBlob) -> Result<PathBuf> {
        let out = self.config.output_path.clone();
        resolve_native_target(
            self.config.native_target,
            self.config.target_triple.as_deref(),
        )?;

        let (format, arch) = detect_target(self.config.target_triple.as_deref())?;

        let plan = emit::emit_plan(lir_program, format, arch)?;
        if let Some(path) = self.config.asm_dump.as_ref() {
            emit::dump_asm(path, &plan)?;
        }

        match self.config.emit {
            EmitKind::Object => emit::write_object(&out, &plan)?,
            EmitKind::Executable => emit::write_executable(&out, &plan)?,
            EmitKind::AssemblyText => {
                std::fs::write(&out, asm_program_to_text(&plan.asmir, arch))?;
            }
        }
        Ok(out)
    }
}

/// Renders `asmir` (already selected/normalized for `arch` by
/// `emit::emit_plan`/`emit_plan_from_asmir`) as human-readable target
/// assembly text — `EmitKind::AssemblyText`'s implementation, shared by
/// `emit_impl` (from `LirBlob`) and `emit_precompiled` (from an
/// already-compiled `AsmProgram`, e.g. lifted from asm text or an object
/// file) since both end up with the same kind of `EmitPlan` either way.
fn asm_program_to_text(asmir: &AsmProgram, arch: emit::TargetArch) -> String {
    match arch {
        emit::TargetArch::X86_64 => crate::asmir::lower_to_x86_64(asmir).to_text(),
        emit::TargetArch::Aarch64 => crate::asmir::lower_to_aarch64(asmir).to_text(),
    }
}

/// True if any relocation in `plan` targets a symbol not defined anywhere
/// in the plan itself — used to decide whether the in-process linker
/// (which can't resolve external symbols) must fall back to an external
/// `cc`/`clang` driver instead (see `NativeEmitter::link_with_clang`).
fn plan_has_undefined_symbols(plan: &emit::EmitPlan) -> bool {
    let mut defined = HashSet::new();
    defined.extend(plan.symbols.keys().map(|name| name.as_str()));
    defined.extend(plan.rodata_symbols.keys().map(|name| name.as_str()));

    plan.relocs
        .iter()
        .any(|reloc| !defined.contains(reloc.symbol.as_str()))
}

/// `PackageProvider` for a pre-compiled object file given directly as
/// `fp compile`'s input (not FerroPhase source) — lifts it to
/// `AsmProgram` once at construction (there's nothing to parse lazily)
/// and embeds it directly as the package's one item
/// (`fp_core::ast::ItemKind::PrecompiledAsm`), so `NativeEmitter::
/// emit_package_artifact` picks it up from `workspace.package_source(id)` the
/// same way every AST-emitting backend already reads its package's
/// items — no side-channel field, no extra trait method.
pub struct NativeObjectPackageProvider {
    package_id: fp_core::ast::package::PackageId,
    descriptor: std::sync::Arc<fp_core::ast::package::PackageDescriptor>,
    source: fp_core::ast::package::AstPackage,
}

impl NativeObjectPackageProvider {
    pub fn new(package_id: fp_core::ast::package::PackageId, bytes: &[u8]) -> Result<Self> {
        let asm = crate::binary::lift_object_to_asmir(bytes)
            .map_err(|err| Error::from(format!("Failed to lift object file: {err}")))?;
        Ok(Self::from_asm(package_id, asm))
    }

    /// Same one-item package shape as `new`, but from an `AsmProgram`
    /// that's already been lifted — e.g. from asm *text* (`fp compile
    /// foo.s`, lifted via `asmir::lift_from_x86_64`/`lift_from_aarch64`
    /// after `asm::x86_64::AsmX86_64Program::parse_text`/`asm::aarch64::
    /// AsmAarch64Program::parse_text`) rather than a binary object file.
    /// The one item's path is empty — `NativeEmitter::emit_package_artifact`
    /// treats an empty-path single item as "one plain object/asm", not an
    /// archive (see `from_archive`, whose members are each tagged with
    /// their own non-empty path).
    pub fn from_asm(package_id: fp_core::ast::package::PackageId, asm: AsmProgram) -> Self {
        let mut source = Self::empty_source(&package_id);
        source.items.push(fp_core::ast::package::PackageItem {
            module_path: fp_core::ast::path::QualifiedPath::new(Vec::new()),
            item: fp_core::ast::Item::precompiled_asm(asm),
        });
        Self::from_source(package_id, source)
    }

    /// A native archive (`.a`/`.lib`) given directly as `fp compile`'s
    /// input — one item per member, each tagged with the member's own
    /// name as its `QualifiedPath` (so `NativeEmitter::emit_package_artifact`
    /// can recover it when repacking the retargeted archive). A member
    /// recognized as an object file lifts to `PrecompiledAsm`, the same
    /// as a standalone object; anything else (e.g. a symbol-table member)
    /// carries its raw bytes as an opaque `PrecompiledArtifact` and is
    /// repacked verbatim, unretargeted — this mirrors exactly what the
    /// bespoke `container/pipeline.rs` archive transpile used to do.
    pub fn from_archive(
        package_id: fp_core::ast::package::PackageId,
        bytes: &[u8],
    ) -> Result<Self> {
        let members = crate::archive::read_archive_members(bytes)
            .map_err(|err| Error::from(format!("Failed to parse archive input: {err}")))?;
        let object_reader = crate::container::ObjectContainerReader::new();
        let mut source = Self::empty_source(&package_id);
        for member in members {
            let item = if !member.data.is_empty() && object_reader.can_read(&member.data) {
                let asm = crate::binary::lift_object_to_asmir(&member.data).map_err(|err| {
                    Error::from(format!(
                        "Failed to lift archive member '{}': {err}",
                        member.name
                    ))
                })?;
                fp_core::ast::Item::precompiled_asm(asm)
            } else {
                fp_core::ast::Item::precompiled_artifact(member.data)
            };
            source.items.push(fp_core::ast::package::PackageItem {
                module_path: fp_core::ast::path::QualifiedPath::new(vec![member.name]),
                item,
            });
        }
        Ok(Self::from_source(package_id, source))
    }

    fn empty_source(
        package_id: &fp_core::ast::package::PackageId,
    ) -> fp_core::ast::package::AstPackage {
        fp_core::ast::package::AstPackage::new(
            package_id.clone(),
            package_id.as_str().to_string(),
            fp_core::ast::package::graph::PackageGraph::new(Vec::new()),
        )
    }

    fn from_source(
        package_id: fp_core::ast::package::PackageId,
        source: fp_core::ast::package::AstPackage,
    ) -> Self {
        let descriptor = fp_core::ast::package::PackageDescriptor {
            id: package_id.clone(),
            name: package_id.as_str().to_string(),
            version: None,
            manifest_path: fp_core::vfs::VirtualPath::new_relative(Vec::<String>::new()),
            root: fp_core::vfs::VirtualPath::new_relative(Vec::<String>::new()),
            metadata: fp_core::ast::package::PackageMetadata::default(),
            modules: Vec::new(),
        };
        Self {
            package_id,
            descriptor: std::sync::Arc::new(descriptor),
            source,
        }
    }
}

impl fp_core::ast::package::provider::PackageProvider for NativeObjectPackageProvider {
    fn list_packages(
        &self,
    ) -> fp_core::ast::package::provider::ProviderResult<Vec<fp_core::ast::package::PackageId>>
    {
        Ok(vec![self.package_id.clone()])
    }

    fn workspace_packages(
        &self,
    ) -> fp_core::ast::package::provider::ProviderResult<Vec<fp_core::ast::package::PackageId>>
    {
        self.list_packages()
    }

    fn intrinsic_normalizer(&self) -> Box<dyn fp_core::intrinsics::IntrinsicNormalizer> {
        Box::new(fp_core::intrinsics::NoopIntrinsicNormalizer)
    }

    fn load_package_metadata(
        &self,
        id: &fp_core::ast::package::PackageId,
    ) -> fp_core::ast::package::provider::ProviderResult<
        std::sync::Arc<fp_core::ast::package::PackageDescriptor>,
    > {
        if id != &self.package_id {
            return Err(
                fp_core::ast::package::provider::ProviderError::PackageNotFound(id.clone()),
            );
        }
        Ok(self.descriptor.clone())
    }

    fn refresh(&self) -> fp_core::ast::package::provider::ProviderResult<()> {
        Ok(())
    }

    fn load_package_source(
        &self,
        id: &fp_core::ast::package::PackageId,
    ) -> fp_core::ast::package::provider::ProviderResult<fp_core::ast::package::AstPackage> {
        if id != &self.package_id {
            return Err(
                fp_core::ast::package::provider::ProviderError::PackageNotFound(id.clone()),
            );
        }
        Ok(self.source.clone())
    }
}

pub type NativeCompiler = NativeEmitter;
