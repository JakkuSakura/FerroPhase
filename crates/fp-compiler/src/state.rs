use std::rc::Rc;

use fp_core::{
    ast::package::PackageId,
    ast::program::AstProgram,
    executor::ExecutorHandle,
    hir, lir, mir,
};
use fp_interpret::LirInterpreter;
use fp_typing::ComptimeResolver;

use crate::error::CompilerDriverError;

pub struct CompilerState {
    /// Every package's own HIR published so far this session — mirrors
    /// `mir_program`/`lir_program` below; keyed internally by `hir::PackageId`
    /// (see `HirProgram`'s own shape), not the compiler's surface
    /// `ast::package::PackageId`, since a `hir::Package`'s identity is
    /// always the former. Typed results (expr/pat types, resolutions,
    /// diagnostics, ...) live directly on each published `HirPackage` —
    /// there's no separate typed-results table to keep in sync. `Rc`-wrapped
    /// so callers that need their own handle onto the whole program
    /// (`HirToMirLowerer::new`, `hir_program_rc`) get a cheap pointer clone
    /// instead of deep-cloning every published package's HIR on every call.
    hir_program: Rc<hir::HirProgram>,
    /// Every package's MIR content produced so far this session, one
    /// `mir::MirCodeUnit` per top-level `DefId` (see `mir::MirPackage`'s
    /// own doc comment) — replaces the old `BTreeMap<MirId, MirModule>`,
    /// which stored one whole flattened program per artifact-id and had no
    /// way to update just the one item a resolved comptime value actually
    /// affects. `Rc`-wrapped for the same reason as `lir_program` below —
    /// `MirToLirLowerer` owns a cloned handle directly (`MirToLirLowerer::new`)
    /// instead of the driver cloning dependency packages/own units out of
    /// it on every call.
    mir_program: Rc<mir::MirProgram>,
    /// Mirrors `mir_program` for LIR — one `lir::LirProgram`, a collection
    /// of `LirPackage`s, each just a `LirBlob`. `Rc`-wrapped so
    /// `evaluate_comptime_lir`'s `LirInterpreter::load_program` (which
    /// needs its own `Rc<lir::LirProgram>`) can clone the handle instead of
    /// deep-cloning the whole program on every comptime evaluation; mutating
    /// methods below go through `Rc::make_mut` (a real clone only if some
    /// other `Rc` clone — e.g. one still loaded into `interpreter` — is
    /// outstanding).
    lir_program: Rc<lir::LirProgram>,
    /// The one renamed entrypoint function `select_entrypoint` builds per
    /// selection (the resolved entrypoint, renamed to its bare linkage
    /// name) — just that one `LirCodeUnit`, not a whole duplicated
    /// `LirBlob`: every other function it might call already lives in
    /// `lir_program`'s own per-package storage, so only the rename itself
    /// needs its own slot. `runtime_blob` reassembles the full executable
    /// blob on demand by substituting this into the package's own flattened
    /// LIR. Cached by `lir::LirPath` since it's one specific
    /// package+entrypoint selection, not per-`DefId` lowering output, so it
    /// doesn't fit the `lir_program` hierarchy above.
    runtime_programs: std::collections::HashMap<lir::LirPath, lir::LirCodeUnit>,
    runtime_entrypoints: std::collections::HashMap<lir::LirPath, hir::DefId>,
    /// This package's comptime resolver, wired up by the driver
    /// (`make_comptime_resolver`) before `TypingShared` exists — a
    /// package's HIR isn't generated yet at that point, so there's nowhere
    /// else to stash it until `type_check_program` builds the real
    /// `TypingShared` and hands the resolver straight through.
    pub comptime_resolver: Option<ComptimeResolver>,
    /// The compiled-package registry for this compilation session.
    pub workspace: Rc<AstProgram>,
    /// Target ABI data shared by typing-triggered comptime blocks and
    /// normal MIR-to-LIR lowering for this compilation session — same
    /// rationale as `workspace` above.
    pub data_layout: lir::LirDataLayout,
    /// What the active `TargetBackend` can express directly — see
    /// `fp_core::capabilities::LanguageCapabilities`. Defaults to `NATIVE`
    /// (nothing first-class); `fp-cli` reads the real value off its
    /// already-constructed backend (`TargetBackend::capabilities`) and
    /// sets it here before compiling (`set_backend_capabilities`).
    backend_capabilities: fp_core::capabilities::LanguageCapabilities,
    /// The one shared task pool every suspendable unit of driver work runs
    /// through: per-compile-unit HIR typing tasks and compiler-owned
    /// comptime work. Lives here since scheduling ("what task runs next")
    /// is the driver's concern, not typing's. `Rc`, not `Rc<RefCell<_>>`:
    /// `CompilerExecutor` is already internally
    /// interior-mutable (its own methods take `&self`, specifically so a
    /// task can reentrantly `spawn`/`contains`-check it from within its own
    /// poll).
    pub(crate) tasks: ExecutorHandle,
    /// The one `LirInterpreter` real comptime evaluation runs through
    /// (`evaluate_comptime_lir`) — lives here, not on `CompilerDriver`,
    /// since `evaluate_comptime_lir` is a free-standing fn taking only
    /// `&Rc<RefCell<CompilerState>>` (it's reached both from `&mut self`
    /// driver methods and from `resolve_comptime_request_with`'s
    /// mid-typing-pass, free-standing context).
    interpreter: LirInterpreter,
}

impl CompilerState {
    pub fn new(data_layout: lir::LirDataLayout, tasks: ExecutorHandle) -> Self {
        Self::with_workspace(
            data_layout,
            tasks,
            Rc::new(AstProgram::new(std::sync::Arc::new(
                fp_core::ast::package::provider::EmptyProvider,
            ))),
        )
    }

    pub fn with_workspace(
        data_layout: lir::LirDataLayout,
        tasks: ExecutorHandle,
        workspace: Rc<AstProgram>,
    ) -> Self {
        Self {
            hir_program: Rc::new(hir::HirProgram::new()),
            mir_program: Rc::new(mir::MirProgram::new()),
            lir_program: Rc::new(lir::LirProgram::new()),
            runtime_programs: std::collections::HashMap::new(),
            runtime_entrypoints: std::collections::HashMap::new(),
            comptime_resolver: None,
            workspace,
            data_layout,
            backend_capabilities: fp_core::capabilities::LanguageCapabilities::NATIVE,
            tasks,
            interpreter: LirInterpreter::new(),
        }
    }

    /// The shared `LirInterpreter` real comptime evaluation runs through —
    /// see the field's own doc comment for why it lives here.
    pub fn interpreter_mut(&mut self) -> &mut LirInterpreter {
        &mut self.interpreter
    }

    /// Publishes `package` under its own `id` — `HirProgram::add_package`
    /// already keys by that, so no separate id parameter is needed.
    pub fn insert_hir(&mut self, package: hir::HirPackage) {
        Rc::make_mut(&mut self.hir_program).add_package(std::rc::Rc::new(package));
    }

    /// Records `def_id`'s own lowered content — the only way `mir_program`
    /// is ever written, so re-lowering one item after a comptime value
    /// resolves is always this exact call with a fresh unit (see
    /// `mir::MirCodeUnit`'s doc comment), never a partial in-place edit.
    pub fn insert_mir_unit(
        &mut self,
        package_id: &PackageId,
        def_id: hir::DefId,
        unit: mir::MirCodeUnit,
    ) {
        Rc::make_mut(&mut self.mir_program)
            .package_mut(package_id)
            .insert_unit(def_id, unit);
    }

    pub fn mir_unit(&self, package_id: &PackageId, def_id: hir::DefId) -> Option<&mir::MirCodeUnit> {
        self.mir_program.package(package_id)?.unit(def_id)
    }

    /// Read-only view of every package's MIR compiled so far this session.
    pub fn mir_program(&self) -> &mir::MirProgram {
        &self.mir_program
    }

    /// Cheap `Rc` clone of the whole session's `mir::MirProgram` — what
    /// `MirToLirLowerer::new` owns directly (its lazy callee-signature/
    /// ADT-def lookups read straight off this, in this package first and
    /// then every other loaded package, without requiring a whole-program
    /// predeclare sweep).
    pub fn mir_program_rc(&self) -> Rc<mir::MirProgram> {
        self.mir_program.clone()
    }

    /// Folds `struct_fields`/`adt_defs` produced while lowering
    /// `package_id`'s HIR into its `mir::MirPackage` — the per-package
    /// tables `MirToLirLowerer` reads alongside the package's lowered
    /// units.
    pub fn extend_mir_package(
        &mut self,
        package_id: &PackageId,
        struct_fields: impl IntoIterator<Item = (mir::DefId, Vec<mir::Ty>)>,
        adt_defs: impl IntoIterator<Item = (hir::DefId, mir::ty::AdtDef)>,
    ) {
        let package = Rc::make_mut(&mut self.mir_program).package_mut(package_id);
        package.extend_struct_fields(struct_fields);
        package.extend_adt_defs(adt_defs);
    }

    /// Resets `package_id`'s LIR back to empty — used when re-lowering a
    /// whole package after a comptime value resolves
    /// (`CompilerDriver::relower_cached_lir_units`), since
    /// `LirBlob::extend` errors on a data-layout mismatch rather than
    /// silently overwriting the previous lowering, and appending onto a
    /// stale blob would just duplicate everything already in it.
    pub fn reset_lir_package(&mut self, package_id: &PackageId) {
        let data_layout = self.data_layout.clone();
        Rc::make_mut(&mut self.lir_program)
            .packages
            .insert(package_id.clone(), lir::LirPackage::new(data_layout));
    }

    /// Merges `blob` into `package_id`'s own `LirBlob` — the only way
    /// `lir_program` is ever written, mirroring `insert_mir_unit`.
    pub fn insert_lir_blob_for_package(
        &mut self,
        package_id: &PackageId,
        blob: lir::LirBlob,
    ) -> Result<(), CompilerDriverError> {
        let data_layout = self.data_layout.clone();
        Rc::make_mut(&mut self.lir_program)
            .package_mut(package_id, &data_layout)
            .blob
            .extend(blob)
            .map_err(|error| CompilerDriverError::Core(error.to_string().into()))
    }

    /// `package_id`'s own `lir::LirBlob`. Empty (not an error) if the
    /// package hasn't produced any LIR yet.
    pub fn lir_blob(&self, package_id: &PackageId) -> lir::LirBlob {
        self.lir_program
            .package(package_id)
            .map(|package| package.blob.clone())
            .unwrap_or_else(|| lir::LirBlob::new(self.data_layout.clone()))
    }

    /// The whole session's `lir::LirProgram` — used by callers (`fp-cli`'s
    /// `run_compile_pipeline`) that need to merge a package's LIR together
    /// with every dependency's (`LirProgram::merged_blob_for_package`)
    /// before handing it to a `TargetBackend`.
    pub fn lir_program(&self) -> &lir::LirProgram {
        &self.lir_program
    }

    /// Cheap `Rc` clone of the whole session's `lir::LirProgram` — what
    /// `evaluate_comptime_lir` hands to `LirInterpreter::load_program`
    /// (which needs to own its own `Rc`), instead of deep-cloning the
    /// program on every comptime evaluation.
    pub fn lir_program_rc(&self) -> Rc<lir::LirProgram> {
        self.lir_program.clone()
    }

    /// The whole session's `hir::HirProgram` — used by callers
    /// (`HirToAstLifter`, `AstToHirLowerer::with_hir_program`) that need
    /// cross-package HIR lookups (`find_export`, `find_hir_impl_method`,
    /// `find_hir_enum_for_variant`, ...), now that `AstProgram` no longer
    /// carries HIR content itself.
    pub fn hir_program(&self) -> &hir::HirProgram {
        &self.hir_program
    }

    /// Cheap `Rc` clone of the whole session's `hir::HirProgram` — what
    /// `HirToMirLowerer::new` needs its own handle onto (it resolves
    /// `current_package` straight out of it), instead of deep-cloning every
    /// published package's HIR on every lowering call.
    pub fn hir_program_rc(&self) -> Rc<hir::HirProgram> {
        self.hir_program.clone()
    }

    /// Records the one renamed entrypoint function `select_entrypoint`
    /// produced — a single `LirCodeUnit`, not a whole duplicated `LirBlob`:
    /// every other function the entrypoint might call already lives in
    /// this package's own `lir_program` entry, so only the one thing that
    /// actually changed (the rename) needs its own storage.
    pub fn insert_runtime_program(&mut self, lir_path: lir::LirPath, unit: lir::LirCodeUnit) {
        self.runtime_programs.insert(lir_path, unit);
    }

    /// Assembles the full, executable `LirBlob` for a selected entrypoint:
    /// `package_id`'s own flattened LIR (`lir_blob`), with `def_id`'s
    /// original (mangled-named) function swapped out for the one renamed
    /// `LirCodeUnit` `select_entrypoint` recorded under `lir_path`.
    pub fn runtime_blob(
        &self,
        package_id: &PackageId,
        lir_path: &lir::LirPath,
        def_id: hir::DefId,
    ) -> Result<lir::LirBlob, CompilerDriverError> {
        let unit = self.runtime_programs.get(lir_path).ok_or_else(|| {
            CompilerDriverError::MissingLir(format!("{lir_path:?}"))
        })?;
        let lir::LirCodeUnitKind::Function(renamed) = &unit.kind else {
            return Err(CompilerDriverError::Interpreter(
                "runtime entrypoint unit is not a function".to_string(),
            ));
        };
        let mut blob = self.lir_blob(package_id);
        blob.functions.retain(|function| function.def_id != Some(def_id.clone()));
        blob.functions.push(renamed.clone());
        Ok(blob)
    }

    pub fn insert_runtime_entrypoint(&mut self, lir_path: lir::LirPath, def_id: hir::DefId) {
        self.runtime_entrypoints.insert(lir_path, def_id);
    }

    pub fn runtime_entrypoint(&self, lir_path: &lir::LirPath) -> Result<hir::DefId, CompilerDriverError> {
        self.runtime_entrypoints
            .get(lir_path)
            .cloned()
            .ok_or_else(|| {
                CompilerDriverError::Interpreter("program has no explicit entrypoint".to_string())
            })
    }

    pub fn set_backend_capabilities(&mut self, capabilities: fp_core::capabilities::LanguageCapabilities) {
        self.backend_capabilities = capabilities;
    }

    pub fn backend_capabilities(&self) -> fp_core::capabilities::LanguageCapabilities {
        self.backend_capabilities
    }

    pub fn hir(&self, package_id: hir::PackageId) -> Result<hir::HirPackage, CompilerDriverError> {
        self.hir_program
            .package(&package_id)
            .cloned()
            .ok_or_else(|| CompilerDriverError::MissingHir(format!("{package_id:?}")))
    }

    /// Every package's own HIR compiled so far — used by `drain_driver` to
    /// report typing diagnostics, which live directly on each package
    /// (see `hir::HirPackage::diagnostics`'s doc comment), not on the
    /// driver's scratch, per-package `TypingShared`.
    pub fn all_packages(&self) -> impl Iterator<Item = &Rc<hir::HirPackage>> {
        self.hir_program.packages.values()
    }
}
