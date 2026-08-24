use std::collections::HashMap;

use crate::ast::package::PackageId;

use super::{LirBlobError, LirCodeUnitKind, LirDataLayout, LirFunction, LirGlobal, LirPackage, Name};

/// The whole compiled result across every package this compilation session
/// has produced LIR for, keyed by `PackageId` — mirrors `mir::MirProgram`/
/// `hir::HirProgram`'s own shape: a `LirProgram` is a collection of
/// `LirPackage`s, each of which is already a collection of `LirCodeUnit`s
/// (via its own `own_artifacts: LirUnitTable`, keyed by `Name`).
#[derive(Debug, Clone, PartialEq)]
pub struct LirProgram {
    pub packages: HashMap<PackageId, LirPackage>,
}

impl LirProgram {
    pub fn new() -> Self {
        Self {
            packages: HashMap::new(),
        }
    }

    pub fn package(&self, id: &PackageId) -> Option<&LirPackage> {
        self.packages.get(id)
    }

    /// Wraps a single already-lowered `LirBlob` as a one-package
    /// `LirProgram` — for callers (e.g. the LIR interpreter's own
    /// `--target interpret` backend) that only ever get handed one
    /// package's flat, already-merged blob and have no other package to
    /// look anything up in.
    pub fn from_single_blob(
        package_id: PackageId,
        module_path: crate::ast::path::QualifiedPath,
        blob: super::LirBlob,
    ) -> Result<Self, LirBlobError> {
        let mut table = super::LirUnitTable::new(blob.data_layout.clone());
        table.add_program(package_id.clone(), module_path, blob)?;
        let mut packages = HashMap::new();
        packages.insert(package_id, LirPackage { own_artifacts: table });
        Ok(Self { packages })
    }

    /// Looks a function up by name within one specific package — the
    /// direct on-demand replacement for pre-populating a lookup cache
    /// from that package's own artifacts.
    pub fn find_function(&self, package_id: &PackageId, name: &Name) -> Option<&LirFunction> {
        self.package(package_id)?
            .own_artifacts
            .find_function(package_id.clone(), name)
    }

    /// Same as `find_function`, but searches every loaded package for a
    /// match — the on-demand replacement for a flattened, bare-name
    /// function cache spanning the whole program.
    pub fn find_function_any_package(&self, name: &Name) -> Option<&LirFunction> {
        self.packages
            .iter()
            .find_map(|(package_id, package)| package.own_artifacts.find_function(package_id.clone(), name))
    }

    /// Looks a function up by its own `DefId` — `def_id` already carries
    /// its owning package's id, so no separate package lookup is needed.
    pub fn find_function_by_def_id(&self, def_id: &crate::hir::DefId) -> Option<&LirFunction> {
        let package_id = PackageId::new(def_id.package_id.as_str());
        self.package(&package_id)?
            .own_artifacts
            .artifacts()
            .iter()
            .find_map(|artifact| match &artifact.kind {
                LirCodeUnitKind::Function(function) if function.def_id.as_ref() == Some(def_id) => {
                    Some(function)
                }
                _ => None,
            })
    }

    /// Looks a global up by name within one specific package.
    pub fn find_global(&self, package_id: &PackageId, name: &Name) -> Option<&LirGlobal> {
        self.package(package_id)?
            .own_artifacts
            .find_global(package_id.clone(), name)
    }

    /// Every package holds its own target `LirDataLayout` (`LirPackage::new`
    /// needs one), so getting-or-inserting can't just be `entry(..).or_default()`
    /// — `data_layout` is only needed on a genuine first insert.
    pub fn package_mut(&mut self, id: &PackageId, data_layout: &LirDataLayout) -> &mut LirPackage {
        self.packages
            .entry(id.clone())
            .or_insert_with(|| LirPackage::new(data_layout.clone()))
    }

    /// Merges every OTHER loaded package's own LIR artifacts into `id`'s
    /// own (dependencies first, mirroring the same merge order
    /// `evaluate_comptime_lir` uses for comptime execution), producing one
    /// flat, executable `LirBlob` — moved from the old
    /// `AstProgram::merged_lir_program`, which additionally resolved and
    /// renamed `id`'s `main` function; that step now happens at the call
    /// site instead (it needs HIR data this type doesn't have — see
    /// `crate::ast::package::resolve_entrypoint_def_id`/`rename_lir_function`).
    pub fn merged_blob_for_package(&self, id: &PackageId) -> crate::error::Result<super::LirBlob> {
        let package = self.packages.get(id).ok_or_else(|| {
            crate::error::Error::from(format!(
                "compiled package `{id}` is unavailable for LIR merging"
            ))
        })?;
        if package.own_artifacts.artifacts().is_empty() {
            return Err(crate::error::Error::from(format!(
                "compiled package `{id}` contains no LIR artifacts"
            )));
        }
        let mut combined = super::LirUnitTable::new(package.own_artifacts.data_layout.clone());
        for (dependency_id, dep_package) in self.packages.iter() {
            if dependency_id == id {
                continue;
            }
            combined
                .add_workspace(&dep_package.own_artifacts)
                .map_err(|error| crate::error::Error::from(error.to_string()))?;
        }
        combined
            .add_workspace(&package.own_artifacts)
            .map_err(|error| crate::error::Error::from(error.to_string()))?;
        Ok(combined.to_blob())
    }
}

impl Default for LirProgram {
    fn default() -> Self {
        Self::new()
    }
}
