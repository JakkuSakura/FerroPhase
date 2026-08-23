use std::collections::HashMap;

use crate::ast::package::PackageId;

use super::MirPackage;

/// The whole compiled result across every package this compilation session
/// has produced MIR for, keyed by `PackageId` — mirrors `hir::HirProgram`'s
/// own shape one layer further down (a `MirPackage` is itself keyed by
/// `DefId`, see `MirPackage::units`). Lives on `CompilerState` as the one
/// place MIR lowering results accumulate; `CompiledPackage.mir` holds a
/// single package's own `MirPackage` once that package's compile finishes
/// (mirroring how `CompiledPackage.hir_program` holds one `HirPackage`).
#[derive(Debug, Clone, Default, PartialEq)]
pub struct MirProgram {
    pub packages: HashMap<PackageId, MirPackage>,
}

impl MirProgram {
    pub fn new() -> Self {
        Self {
            packages: HashMap::new(),
        }
    }

    pub fn package(&self, id: &PackageId) -> Option<&MirPackage> {
        self.packages.get(id)
    }

    pub fn package_mut(&mut self, id: &PackageId) -> &mut MirPackage {
        self.packages.entry(id.clone()).or_default()
    }
}
