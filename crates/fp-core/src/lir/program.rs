use std::collections::HashMap;

use crate::ast::package::PackageId;

use super::{LirDataLayout, LirPackage};

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

    /// Every package holds its own target `LirDataLayout` (`LirPackage::new`
    /// needs one), so getting-or-inserting can't just be `entry(..).or_default()`
    /// — `data_layout` is only needed on a genuine first insert.
    pub fn package_mut(&mut self, id: &PackageId, data_layout: &LirDataLayout) -> &mut LirPackage {
        self.packages
            .entry(id.clone())
            .or_insert_with(|| LirPackage::new(data_layout.clone()))
    }
}

impl Default for LirProgram {
    fn default() -> Self {
        Self::new()
    }
}
