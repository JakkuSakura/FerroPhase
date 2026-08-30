use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::package::PackageId;

use super::MirPackage;

/// The whole compiled result across every package this compilation session
/// has produced MIR for, keyed by `PackageId` — mirrors `hir::HirProgram`'s
/// own shape one layer further down (a `MirPackage` is itself keyed by
/// `DefId`, see `MirPackage::units`). Lives on `CompilerState` as the one
/// place MIR lowering results accumulate. Each package is `Rc<RefCell<_>>`
/// (not owned by value) so `HirToMirLowerer` can hold the exact same handle
/// `CompilerState` stores and write struct/enum layouts, method tables, and
/// const values directly into it as it lowers — no separate "compute
/// locally, then merge into the session's real package" step, and no risk
/// of losing work a re-lowering call would otherwise have to recompute.
#[derive(Debug, Clone, Default)]
pub struct MirProgram {
    pub packages: HashMap<PackageId, Rc<RefCell<MirPackage>>>,
}

impl MirProgram {
    pub fn new() -> Self {
        Self {
            packages: HashMap::new(),
        }
    }

    pub fn package(&self, id: &PackageId) -> Option<Rc<RefCell<MirPackage>>> {
        self.packages.get(id).cloned()
    }

    /// This package's shared handle, creating an empty one on first
    /// reference — every caller that wants to read or write `id`'s own
    /// `MirPackage` goes through this (or `package`, for a caller that
    /// tolerates "not compiled yet" as `None`), never `self.packages`
    /// directly.
    pub fn package_rc(&mut self, id: &PackageId) -> Rc<RefCell<MirPackage>> {
        self.packages.entry(id.clone()).or_default().clone()
    }

    /// Finds a concrete function definition and its owning package by the
    /// resolved definition identity. This is the authoritative cross-package
    /// lookup for downstream lowering stages.
    pub fn function_by_def_id(
        &self,
        def_id: &crate::hir::DefId,
    ) -> Option<(PackageId, super::Function)> {
        let package_id = def_id.package_id.clone();
        self.packages.get(&package_id).and_then(|package| {
            package
                .borrow()
                .sigs
                .get(def_id)
                .cloned()
                .map(|function| (package_id, function))
        })
    }

    /// Finds a callable signature for a resolved definition. Impl methods
    /// and signature-only declarations do not necessarily have a concrete
    /// `Function` body in this program, so callers that only need call
    /// metadata must use this identity-based API instead of fabricating one.
    pub fn signature_by_def_id(
        &self,
        def_id: &crate::hir::DefId,
    ) -> Option<(
        PackageId,
        crate::mir::Symbol,
        super::FunctionSig,
        super::SubstsRef,
    )> {
        let package_id = def_id.package_id.clone();
        self.packages.get(&package_id).and_then(|package| {
            let package = package.borrow();
            if let Some(info) = package.method_lookup_by_def.get(def_id) {
                return Some((
                    package_id.clone(),
                    info.fn_name.clone().into(),
                    info.sig.clone(),
                    info.substs.clone(),
                ));
            }
            package
                .function_sigs
                .get(def_id)
                .cloned()
                .map(|sig| (package_id, format!("fn#{}", def_id).into(), sig, Vec::new()))
        })
    }
}
