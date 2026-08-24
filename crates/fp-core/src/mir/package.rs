use std::collections::HashMap;

use super::{ty, DefId, Function, ItemKind, MirCodeUnit, Ty};

/// A package's MIR-level identity — the same `hir::PackageId` a `DefId`
/// embeds, reused directly (not a separate namespace) since MIR items are
/// always lowered 1:1 from an already-identified HIR package.
pub type PackageId = crate::hir::PackageId;

/// One compiled package's MIR content — its lowered items, one
/// `MirCodeUnit` per top-level `DefId`, plus the derived tables
/// `HirToMirLowerer` produces alongside them. Pairs with `MirProgram` the same
/// way `hir::HirPackage` pairs with `hir::HirProgram`; several of these
/// live on `CompiledPackage`'s `mir` field, one per package.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct MirPackage {
    /// This package's lowered content, one `MirCodeUnit` per top-level
    /// `DefId` — see `MirCodeUnit`'s own doc comment for why this is
    /// partitioned instead of one flat blob. Use `items()`/`bodies()` for a
    /// whole-package view.
    pub units: HashMap<DefId, MirCodeUnit>,
    /// Struct field types keyed by `DefId`, computed during MIR lowering.
    pub struct_fields: HashMap<DefId, Vec<Ty>>,
    pub adt_defs: HashMap<crate::hir::DefId, ty::AdtDef>,
    /// Each top-level function's own `mir::Function` (name/sig/substs/abi),
    /// keyed by the same `DefId` as `units` — maintained incrementally as
    /// units are inserted, not swept eagerly over a whole package. This
    /// is what lets `MirToLirLowerer` resolve a callee's LIR signature lazily,
    /// on first reference (see `MirToLirLowerer::with_signature_resolver`),
    /// instead of requiring every function in the package to be predeclared
    /// up front before any body is lowered.
    pub sigs: HashMap<DefId, Function>,
}

impl MirPackage {
    /// Replaces (or inserts) the `MirCodeUnit` produced for `def_id` — the
    /// only way this package's `units` map is ever written, so re-lowering
    /// one item after a comptime value resolves is always this exact call
    /// with a fresh unit, never a partial in-place edit. Also (re)records
    /// the unit's own function signature into `sigs`, if it lowered to a
    /// `Function` item — the incremental counterpart to a whole-module
    /// predeclare sweep.
    pub fn insert_unit(&mut self, def_id: DefId, unit: MirCodeUnit) {
        for item in &unit.items {
            if let ItemKind::Function(func) = &item.kind {
                if func.def_id.as_ref() == Some(&def_id) {
                    self.sigs.insert(def_id.clone(), func.clone());
                }
            }
        }
        self.units.insert(def_id, unit);
    }

    pub fn unit(&self, def_id: DefId) -> Option<&MirCodeUnit> {
        self.units.get(&def_id)
    }

    /// All items across every unit, in no particular cross-unit order —
    /// the whole-package view for anything that used to iterate a flat module's items.
    pub fn items(&self) -> impl Iterator<Item = &super::Item> {
        self.units.values().flat_map(|unit| unit.items.iter())
    }

    /// All bodies across every unit, keyed by `BodyId` — the whole-package
    /// view for anything that used to iterate a flat module's bodies.
    pub fn bodies(&self) -> impl Iterator<Item = (&super::BodyId, &super::Body)> {
        self.units.values().flat_map(|unit| unit.bodies.iter())
    }

    /// Mutable counterpart to `bodies()`, for whole-package passes (e.g. the
    /// MIR optimizer) that rewrite bodies in place regardless of which unit
    /// they came from.
    pub fn bodies_mut(&mut self) -> impl Iterator<Item = (&super::BodyId, &mut super::Body)> {
        self.units.values_mut().flat_map(|unit| unit.bodies.iter_mut())
    }

    /// Looks up a body by `BodyId` across all units, since a package's bodies
    /// aren't otherwise addressable without knowing which unit produced them.
    pub fn body(&self, id: super::BodyId) -> Option<&super::Body> {
        self.units.values().find_map(|unit| unit.bodies.get(&id))
    }

    /// Mutable counterpart to `body()`.
    pub fn body_mut(&mut self, id: super::BodyId) -> Option<&mut super::Body> {
        self.units.values_mut().find_map(|unit| unit.bodies.get_mut(&id))
    }

    pub fn span(&self) -> super::Span {
        super::Span::union(self.items().map(super::Item::span))
    }

    pub fn extend_struct_fields(&mut self, entries: impl IntoIterator<Item = (DefId, Vec<Ty>)>) {
        self.struct_fields.extend(entries);
    }

    pub fn extend_adt_defs(&mut self, entries: impl IntoIterator<Item = (crate::hir::DefId, ty::AdtDef)>) {
        self.adt_defs.extend(entries);
    }
}
