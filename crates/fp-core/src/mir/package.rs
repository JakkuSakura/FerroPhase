use std::collections::HashMap;

use super::{ty, Constant, DefId, Function, ItemKind, MirCodeUnit, MirModule, Ty};

/// One compiled package's MIR content — its lowered items, one
/// `MirCodeUnit` per top-level `DefId`, plus the derived tables
/// `HirToMirLowerer` produces alongside them. Pairs with `MirProgram` the same
/// way `hir::HirPackage` pairs with `hir::HirProgram`; several of these
/// live on `CompiledPackage`'s `mir` field, one per package.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct MirPackage {
    /// This package's lowered content, one `MirCodeUnit` per top-level
    /// `DefId` — see `MirCodeUnit`'s own doc comment for why this is
    /// partitioned instead of one flat blob. Use `flatten` to get the
    /// whole-package `MirModule` view most MIR-consuming backends still
    /// expect.
    pub units: HashMap<DefId, MirCodeUnit>,
    /// Struct field types keyed by `DefId`, computed during MIR lowering.
    pub struct_fields: HashMap<DefId, Vec<Ty>>,
    pub adt_defs: HashMap<crate::hir::DefId, ty::AdtDef>,
    /// Top-level consts resolved by direct constant-folding during MIR
    /// lowering (see `HirToMirLowerer::lower_const`'s fast path) — a
    /// directly-foldable const (no `let`, no side effects requiring the
    /// real interpreter) never becomes a comptime entry, so without this,
    /// nothing would ever surface its value to a caller that only knows
    /// how to ask "what did evaluating this package's comptime entries
    /// produce" (e.g. `evaluate_comptime_lir`'s "no comptime entries at
    /// all" case, which otherwise has nothing to fall back to but an
    /// arbitrary placeholder).
    pub resolved_const_values: HashMap<String, Constant>,
    /// The originating `hir::DefId` of each `resolved_const_values` entry,
    /// keyed by the same name — populated only at the entry's true origin
    /// (`HirToMirLowerer::lower_const`'s fold fast path, which always has the
    /// const item's own `DefId` in scope), not by `seed_resolved_const`'s
    /// cross-pass reseeding (which only needs the value, not its identity).
    /// Lets the driver record a folded const's value onto
    /// `hir::HirPackage::const_values` (DefId-keyed) without re-deriving
    /// identity from the name string.
    pub resolved_const_defs: HashMap<String, DefId>,
    /// For a `const { .. }` block found incidentally while lowering some
    /// other item's body, maps that block's own synthetic comptime probe
    /// `DefId` (`mir::ExecutableConst::def_id`/`lir::LirComptimeEntry::
    /// def_id` — freshly minted per block, unrelated to any real item) to
    /// the top-level `DefId` of the item whose body actually contains it
    /// (`HirToMirLowerer::current_lowering_def_id` at the moment the block is
    /// found). Once the driver resolves a block's real value
    /// (`evaluate_comptime_lir`, keyed by this same synthetic `DefId`),
    /// this is what tells it exactly which single item needs re-lowering
    /// to fold that value in — instead of re-lowering the whole package.
    pub const_block_owners: HashMap<DefId, DefId>,
    /// Each top-level function's own `mir::Function` (name/sig/substs/abi),
    /// keyed by the same `DefId` as `units` — maintained incrementally as
    /// units are inserted, not swept eagerly over a whole `MirModule`. This
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
                if func.def_id == Some(def_id) {
                    self.sigs.insert(def_id, func.clone());
                }
            }
        }
        self.units.insert(def_id, unit);
    }

    pub fn unit(&self, def_id: DefId) -> Option<&MirCodeUnit> {
        self.units.get(&def_id)
    }

    /// Folds every unit's `items`/`bodies` together into one flat
    /// `MirModule` — the view `MirToLirLowerer` and every other MIR-consuming
    /// backend (`fp-bytecode`, `fp-jvm`, `fp-cil`, ...) still expects, since
    /// none of them need to know or care that this package's own lowering
    /// happened one `DefId` at a time.
    pub fn flatten(&self) -> MirModule {
        let mut module = MirModule::new();
        for unit in self.units.values() {
            module.items.extend(unit.items.iter().cloned());
            module.bodies.extend(unit.bodies.iter().map(|(k, v)| (*k, v.clone())));
        }
        module
    }

    pub fn extend_struct_fields(&mut self, entries: impl IntoIterator<Item = (DefId, Vec<Ty>)>) {
        self.struct_fields.extend(entries);
    }

    pub fn extend_adt_defs(&mut self, entries: impl IntoIterator<Item = (crate::hir::DefId, ty::AdtDef)>) {
        self.adt_defs.extend(entries);
    }

    pub fn extend_resolved_const_values(&mut self, entries: impl IntoIterator<Item = (String, Constant)>) {
        self.resolved_const_values.extend(entries);
    }

    pub fn extend_resolved_const_defs(&mut self, entries: impl IntoIterator<Item = (String, DefId)>) {
        self.resolved_const_defs.extend(entries);
    }

    pub fn extend_const_block_owners(&mut self, entries: impl IntoIterator<Item = (DefId, DefId)>) {
        self.const_block_owners.extend(entries);
    }

    /// A single folded const's value, by name — see `resolved_const_values`'s
    /// doc comment. Read this in place off an already-borrowed package
    /// rather than cloning the whole map.
    pub fn resolved_const(&self, key: &str) -> Option<&Constant> {
        self.resolved_const_values.get(key)
    }

    /// The originating `DefId` of a single folded const, by the same name
    /// `resolved_const` uses — see `resolved_const_defs`'s doc comment.
    pub fn resolved_const_def(&self, key: &str) -> Option<DefId> {
        self.resolved_const_defs.get(key).copied()
    }
}
