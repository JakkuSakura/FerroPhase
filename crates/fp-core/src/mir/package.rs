use std::collections::HashMap;

use super::{
    ConstInfo, DefId, EnumDefinition, EnumLayout, EnumLayoutKey, EnumVariantInfo, Function,
    FunctionSig, FunctionSpecializationInfo, ItemKind, MethodDefinition, MethodHirRef,
    MethodLoweringInfo, MirCodeUnit, MirId, StructDefinition, StructLayout, StructLayoutKey,
    StructuralLayoutKey, Symbol, Ty, ty,
};

/// A package's MIR-level identity — the same `hir::PackageId` a `DefId`
/// embeds, reused directly (not a separate namespace) since MIR items are
/// always lowered 1:1 from an already-identified HIR package.
pub type PackageId = crate::package::PackageId;

/// Monotonic fresh-id counters for MIR lowering — the "next id to mint"
/// state that used to live as private fields on `HirToMirLowerer`. Lifted
/// onto `MirPackage` so they survive across separate lowering passes over
/// the same package (a package re-lowered after a comptime value resolves
/// keeps minting from where the previous pass left off), instead of
/// resetting to zero on every fresh lowerer instance.
#[derive(Debug, Clone)]
pub struct MirIdCounters {
    pub next_mir_id: MirId,
    pub next_body_id: u32,
    pub next_error_id: u32,
    pub next_synthetic_def_id: DefId,
    pub next_synthetic_hir_def_id: DefId,
}

impl Default for MirIdCounters {
    fn default() -> Self {
        Self {
            next_mir_id: 0,
            next_body_id: 0,
            next_error_id: 0,
            next_synthetic_def_id: DefId::local(1),
            next_synthetic_hir_def_id: DefId::local(1),
        }
    }
}

/// One compiled package's MIR content — its lowered items, one
/// `MirCodeUnit` per top-level `DefId`, plus the derived tables
/// `HirToMirLowerer` produces alongside them. Pairs with `MirProgram` the same
/// way `hir::HirPackage` pairs with `hir::HirProgram`; several of these
/// live on `CompiledPackage`'s `mir` field, one per package.
#[derive(Debug, Clone, Default)]
pub struct MirPackage {
    /// This package's lowered content, one `MirCodeUnit` per top-level
    /// `DefId` — see `MirCodeUnit`'s own doc comment for why this is
    /// partitioned instead of one flat blob. Use `items()`/`bodies()` for a
    /// whole-package view.
    pub units: HashMap<DefId, MirCodeUnit>,
    /// Runtime-support content with no owning `DefId` at all. Deliberately
    /// separate from `units`: `MirCodeUnit` is keyed by the `DefId` that
    /// produced it (see its own doc comment), and this content has no such
    /// owner.
    pub runtime_support: MirCodeUnit,
    /// Every concrete struct/enum instantiation's own field types, keyed by
    /// `(DefId, generic args)` since two instantiations of the same generic
    /// ADT need different field lists — computed once during HIR->MIR
    /// lowering (`HirToMirLowerer::struct_layout_map`/`enum_layout_map`) and
    /// read directly off here by `MirToLirLowerer` (`lookup_full_layout`),
    /// instead of being handed a private copy of the same map at
    /// construction time.
    pub full_layouts: HashMap<(DefId, Vec<Ty>), Vec<Ty>>,
    /// Byte size for an opaque enum-payload-slot placeholder (see
    /// `HirToMirLowerer::opaque_ty_sizes`'s doc comment), keyed by the
    /// placeholder's own synthetic variant name — same idea as
    /// `full_layouts`.
    pub opaque_payload_sizes: HashMap<String, u64>,
    pub adt_defs: HashMap<crate::hir::DefId, ty::AdtDef>,
    /// Each top-level function's own `mir::Function` (name/sig/substs/abi),
    /// keyed by the same `DefId` as `units` — maintained incrementally as
    /// units are inserted, not swept eagerly over a whole package. This
    /// is what lets `MirToLirLowerer` resolve a callee's LIR signature lazily,
    /// on first reference (see `MirToLirLowerer::with_signature_resolver`),
    /// instead of requiring every function in the package to be predeclared
    /// up front before any body is lowered.
    pub sigs: HashMap<DefId, Function>,

    // --- Struct/enum/method/const bookkeeping `HirToMirLowerer` (fp-backend)
    // computes while lowering this package — lives here, not as private
    // fields on the lowerer itself, so a struct/enum layout or method
    // signature computed during one lowering call survives past it (merged
    // in via `CompilerState::mir_package_mut` once lowering finishes),
    // instead of being thrown away and possibly recomputed on a later
    // re-lowering of the same package. ---
    pub struct_defs: HashMap<DefId, StructDefinition>,
    /// Reverse index from a struct's name's tail segment (the part after
    /// its final `::`, or the whole name if unqualified) to every
    /// registered `DefId` sharing that tail — see `HirToMirLowerer::
    /// struct_def_from_ty`'s doc comment for why this exists.
    pub struct_defs_by_tail_name: HashMap<String, Vec<DefId>>,
    pub struct_layouts: HashMap<StructLayoutKey, StructLayout>,
    pub struct_layouts_by_ty: HashMap<Ty, StructLayoutKey>,
    pub structural_defs: HashMap<StructuralLayoutKey, DefId>,
    pub enum_defs: HashMap<DefId, EnumDefinition>,
    /// Reverse index from an enum's exact name to its `DefId` — see
    /// `HirToMirLowerer::enum_defs_by_name`'s doc comment.
    pub enum_defs_by_name: HashMap<String, DefId>,
    pub enum_layouts: HashMap<EnumLayoutKey, EnumLayout>,
    /// Exact reverse index from a flattened-tuple enum representation back
    /// to the `EnumLayoutKey` that produced it — mirrors
    /// `struct_layouts_by_ty`.
    pub enum_layouts_by_ty: HashMap<Ty, EnumLayoutKey>,
    pub enum_variants: HashMap<DefId, EnumVariantInfo>,
    pub enum_variant_names: HashMap<String, DefId>,
    pub const_values: HashMap<DefId, ConstInfo>,
    pub executable_consts: HashMap<DefId, (Symbol, Ty)>,
    /// Pre-substitution `mir::FunctionSig` for a top-level function, keyed
    /// by its HIR `DefId` — distinct from `sigs` (keyed the same way but
    /// storing the fully-lowered `mir::Function`, only populated once a
    /// unit is actually inserted): this is populated by signature-only
    /// registration, before the function's own body is necessarily lowered.
    pub function_sigs: HashMap<DefId, FunctionSig>,
    pub generic_function_defs: HashMap<DefId, crate::hir::Function>,
    pub struct_methods: HashMap<String, HashMap<String, MethodLoweringInfo>>,
    /// For each bare method name, `Some(ty)` if every method registered
    /// under that name (across every struct) so far agrees on the same
    /// declared output type, or `None` once any two disagree.
    pub method_name_output_consensus: HashMap<String, Option<Ty>>,
    pub method_lookup_by_def: HashMap<DefId, MethodLoweringInfo>,
    pub method_lookup: HashMap<String, MethodLoweringInfo>,
    pub method_defs: HashMap<String, MethodDefinition>,
    pub method_defs_by_def: HashMap<DefId, MethodDefinition>,
    /// Raw HIR for every non-generic method, keyed by `impl_item.def_id` —
    /// see `MethodHirRef`'s own doc comment.
    pub method_hir_defs: HashMap<DefId, MethodHirRef>,
    /// Reverse index from `(self_def, method_name)` to the method's key in
    /// `method_defs_by_def`.
    pub method_defs_by_self_and_name: HashMap<(DefId, String), DefId>,
    pub method_specializations: HashMap<(DefId, ty::SubstsRef), MethodLoweringInfo>,
    pub function_specializations: HashMap<(DefId, ty::SubstsRef), FunctionSpecializationInfo>,
    /// Pre-substitution memo, keyed directly on a call site's own raw
    /// argument/return types rather than the `substs` derived from them.
    pub function_specialization_call_cache:
        HashMap<(DefId, Vec<Ty>, Vec<Ty>, Option<Ty>), FunctionSpecializationInfo>,
    pub method_specialization_call_cache:
        HashMap<(DefId, Vec<Ty>, Vec<Ty>, Option<Ty>), MethodLoweringInfo>,
    pub opaque_types: HashMap<String, Ty>,
    /// Byte size for an opaque type minted for a *mismatched enum payload
    /// slot* — see `HirToMirLowerer::opaque_ty_sizes`'s doc comment.
    pub opaque_ty_sizes: HashMap<String, u64>,
    /// Fresh-id allocators — see `MirIdCounters`'s own doc comment.
    pub id_counters: MirIdCounters,
}

impl MirPackage {
    /// Replaces (or inserts) the `MirCodeUnit` produced for `def_id` — the
    /// only way this package's `units` map is ever written, so re-lowering
    /// one item after a comptime value resolves is always this exact call
    /// with a fresh unit, never a partial in-place edit. Also (re)records
    /// every function item's signature into `sigs` — including methods nested
    /// in an impl unit, whose `DefId` does not equal the unit's top-level
    /// owner. This is the incremental counterpart to a whole-module
    /// predeclare sweep and is required for cross-package method calls.
    pub fn insert_unit(&mut self, def_id: DefId, unit: MirCodeUnit) {
        for item in &unit.items {
            if let ItemKind::Function(func) = &item.kind {
                if let Some(function_def_id) = &func.def_id {
                    self.sigs.insert(function_def_id.clone(), func.clone());
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
        self.units
            .values_mut()
            .flat_map(|unit| unit.bodies.iter_mut())
    }

    /// Looks up a body by `BodyId` across all units, since a package's bodies
    /// aren't otherwise addressable without knowing which unit produced them.
    pub fn body(&self, id: super::BodyId) -> Option<&super::Body> {
        self.units.values().find_map(|unit| unit.bodies.get(&id))
    }

    /// Mutable counterpart to `body()`.
    pub fn body_mut(&mut self, id: super::BodyId) -> Option<&mut super::Body> {
        self.units
            .values_mut()
            .find_map(|unit| unit.bodies.get_mut(&id))
    }

    /// Mints and returns the next `MirId`, advancing the counter.
    pub fn fresh_mir_id(&mut self) -> MirId {
        let id = self.id_counters.next_mir_id;
        self.id_counters.next_mir_id += 1;
        id
    }

    /// Mints and returns the next `BodyId` index, advancing the counter.
    pub fn fresh_body_id(&mut self) -> u32 {
        let id = self.id_counters.next_body_id;
        self.id_counters.next_body_id += 1;
        id
    }

    /// Mints and returns the next error-guarantee index, advancing the counter.
    pub fn fresh_error_id(&mut self) -> u32 {
        let id = self.id_counters.next_error_id;
        self.id_counters.next_error_id += 1;
        id
    }

    /// Mints and returns the next synthetic `DefId` (opaque types, etc.).
    pub fn fresh_synthetic_def_id(&mut self) -> DefId {
        let id = self.id_counters.next_synthetic_def_id.clone();
        self.id_counters.next_synthetic_def_id = self
            .id_counters
            .next_synthetic_def_id
            .clone()
            .saturating_add(1);
        id
    }

    /// Mints and returns the next synthetic HIR-level `DefId`.
    pub fn fresh_synthetic_hir_def_id(&mut self) -> DefId {
        let id = self.id_counters.next_synthetic_hir_def_id.clone();
        self.id_counters.next_synthetic_hir_def_id = self
            .id_counters
            .next_synthetic_hir_def_id
            .clone()
            .saturating_add(1);
        id
    }

    /// Re-seeds the synthetic HIR `DefId` counter (see `HirToMirLowerer`'s
    /// callers, which seed it past the package's own real `DefId`s).
    pub fn set_next_synthetic_hir_def_id(&mut self, id: DefId) {
        self.id_counters.next_synthetic_hir_def_id = id;
    }

    pub fn span(&self) -> super::Span {
        super::Span::union(self.items().map(super::Item::span))
    }

    pub fn extend_full_layouts(
        &mut self,
        entries: impl IntoIterator<Item = ((DefId, Vec<Ty>), Vec<Ty>)>,
    ) {
        self.full_layouts.extend(entries);
    }

    pub fn extend_opaque_payload_sizes(
        &mut self,
        entries: impl IntoIterator<Item = (String, u64)>,
    ) {
        self.opaque_payload_sizes.extend(entries);
    }

    pub fn extend_adt_defs(
        &mut self,
        entries: impl IntoIterator<Item = (crate::hir::DefId, ty::AdtDef)>,
    ) {
        self.adt_defs.extend(entries);
    }

    /// Merges `other` (typically `HirToMirLowerer::take_mir_package`'s
    /// output, once a lowering call finishes) into `self` field-by-field —
    /// the one thing that lets a struct/enum layout, method table entry, or
    /// const value computed during one lowering call survive past it, for
    /// reuse the next time this package is (re-)lowered. `units`/`sigs`
    /// extend the same way as every other field here: harmless, since
    /// `HirToMirLowerer` never writes its own `units`/`sigs` (those are only
    /// ever populated by `CompilerState::insert_mir_unit`'s `insert_unit`
    /// call, on the session's own persistent `MirPackage`, not on the
    /// lowering instance's private one).
    pub fn extend_from(&mut self, other: MirPackage) {
        self.units.extend(other.units);
        self.runtime_support
            .items
            .extend(other.runtime_support.items);
        self.runtime_support
            .bodies
            .extend(other.runtime_support.bodies);
        self.full_layouts.extend(other.full_layouts);
        self.opaque_payload_sizes.extend(other.opaque_payload_sizes);
        self.adt_defs.extend(other.adt_defs);
        self.sigs.extend(other.sigs);
        self.struct_defs.extend(other.struct_defs);
        self.struct_defs_by_tail_name
            .extend(other.struct_defs_by_tail_name);
        self.struct_layouts.extend(other.struct_layouts);
        self.struct_layouts_by_ty.extend(other.struct_layouts_by_ty);
        self.structural_defs.extend(other.structural_defs);
        self.enum_defs.extend(other.enum_defs);
        self.enum_defs_by_name.extend(other.enum_defs_by_name);
        self.enum_layouts.extend(other.enum_layouts);
        self.enum_layouts_by_ty.extend(other.enum_layouts_by_ty);
        self.enum_variants.extend(other.enum_variants);
        self.enum_variant_names.extend(other.enum_variant_names);
        self.const_values.extend(other.const_values);
        self.executable_consts.extend(other.executable_consts);
        self.function_sigs.extend(other.function_sigs);
        self.generic_function_defs
            .extend(other.generic_function_defs);
        for (name, methods) in other.struct_methods {
            self.struct_methods.entry(name).or_default().extend(methods);
        }
        self.method_name_output_consensus
            .extend(other.method_name_output_consensus);
        self.method_lookup_by_def.extend(other.method_lookup_by_def);
        self.method_lookup.extend(other.method_lookup);
        self.method_defs.extend(other.method_defs);
        self.method_defs_by_def.extend(other.method_defs_by_def);
        self.method_hir_defs.extend(other.method_hir_defs);
        self.method_defs_by_self_and_name
            .extend(other.method_defs_by_self_and_name);
        self.method_specializations
            .extend(other.method_specializations);
        self.function_specializations
            .extend(other.function_specializations);
        self.function_specialization_call_cache
            .extend(other.function_specialization_call_cache);
        self.method_specialization_call_cache
            .extend(other.method_specialization_call_cache);
        self.opaque_types.extend(other.opaque_types);
        self.opaque_ty_sizes.extend(other.opaque_ty_sizes);
    }
}
