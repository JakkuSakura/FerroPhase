# Name Resolution in fp-backend

This document describes the current name resolution architecture used when
lowering AST to HIR (`crates/fp-backend/src/transforms/ast_to_hir/`), how it
compares to rustc's `rustc_resolve`, and a target design for closing the one
structural gap identified.

## Current design

The resolver is `HirGenerator`
(`crates/fp-backend/src/transforms/ast_to_hir/mod.rs`). It is already
module-aware and namespace-aware, and already returns an enum per lookup —
matching the intent of "module-aware, namespaced, enum-returning" resolution.
Concretely:

- **Two namespaces** (type, value): `type_scopes` / `value_scopes` (lexical,
  block/module scope stacks) and `global_type_defs` / `global_value_defs`
  (flat maps keyed by fully-qualified path strings). This mirrors rustc's
  `Namespace::{Type, Value, Macro}`, minus the macro namespace — see below.
- **Current-module tracking**: `module_path` plus `with_module_scope`
  (mod.rs:2168) push/pop a path stack around `predeclare_items` and
  `append_item`, so every resolution and visibility check
  (`can_access`, `SymbolExport::Scoped`) knows what module it's running in.
  This is the equivalent of rustc's per-namespace `Rib` stack.
- **Resolution result enum**: `hir::Res` (`fp-core/src/hir/mod.rs:720`) —
  `Def(DefId) | Local(HirId) | SelfTy | Module(Vec<String>) | Builtin(...)`.
  Close to rustc's `Res<Id>` (`Def(DefKind, DefId) | PrimTy | SelfTyParam |
  SelfTyAlias | SelfCtor | Local(Id) | ToolMod | NonMacroAttr | Err`).
- **Modules as flat path sets**: `module_defs: HashSet<QualifiedPath>` records
  which module paths exist, but there is no tree of `Module` nodes with
  parent/child pointers — unlike rustc's `ModuleData` tree.
- **Import resolution as fixpoint**: `resolve_pending_imports` (mod.rs:2003)
  collects all `use`/re-export bindings via
  `collect_pending_imports_recursive` (recursing into inline modules), then
  loops calling `register_import_binding` until a full sweep makes no
  progress — the same shape as rustc's indeterminate-import worklist, needed
  to resolve multi-hop `pub use` chains and glob imports whose target module
  isn't fully known yet.
- **No macro namespace**: `expand_item_macros` expands item-position
  `macro_rules!` invocations into concrete items *before* any
  definition/import pass runs, so macros never exist as an entity that later
  resolution needs to look up. This is a deliberate simplification, not a gap.

## Comparison with rustc

| Concept | rustc (`rustc_resolve`) | fp-backend today |
|---|---|---|
| Module representation | Tree of `ModuleData` (parent + children) | Flat `HashSet<QualifiedPath>` (`module_defs`) |
| Namespaces | Type, Value, Macro | Type, Value (macros expanded away pre-resolution) |
| Per-lookup result | `Res<Id>` enum | `hir::Res` enum (near-equivalent shape) |
| Current-scope tracking | `Rib` stack per namespace | `module_path` + `with_module_scope` push/pop |
| Import resolution | Fixpoint loop over indeterminate imports | Fixpoint loop in `resolve_pending_imports` / `register_import_binding` |

The one structural gap is the module tree: because modules are a flat set of
path strings rather than tree nodes, operations like "what's a direct child
of module M" (used by glob-import expansion, `expand_glob_import`, mod.rs:416)
require scanning and filtering the flat `global_type_defs`/`global_value_defs`
maps by path prefix, instead of a direct child lookup on a tree node.

## Foundation landed: `ModuleTree`, `hir::Package`, `hir::Program`

The building blocks for the target design below now exist in `fp-core`,
**not yet consumed by `HirGenerator`'s own internals** — this section
records what's real today versus what's still ahead.

- **`ModuleTree`** (`fp-core/src/hir/resolve.rs`) — a real tree of nodes,
  each with `parent`, `children: HashMap<String, ModuleId>`, and
  `bindings: [HashMap<String, Res>; 2]` indexed by the new `Namespace::{Type,
  Value}` enum. Operations (`ensure_module`, `module_exists`, `child`,
  `bind`/`lookup`, `children`) are all O(depth) or O(1) — no full-map scan.
  Own unit tests cover construction/lookup/child-listing in isolation.
- **`hir::Package`** — what this document used to call `hir::Program`
  (`items`, `def_map`, `def_paths`, `op_defs`, `intrinsic_defs`,
  `type_alias_targets`, `placeholder_defs`), renamed and given `id:
  PackageId` and `module_tree: ModuleTree` fields. One per compiled package.
- **`hir::Program`** — now a genuinely new type: `packages:
  HashMap<PackageId, Package>`, the whole multi-package compiled result,
  with resolution methods (`def_path`, `type_alias_target`, `item`,
  `op_def`, `intrinsic_def`, `is_placeholder_def`, `resolve`) that dispatch
  to the right package via a `DefId`'s own `package_id` — a caller asks
  the whole program a question rather than indexing a package's fields
  directly.

**Fold-in landed**: `ModuleTree`'s bindings now carry the full
`hir::SymbolEntry` shape (`res`/`export`/`path`, moved from `fp-backend`
into `fp-core::hir::resolve` alongside `ModuleTree`), so `HirGenerator`'s
former `global_type_defs`/`global_value_defs`/`prelude_type_defs`/
`prelude_value_defs`/`crate_roots` flat maps are gone entirely — every
value/type binding now lives directly on its owning module's tree node
(`record_value_symbol`/`record_type_symbol`/`record_value_path` all go
through a shared `bind_symbol` helper), and the package-scoped bare-name
prelude fallback lives on a reserved `ModuleTree::prelude()` node instead
of its own pair of maps. `expand_glob_import` now lists a module's own
value/type bindings via `ModuleTree::bindings(module, ns)` — a direct
per-node lookup — instead of a scan over every global definition in the
package filtered by qualified-path prefix; only `type_aliases` (a
`type X = Y;` table with no per-entry visibility, deliberately left out of
this migration) still needs that kind of scan. `lookup_symbol`/
`tree_lookup_raw` are the surviving single entry points for a qualified-
or bare-name key lookup, replacing the old `lookup_symbol(key, &flat_map)`
signature with `lookup_symbol(key, namespace)` against the tree.

**Still not done**: `HirGenerator` still builds `def_paths`/`op_defs`/
`intrinsic_defs`/`type_alias_targets`/`placeholder_defs` as private scratch
fields and merges them into the final `hir::Package` at `transform_package`'s
several return points (`program.X.extend(self.X.clone())`) — the
mirror/extend step this whole migration was meant to eliminate is still
there internally, just now merging into a `Package` instead of the old
`Program`. And `fp-typing`'s `path_ty`/`hir_to_ast`'s `HirToAstLifter` still
hold a `Package` directly rather than querying `hir::Program`'s resolution
methods (`def_path`, `type_alias_target`, `resolve`, ...).

## Target design (still future work)

1. **Rewire `HirGenerator` to hold `program: hir::Program` +
   `current_package: PackageId`, writing directly into
   `self.program.packages[&self.current_package]` throughout** — no private
   scratch fields (`def_paths`/`op_defs`/`intrinsic_defs`/
   `type_alias_targets`/`placeholder_defs`), no mirror/extend step at
   `transform_package`'s return points.
2. **Namespace enum stays two-valued** (already true — `Namespace::{Type,
   Value}` exists, no macro namespace, since macros are already gone by
   resolution time).
3. **`hir::Res` stays as-is** — already close enough to rustc's `Res<Id>`.
4. **`fp-typing`'s `path_ty` and `hir_to_ast`'s `HirToAstLifter` should query
   `hir::Program`'s resolution methods** (`def_path`, `type_alias_target`,
   ...) instead of indexing a package's fields directly, once they're
   threaded a `Program` (today they hold a `Package` directly).
5. **Import fixpoint loop keeps its current shape** (`resolve_pending_imports`,
   `register_import_binding`) — already reading/writing the tree, unaffected
   by this remaining step.
