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

**Not yet done** (this is the actual remaining gap, superseding the old
"Target design" below): `HirGenerator` still keeps its own flat
`module_defs`/`global_type_defs`/`global_value_defs`/`prelude_type_defs`/
`prelude_value_defs`/`crate_roots` fields internally and hasn't been
rewired to write into a `ModuleTree` (via `hir::Package`/`hir::Program`)
instead — the flat-map resolution logic itself is unchanged, only the
*storage shape* for holding a finished package's HIR content has moved.
Similarly, `HirGenerator` still builds `def_paths`/`op_defs`/
`intrinsic_defs`/`type_alias_targets`/`placeholder_defs` as private scratch
fields and merges them into the final `hir::Package` at `transform_package`'s
several return points (`program.X.extend(self.X.clone())`) — the
mirror/extend step this whole migration was meant to eliminate is still
there internally, just now merging into a `Package` instead of the old
`Program`.

## Target design (still future work)

1. **Rewire `HirGenerator` to hold `program: hir::Program` +
   `current_package: PackageId`, writing directly into
   `self.program.packages[&self.current_package]` throughout** — no private
   scratch fields, no mirror/extend step. `module_defs`/`global_type_defs`/
   `global_value_defs`/`prelude_type_defs`/`prelude_value_defs`/`crate_roots`
   are replaced by calls into that package's own `module_tree`:
   `expand_glob_import` becomes "list bindings at this tree node" instead of
   a scan-and-filter over flat maps (`module_tree.children(...)`); a
   sub-crate root (this session's `crate_roots` addition) becomes just a
   child of the crate-root node, not a separate table;
   `resolve_module_path_through_aliases`/`register_import_binding`'s
   per-segment `module_defs.contains` checks become `module_tree.child(...)`
   descents.
2. **Namespace enum stays two-valued** (already true — `Namespace::{Type,
   Value}` exists, no macro namespace, since macros are already gone by
   resolution time).
3. **`hir::Res` stays as-is** — already close enough to rustc's `Res<Id>`.
4. **`fp-typing`'s `path_ty` and `hir_to_ast`'s `HirToAstLifter` should query
   `hir::Program`'s resolution methods** (`def_path`, `type_alias_target`,
   ...) instead of indexing a package's fields directly, once they're
   threaded a `Program` (today they hold a `Package` directly, inherited
   unchanged from before this rename).
5. **Import fixpoint loop keeps its current shape** (`resolve_pending_imports`,
   `register_import_binding`); only the underlying storage changes from flat
   maps to tree-node bindings.

A migration (when undertaken) would migrate one `HirGenerator` consumer at a
time (predeclare → imports → glob-expansion → visibility checks) against the
now-real `ModuleTree`/`Package`/`Program` types, then delete the flat maps
once every consumer reads from the tree instead.
