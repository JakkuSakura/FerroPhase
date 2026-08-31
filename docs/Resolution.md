# Name Resolution in fp-backend

This document describes the name-resolution architecture used by FerroPhase,
how it compares to rustc's `rustc_resolve`, and the migration target. Name
resolution is an AST-stage operation: macro expansion runs first, then an
explicit AST resolver assigns identities, and only then does AST lower to HIR.

## Current design (migration baseline)

The resolver is `HirGenerator`
(`crates/fp-backend/src/transforms/ast_to_hir/mod.rs`). It is already
module-aware and namespace-aware, and already returns an enum per lookup —
matching the intent of "module-aware, namespaced, enum-returning" resolution.
Concretely:

- **Two namespaces** (type, value): `type_scopes` / `value_scopes` (lexical,
  block/module scope stacks) and namespace-indexed bindings in `ModuleTree`.
  This is transitional; the target replaces these lowerer-owned maps with
  `LocalScope` and a single symbol map on each `ModuleTree` node.
- **Current-module tracking**: `module_path` plus `with_module_scope`
  (mod.rs:2168) push/pop a path stack around `predeclare_items` and
  `append_item`, so every resolution and visibility check
  (`can_access`, `SymbolExport::Scoped`) knows what module it's running in.
  This is the equivalent of rustc's per-namespace `Rib` stack.
- **Resolution result enum**: `hir::Res` (`fp-core/src/hir/mod.rs:720`) —
  `Def(DefId) | Local(HirId) | SelfTy | Module(Vec<String>) | Builtin(...)`.
  Close to rustc's `Res<Id>` (`Def(DefKind, DefId) | PrimTy | SelfTyParam |
  SelfTyAlias | SelfCtor | Local(Id) | ToolMod | NonMacroAttr | Err`).
- **Modules as a tree**: `ModuleTree` stores parent/child relationships and
  namespace bindings, matching rustc's `ModuleData` shape and allowing direct
  child lookup.
- **Import resolution as fixpoint**: `resolve_pending_imports` (mod.rs:2003)
  collects all `use`/re-export bindings via
  `collect_pending_imports_recursive` (recursing into inline modules), then
  loops calling `register_import_binding` until a full sweep makes no
  progress — the same shape as rustc's indeterminate-import worklist, needed
  to resolve multi-hop `pub use` chains and glob imports whose target module
  isn't fully known yet.
- **Macro expansion is currently global**: `expand_item_macros` and the
  intrinsic normalizer still use a package-global macro-name map. This is a
  known migration gap; the target stores specialized macro bindings in the
  same symbol map as ordinary declarations and resolves them with module
  context.

## Comparison with rustc

| Concept | rustc (`rustc_resolve`) | fp-backend today |
|---|---|---|
| Module representation | Tree of `ModuleData` (parent + children) | `ModuleTree` with parent/child nodes and qualified-path index |
| Namespaces | Type, Value, Macro | Type, Value today; Macro is being migrated into the shared symbol map |
| Per-lookup result | `Res<Id>` enum | `hir::Res` enum (near-equivalent shape) |
| Current-scope tracking | `Rib` stack per namespace | `module_path` + lowerer-owned type/value stacks (transitional) |
| Import resolution | Fixpoint loop over indeterminate imports | Fixpoint loop in `resolve_pending_imports` / `register_import_binding` |

## Target architecture

The target follows rustc's phase boundary and separates persistent module
state from temporary local state:

```text
parse → macro expansion → AstResolver → AST→HIR lowering → type checking
                         │
                         ├── ModuleTree (persistent package state)
                         └── LocalScope (temporary function/block state)
```

`HirPackage` owns one persistent `ModuleTree`. The existing tree remains the
only module-scope owner; no `ModuleScope` wrapper is introduced. Each existing
tree node stores one ordinary symbol map:

```rust
symbols: HashMap<Symbol, Vec<Binding>>
```

The vector retains all candidates for a spelling. Each `Binding` carries its
namespace (`Type`, `Value`, or `Macro`) and semantic identity. Type/value
bindings therefore share one key space without sharing a collision domain;
macros are represented by a specialized `Binding::Macro(MacroBinding)`
variant, not a second macro hashmap.

`LocalScope` is the explicit counterpart for names whose lifetime is limited
to a function or block:

```rust
struct LocalScope {
    nodes: Vec<LocalScopeNode>,
    current: LocalScopeId,
}

struct LocalScopeNode {
    parent: Option<LocalScopeId>,
    symbols: HashMap<Symbol, Vec<Binding>>,
}
```

It owns parameters, locals, generic parameters, and block-local items. It is
never persisted in the package's module tree.

`AstResolver` owns the active `LocalScope`, borrows the package's
`ModuleTree`, and records AST-node resolutions. `AstToHirLowerer` consumes
those results; it does not perform lexical lookup, module lookup, import
fallback, suffix scans, or alias-name recovery.

Declaration and resolution behavior is configured, not stored, by
language-specific `DeclarationRules` and `ResolutionRules`. The active
`PackageProvider` supplies these values for the package's source language
(alongside its intrinsic normalizer); a composite provider delegates them to
its workspace/source provider. The scope structures own the `declare`,
`resolve`, and `resolve_path` operations and do not choose a language policy
themselves. Ambiguity is represented by retained candidates and returned as
an explicit `ResolutionResult`, never by insertion-order selection.

Aliases are ordinary type bindings with alias metadata and a resolved
`DefId`/`Res`; dedicated alias hashmaps are not part of the target design.

The migration baseline still has compatibility tables for transparent
type-alias payloads and performs macro lookup during expansion; both are
removed by the target architecture above. Import diagnostics now retain the
originating `use` span. `ModuleTree` now
retains an explicit ambiguity marker instead of overwriting competing
bindings; lookups refuse to select a winner and callers recover with an
error. Cross-package associated-item recovery uses a prebuilt,
namespace-qualified leaf index with its own ambiguity markers; it no longer
scans every package's exports or `def_paths` at query time. Transparent type
aliases have HIR definition identities, while enum constructor paths follow
the alias target to preserve the defining variant's `DefId`.

## Foundation landed: `ModuleTree`, `hir::Package`, `hir::Program`

The building blocks for the target design below now exist in `fp-core`,
are consumed by `HirGenerator`'s lookup and import paths; this section
records what remains to be migrated.

- **`ModuleTree`** (`fp-core/src/hir/resolve.rs`) — a real tree of nodes,
  each with `parent`, `children: HashMap<String, ModuleId>`, namespace-indexed
  bindings, and ambiguity markers indexed by the new `Namespace::{Type,
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
package filtered by qualified-path prefix; alias payloads remain in the
lowerer's compatibility table, while `type_alias_children` provides a direct
parent-module index for glob expansion. `lookup_symbol`/
`tree_lookup_raw` are the surviving single entry points for a qualified-
or bare-name key lookup, replacing the old `lookup_symbol(key, &flat_map)`
signature with `lookup_symbol(key, namespace)` against the tree.

**Existing non-resolver migration work**: `HirGenerator` still builds `def_paths`/`op_defs`/
`intrinsic_defs`/`type_alias_targets`/`placeholder_defs` as private scratch
fields and merges them into the final `hir::Package` at `transform_package`'s
several return points (`program.X.extend(self.X.clone())`) — the
mirror/extend step this whole migration was meant to eliminate is still
there internally, just now merging into a `Package` instead of the old
`Program`. And `fp-typing`'s `path_ty`/`hir_to_ast`'s `HirToAstLifter` still
hold a `Package` directly rather than querying `hir::Program`'s resolution
methods (`def_path`, `type_alias_target`, `resolve`, ...).

## Migration steps

1. Add `Binding`, specialized `MacroBinding`, `LocalScope`, namespace-aware
   `ResolutionResult`, and immutable per-language `DeclarationRules`/
   `ResolutionRules` configuration types in `fp-core`. Extend
   `PackageProvider` with rule accessors; composite providers delegate to the
   workspace provider.
2. Replace the existing per-namespace binding maps with one
   `HashMap<Symbol, Vec<Binding>>` directly on each existing `ModuleTree` node.
   Do not add a `ModuleScope` wrapper or a separate macro hashmap. Move
   module-owned alias payloads and lookup metadata into tree/package-owned
   declaration records.
3. Implement an explicit `AstResolver` in the AST→HIR subsystem. It performs
   declaration collection, import fixed-point resolution, local-scope
   resolution, macro lookup, path traversal, privacy checks, and records an
   AST resolution table before HIR lowering begins.
4. Remove lexical/module lookup logic and resolution maps from
   `AstToHirLowerer`; make it consume resolver results while retaining only
   HIR-construction state and unrelated lowering caches.
5. Update `fp-lang`/`fp-rust` macro interfaces so macro expansion receives
   module context and resolves specialized macro bindings from the shared
   symbol map.
6. Update type checking and HIR recovery to consume resolved `Res`/`DefId`
   identities rather than retrying name lookup.
7. Add unit and integration coverage for namespace coexistence, module/type
   conflicts, local shadowing, import ambiguity, macro isolation, aliases,
   explicit path prefixes, and every resolver consumer.
