# Name Resolution

FerroPhase resolves names while transforming AST into HIR. Parsing supplies
syntax and source spans; the resolver assigns semantic identities, and HIR
stores the resolved result. Later stages consume `Res`, `DefId`, and `HirId`
and must not repeat source-name lookup.

```text
parse -> AST -> resolver/AST-to-HIR -> HIR -> type checking -> MIR
                   |                   |
                   |                   +-- Res / DefId / HirId
                   +-- declarations, imports, and local scopes
```

## Ownership

`fp-resolve` owns the resolution algorithms and temporary work state:

- `Resolver` coordinates resolution of one AST package.
- `AstResolver` collects AST declarations and imports while assigning HIR
  identities from the destination `HirPackage`.
- `ResolutionWorklist` owns unresolved import directives until they resolve or
  the worklist reaches quiescence.
- `LocalScope` owns parameters, locals, generic parameters, and block-local
  declarations during resolution.

`fp-core::hir::resolve` owns the stable data types shared with later stages:

- `ModuleTree`
- `Binding`
- `Namespace`
- `DeclarationRules` and `ResolutionRules`
- `DeclarationOutcome` and `ResolutionResult`

`HirPackage` owns its `ModuleTree`. `AstPackage` owns only parsed modules and
other source data. `HirProgram` provides package-directed lookup APIs so a
consumer starts from a `PackageId` or `DefId` instead of scanning packages.

The active `PackageProvider` supplies `DeclarationRules` and
`ResolutionRules`. The rule types are immutable language configuration; they
do not own declarations or execute resolution.

## Module and local storage

Each nested `ModuleTree` value has one ordinary symbol map:

```rust
struct ModuleTree {
    symbols: HashMap<Symbol, Vec<Binding>>,
    children: HashMap<Symbol, ModuleTree>,
}
```

The key is the compiler's common `Symbol`. The vector retains every candidate
with that spelling, allowing declaration and resolution rules to distinguish
valid namespace overlap from a same-namespace conflict. There is no
`ModuleId`, `ModuleNode`, `ModuleScope`, global module index, or dedicated
type-alias map.

`Binding` describes what a declaration means. Its variants cover modules,
definitions, imports, aliases, enum variants, associated items, extern crates,
builtins, locals, parameters, generics, macros, and errors. A binding carries
its namespace and semantic target. An alias is therefore resolved through its
binding to a target `DefId`; consumers do not use an alias-specific lookup API.

Macros use `Binding::Macro` and the `Macro` namespace. They remain isolated
from type and value conflicts without requiring another symbol table.

`LocalScope` is a separate nested scope tree whose nodes use the same
`HashMap<Symbol, Vec<Binding>>` shape. It is temporary resolver state and is
not stored on `AstProgram`, `AstPackage`, or `AstToHirLowerer`.

## Identity and paths

`DefId` is the semantic identity of a definition. `HirId` identifies HIR-local
nodes such as local bindings. `Res` is the result attached to a resolved use;
module results also carry a module definition's `DefId`.

There is no FerroPhase `DefPath` identity. `HirPackage::source_paths` is an
optional map from `DefId` to `InPackagePath` used only for diagnostics and
source re-emission. A path must never be used to reconstruct, compare, or fake
a semantic identity. Cross-package queries dispatch through the package stored
in the `DefId`.

Definition IDs are allocated by the destination `HirPackage` while each AST
declaration is collected. Index zero remains reserved for the package root.
The resolver records the allocated ID directly in the module binding, and the
lowered HIR item uses that same ID.

## Lookup

Ordinary lookup follows these boundaries:

1. Search `LocalScope` from the current scope toward its parents.
2. Search the current module's candidates in the requested namespace.
3. Search parent modules only when `ResolutionRules` permits it.
4. Resolve explicit imports and reachable extern-prelude packages.
5. Consult configured language prelude modules when enabled.
6. Return `NotFound` or `Ambiguous`; never perform package-wide suffix scans.

Path lookup resolves the first segment in the active module context. Every
intermediate segment must resolve to `Res::Module(DefId)`; the module tree maps
that identity to the nested module used for the next segment. A value/type
lookup uses `resolve_path_final`, which rejects a module as the terminal result.

Type and value declarations may share a spelling when the language rules allow
it. Same-namespace duplicates remain multiple candidates and resolve as
ambiguous. Macro candidates are always considered only in the macro namespace.

## Imports and the unified worklist

The resolver first collects declarations, then converts every import leaf into
an `ImportDirective`. Both single imports and globs enter one
`ResolutionWorklist`; aliases and re-exports are not handled by separate retry
tables.

Resolving one directive may add a binding that makes another directive
resolvable. Deferred directives return to the same queue after progress. When
a complete pass makes no progress, the worklist is quiescent and the remaining
directives are unresolved errors. Import spans stay on bindings/directives for
diagnostics, and competing candidates are retained instead of being selected
by insertion order.

This corresponds to rustc's indeterminate-import processing: imports are
resolved incrementally as module resolutions become determined, with explicit
ambiguity rather than a package-global search.

## Comparison with rustc

| Concept | rustc | FerroPhase |
|---|---|---|
| Persistent module state | `ModuleData` tree and name resolutions | Nested `ModuleTree` owned by `HirPackage` |
| Local scopes | Namespace-specific ribs | `LocalScope` nodes with namespace-tagged bindings |
| Semantic result | `Res<Id>` / `DefId` | `Res` / `DefId` / `HirId` |
| Namespaces | Type, Value, Macro | Type, Value, Macro |
| Imports | Indeterminate-import work queue | Unified `ResolutionWorklist` |
| Language behavior | Resolver policy and edition features | Provider-supplied declaration/resolution rules |
| Diagnostic names | `DefPath` derived from definitions | Optional `DefId -> InPackagePath` source metadata |

rustc does have an internal `DefPath`, but it is derived metadata used for
stable hashes, symbol names, diagnostics, and incremental compilation. It is
not rustc's name-resolution result and does not replace `DefId`. FerroPhase
currently has no need for an equivalent stable-hashing identity, so it keeps
only `DefId` for semantics and source paths for presentation.

## Remaining work

- Complete local declaration/use collection through `LocalScope`; no lexical
  state should remain in AST-to-HIR lowering.
- Resolve extern-prelude packages through `HirProgram` and declared dependency
  edges rather than any session-wide package scan.
- Make prelude module population and lookup package-aware, including preludes
  supplied by dependency packages.
- Report every quiescent import directive and ambiguous lookup through the
  diagnostic channel using its parser-provided span.
- Remove lowerer fallbacks that retry unresolved names or synthesize semantic
  identities from strings.
- Port resolver tests for namespace coexistence, shadowing, explicit path
  prefixes, re-export chains, glob ambiguity, macro isolation, and privacy.
