# Modules in FerroPhase

The FerroPhase toolchain treats *modules* as the canonical unit of
namespacing, compilation, and binding generation. A module corresponds to one
or more source files under a package and is preserved all the way through the
typed AST pipeline, optimization passes, and language-specific backends. This
document captures the rules for defining, resolving, and consuming modules
across the multi-language ecosystem.

## Core Concepts

- **Module Path** – A double-colon (`::`) separated namespace anchored at the
  package root: `physics::solvers::newton`. Paths are case-sensitive and map to
  directory hierarchies inside `src/`.
- **Primary Source** – The canonical FerroPhase file for a module. For
  `physics::solvers::newton`, the primary source lives at
  `src/physics/solvers/newton.fp`.
- **Bindings** – Optional façades emitted for other languages. Generated files
  mirror the module path, e.g. `bindings/typescript/physics/solvers/newton.ts`
  or `bindings/python/physics/solvers/newton.py`.
- **Crate Scope** – `crate::` refers to the current package; `super::` climbs up
  the module hierarchy; absolute paths begin with the package name.

## Directory Layout

```
my_package/
├── Ferrophase.toml
├── src/
│   ├── lib.fp                # optional root module shim
│   └── physics/
│       ├── mod.fp            # re-exports submodules
│       └── solvers/
│           ├── mod.fp
│           └── newton.fp     # physics::solvers::newton
└── bindings/
    ├── typescript/
    │   └── physics/solvers/newton.ts
    └── python/
        └── physics/solvers/newton.py
```

- `mod.fp` files collect and re-export sibling modules; they are optional when
  a directory only hosts a single leaf module.
- Packages may expose a `src/lib.fp` as the root module when they provide a
  library-style API. Binary targets declare entrypoints elsewhere (see
  `Packages.md`).

## Declaring Modules

Inside a `mod.fp`, list child modules explicitly:

```ferro
pub mod solvers;
pub use solvers::newton::step;
```

Each `mod` statement causes the toolchain to load `solvers/mod.fp` (if present)
and all leaf `.fp` files underneath. Re-exports (`pub use`) produce typed
aliases in the AST so other packages can import `physics::step` directly.

## Import Rules

- `use crate::physics::solvers::newton::step;`
- `use package_name::physics::solvers::*;`
- Relative paths: `use super::solvers::broyden;`

Imports are resolved during the AST normalisation stage. The resolver consults
the package manifest (`Ferrophase.toml`) to ensure the dependency graph permits
cross-package references and records feature requirements for each edge.

### Glob Imports

`use foo::bar::*;` expands to the set of public items defined in the referenced
module at type-check time. The expansion is cached per feature set so const
evaluation and transpiled outputs stay in sync.

### Qualified Names in Const Eval

During const evaluation the interpreter stores results under their fully
qualified module path (e.g. `physics::solvers::newton::STEP_SIZE`). This allows
other modules – including bindings – to access results without worrying about
evaluation order.

## Module Metadata

The typed AST retains per-module metadata:

- `module_id`: stable identifier used by dependency graphs and caches.
- `features`: required feature flags to compile or import the module.
- `targets`: list of language backends that must generate bindings for the
  module (derived from the manifest).
- `doc`: pulled from the leading doc comments.

This metadata is embedded into generated bindings (e.g. TypeScript `/** */`
headers) and emitted alongside diagnostics.

### In-Memory Representation

Tooling that operates on packages in memory uses a `ModuleDescriptor` struct to
capture the same concepts exposed in the source tree:

```rust
pub struct ModuleDescriptor {
    pub id: ModuleId,
    pub package: PackageId,
    pub path: VirtualPath,
    pub module_path: Vec<String>,
    pub language: ModuleLanguage,    // Ferro, Rust, TypeScript, Python, ...
    pub exports: Vec<SymbolDescriptor>,
    pub requires_features: Vec<FeatureRef>,
}
```

- `path` points at the canonical source file inside the virtual filesystem.
- `module_path` mirrors the namespace segments (e.g. `physics`, `solvers`,
  `newton`).
- `language` indicates which backend generated the module (useful when treating
  Rust crates and FerroPhase sources uniformly).
- `exports` enumerate public symbols; providers may enrich them with signatures
  or docstrings.

Providers such as `RustModuleProvider` or the FerroPhase module loader populate
these descriptors from the virtual filesystem, allowing language servers and
build tools to reason about modules without touching the real disk.

## Interplay With Language Bindings

- TypeScript/JavaScript bindings mirror module paths and export the same public
  API. Tree-shaking works because each FerroPhase module maps to its own ES
  module.
- Python bindings create packages (`__init__.py`) reflecting the module tree.
  Public members become Python functions/classes with type hints derived from
  the FerroPhase signature.
- Rust bindings expose modules under `bindings::rust::physics::solvers`. Users
  can `use bindings::rust::physics::solvers::newton;` after adding the generated
  crate as a dependency.

## Cross-Package Modules

To consume another package’s module:

```toml
[dependencies]
ferro-math = "1.2"

[targets.transpile]
languages = ["typescript", "python"]
```

```ferro
use ferro_math::matrix::linalg::solve;
```

The resolver ensures the dependency supports the selected targets; the
transpiler only generates bindings for modules declared as public.

### Magnet-Aware Layouts

When working inside a Magnet super-workspace the directory structure typically
looks like:

```
Magnet.toml
crates/
  ├── ferro-physics/
  │   ├── Cargo.toml        # generated by magnet
  │   ├── Ferrophase.toml   # authored by you
  │   └── src/...
  └── ferro-render/
      └── ...
```

- Magnet orchestrates Rust crate metadata; FerroPhase modules remain under the
  same `src/` tree.
- Module paths still resolve via the package name (`ferro-physics::...`). The
  CLI derives the package name from `Ferrophase.toml` rather than the enclosing
  Cargo manifest, so FerroPhase imports remain stable even if Magnet rewrites
  Cargo metadata.
- Generated bindings (e.g. `bindings/typescript/...`) can coexist with Magnet’s
  output. Add them to `.gitignore` or commit them depending on your deployment
  strategy.

## Module Initialisation

- `const` items evaluated at compile time populate module-level constants before
  downstream compilation or transpilation.
- `static` items remain runtime initialised; the interpreter records their
  default expressions for backends that need them (e.g. Python defers
  evaluation until import).
- Modules can register intrinsic implementations through `#[intrinsic]` blocks;
  the intrinsic registry uses the module path as the namespacing key.

## Best Practices

- Keep module boundaries small and cohesive; they become re-exportable units for
  other languages.
- Use `mod.rs`-style aggregators (`mod.fp`) to curate public APIs instead of
  re-exporting entire directory trees.
- Document modules with leading doc comments – generated bindings surface them
  in language-appropriate formats.
- Prefer `pub(crate)` for package-internal APIs so accidental exports do not
  leak into bindings.
- Ensure const evaluation of module-level items has no side effects; results are
  cached and shared across targets.

Modules are the backbone of the multi-language story: design them carefully and
the rest of the pipeline – const evaluation, optimization, and bindings – will
slot neatly into place.
