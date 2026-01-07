# Packages and Workspaces

Packages define distribution, dependency management, and multi-language build
targets for FerroPhase projects. They wrap module trees, generated bindings, and
metadata that the toolchain uses to assemble reproducible builds across the
Rust, TypeScript/JavaScript, Python, and C/LLVM ecosystems. This guide explains
how packages are structured, how manifests are authored, and how the pipeline
interacts with them.

## Anatomy of a Package

```
awesome-lib/
├── Ferrophase.toml
├── src/
│   ├── lib.fp
│   └── math/
│       ├── mod.fp
│       └── vector.fp
├── bindings/
│   ├── typescript/
│   └── python/
├── tests/
│   ├── compile/
│   └── runtime/
└── target/               # build artefacts (generated)
```

- `Ferrophase.toml` – package manifest (see below).
- `src/` – FerroPhase modules (see `Modules.md`). The fp compiler does not scan
  the filesystem directly; module discovery is provided by the package graph
  produced by Magnet.
- `bindings/` – generated or hand-authored language bindings.
- `tests/` – language-aware tests; subdirectories mirror `targets`.
- `target/` – build outputs, caches, and transpiled artefacts. The CLI manages
  this directory.

## Manifest Overview

`Ferrophase.toml` is a TOML document split into logical sections:

```toml
[package]
name = "awesome-lib"
version = "0.3.0"
edition = "2024"
authors = ["ACME Labs <dev@acme.test>"]
license = "MIT"
description = "Vector utilities shared across backends"

[targets]
default = ["ferro", "typescript", "python"]

[targets.ferro]
kind = "library"

[targets.typescript]
kind = "transpile"
module_root = "bindings/typescript"
emit = "esm"

[targets.python]
kind = "transpile"
module_root = "bindings/python"

[dependencies]
ferro-math = { version = "^2.1", features = ["linalg"], targets = ["ferro", "typescript"] }

[features]
default = ["std"]
std = []
serde = ["ferro-math/serde"]

[build]
toolchain = "nightly-2024-08-15"
const_eval = { enable = true, allow_io = false }
optimization = "speed"

[bindings.typescript]
package = "@acme/awesome-lib"
types = "dist/index.d.ts"
publish = true

[bindings.python]
package = "acme-awesome-lib"
entry = "dist/__init__.py"
publish = true
```

### Key Sections

- `[package]` – identity and metadata. Edition controls language features.
- `[targets]` – declares which backends to build. Each sub-table may set kind
  (`library`, `binary`, `ffi`), entrypoints, or emission formats.
- `[dependencies]` – semver-based requirements. Optional fields:
  - `features` – feature flags to enable on the dependency.
  - `targets` – restricts the dependency to specific targets (e.g. skip when
    transpiling to Python).
  - `package` – rename the dependency within the package namespace.
- `[features]` – feature flag graph. Values list other features or dependency
  flags to enable.
- `[build]` – pipeline configuration: toolchain pinning, const-eval policy,
  optimisation mode, codegen flags.
- `[bindings.<lang>]` – metadata used when publishing to external registries.

### In-Memory Package Model

Internally the toolchain represents each package with an immutable snapshot.
Magnet provides this package graph to fp; fp does not load manifests or scan
files directly:

```rust
pub struct PackageDescriptor {
    pub id: PackageId,
    pub name: String,
    pub version: Version,
    pub manifest_path: VirtualPath,
    pub root: VirtualPath,
    pub metadata: PackageMetadata,
    pub modules: Vec<ModuleDescriptor>,
}

pub struct PackageMetadata {
    pub edition: Option<String>,
    pub authors: Vec<String>,
    pub description: Option<String>,
    pub license: Option<String>,
    pub keywords: Vec<String>,
    pub registry: Option<String>,
    pub features: BTreeMap<String, Vec<FeatureRef>>,
    pub dependencies: Vec<DependencyDescriptor>,
}
```

- `manifest_path` and `root` live inside the virtual filesystem layer so the
  package graph can be generated from overlays or build outputs.
- `modules` stores module descriptors collected by Magnet. fp treats this as
  authoritative input and never enumerates the filesystem.
- `dependencies` captures the normalized dependency graph, including feature
  edges and target filters.

Magnet is responsible for producing this graph; fp only consumes it.

### Package Graph Contract

Magnet emits a package graph that fp consumes at runtime:

- Package identity (name, version, features, dependencies) is resolved by
  Magnet.
- Module descriptors include `module_path`, `language`, and `source`. The
  `module_path` is canonical and language-agnostic.
- The graph is immutable for a compilation run; fp caches it for resolution.

fp does not attempt to infer module trees or read manifests. It relies entirely
on the graph for correctness and reproducibility.

### Language-Specific Resolution

fp resolves modules and symbols via a language-specific strategy:

- **Rust-like (FerroPhase)**: `crate::`, `self::`, `super::`, `use` trees, and
  Rust visibility rules.
- **Python**: dotted module paths, `from x import y`, optional `*` imports, and
  runtime-only dynamic imports.
- **TypeScript**: module specifiers (package/path) mapped to `module_path` by
  Magnet, with default/named exports.

Each strategy maps imports to `ModuleId` and resolves symbols within a module.
The shared fp pipeline only orchestrates resolution and diagnostics.

## Workspaces

A workspace coordinates multiple packages with a shared lockfile:

```toml
# FerroPhase.workspace.toml
[workspace]
members = ["crates/*", "tools/cli"]

[workspace.metadata]
toolchain = "nightly-2024-08-15"
```

- Each member includes its own `Ferrophase.toml`.
- Running `fp workspace build` resolves dependencies once and builds all
  packages with the shared toolchain.
- `fp workspace publish` can push a consistent set of versions (useful for mono
  repos).

### Magnet Integration

Magnet owns nexus/workspace/package management and emits the package graph that
fp consumes:

1. Magnet owns the outer workspace definition (`Magnet.toml`) and package
   manifests (including cross-language bindings).
2. Magnet emits the normalized package graph (packages, modules, dependencies).
3. fp consumes the graph and performs language-specific module resolution at
   runtime. It does not scan files or interpret manifests itself.
4. Keep lockfiles (`Ferrophase.lock`) alongside Magnet metadata for
   reproducibility.

## Dependency Resolution & Lockfiles

- Dependencies are resolved by Magnet and serialized into the package graph.
- `Ferrophase.lock` records exact versions, checksums, and supported targets per
  dependency, as produced by Magnet.
- Target-specific builds prune dependencies via target filters already captured
  in the graph.
- fp assumes the graph is consistent (no cycles, compatible versions).

## Build Pipeline

1. **Frontend** – parse FerroPhase sources into the canonical AST.
2. **Type Inference** – annotate modules with principal types.
3. **Const Evaluation** – execute compile-time code, caching results per module
   and feature set.
4. **Optimization & Codegen** – lower to HIR/MIR/LIR and emit backend artefacts
   (LLVM IR, transpiled sources, FFI shims).
5. **Binding Generation** – materialise language-specific packages under
   `bindings/`.

The CLI stores intermediate artefacts under `target/<lang>/` so repeated builds
reuse previous work when inputs, feature sets, and toolchain versions match.

## Publishing

- `fp package publish` uploads the manifest, lockfile, and compiled artefacts to
  a registry.
- Language bindings flagged with `publish = true` can automatically trigger
  npm/PyPI/crates.io releases using generated package manifests.
- Packages can be yanked by publishing the same version with `yanked = true` in
  `[package.metadata]`.

### Package Manager Strategy

FerroPhase intentionally mirrors the Cargo model: the package manager (Magnet or
an equivalent standalone CLI) orchestrates dependency resolution, registry
interaction, and workspace coordination, while the FerroPhase compiler focuses on
AST processing, const evaluation, and code generation. Keeping the package
manager separate provides several advantages:

1. **Isolation of responsibilities** – registry credentials, lockfile semantics,
   and publishing policies evolve independently of the compiler/runtime.
2. **Tooling interoperability** – CI pipelines, IDEs, and build systems can talk
   to a stable CLI surface (Magnet) without embedding compiler internals.
3. **Versioning flexibility** – teams can pin package-manager versions (for
   reproducibility or policy) while adopting newer compilers as features land.
4. **Multi-ecosystem bridging** – Magnet already harmonises Cargo workspaces; the
   same CLI can coordinate FerroPhase manifests, npm/PyPI bindings, and future
   language targets from a single entry point.

Projects that need a self-contained runtime can embed the compiler and pull in
dependencies directly, but for the general workflow we recommend continuing with
the Cargo-style separation: Magnet (or a successor) remains the package manager,
and FerroPhase tooling integrates with it via manifests, lockfiles, and CLI
commands.

## Versioning Strategy

- Semantic versioning: breaking API changes bump the major version.
- Transpiled bindings inherit the same version as the FerroPhase package.
- `Ferrophase.lock` pins exact versions; CI pipelines should check it in to
  guarantee reproducible builds.

## Developer Workflow

1. Initialize the repository with your package manager (e.g. `magnet manifest init` and
   updating `Magnet.toml`). Create an initial `Ferrophase.toml` manually or via a
   Magnet plugin.
2. Implement modules under `src/`.
3. Regenerate workspace manifests via the package manager as required (for
   Magnet, `magnet generate`).
4. Run `fp build` to compile FerroPhase artefacts and generate bindings.
5. Execute tests: `fp test --all` or per-language (`--lang typescript`).
6. Update `Ferrophase.toml` (and matching package-manager manifests) to adjust
   targets, dependencies, or features.
7. Publish using the package manager (`magnet publish`, `cargo publish`, etc.)
   alongside `fp package publish` when distributing FerroPhase artefacts.

## Best Practices

- Keep manifests declarative; avoid custom build scripts until absolutely
  necessary.
- Leverage features to gate expensive const-eval paths or optional bindings.
- Use workspaces for mono-repos: they shorten build times by sharing caches and
  lockfiles.
- Document supported targets in the README and expose CI badges per language.
- Run `fp audit` regularly to verify dependency integrity and license
  compliance.

Packages provide the contract between FerroPhase’s typed AST world and the
language ecosystems you target. A well-authored manifest and clean module tree
make multi-language distribution straightforward.
