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
- `src/` – FerroPhase modules (see `Modules.md`).
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

Internally the toolchain represents each package with an immutable snapshot so
higher layers can reason about metadata without repeatedly reading manifests:

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

- `manifest_path` and `root` live inside the virtual filesystem layer so package
  providers work with overlays or generated trees.
- `modules` stores the module descriptors collected for this package (see
  `Modules.md`).
- `dependencies` captures the normalized dependency graph, including feature
  edges and target filters, ready for resolvers or registries.

Providers such as `CargoPackageProvider` populate these structs by parsing
`Cargo.toml`, `Ferrophase.toml`, or registry manifests and reading sources via
the virtual filesystem abstraction.

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

Projects managed by the Magnet super-workspace can layer FerroPhase packages
alongside existing Cargo crates:

1. Magnet owns the outer workspace definition (`Magnet.toml`) and is
   responsible for generating `Cargo.toml` manifests via `magnet generate`.
2. FerroPhase packages live inside Magnet members (e.g. `crates/my_lib/`). Each
   retains its own `Ferrophase.toml` describing multi-language targets.
3. Run Magnet first to ensure Cargo manifests are up-to-date, then invoke
   `fp workspace build` (or `fp build`) to produce FerroPhase artefacts.
4. Keep FerroPhase lockfiles (`Ferrophase.lock`) alongside Magnet’s Cargo
   metadata; both should be committed to source control to guarantee reproducible
   builds.
5. When adding a new package, register it in both `Magnet.toml` (for Rust
   dependency wiring) and `Ferrophase.workspace.toml` (for multi-language
   tracking).

Magnet’s path-based dependency rewrites complement FerroPhase’s own manifest
system: use Magnet to govern Rust crates, while FerroPhase manifests govern
transpiled outputs and cross-language bindings.

## Dependency Resolution & Lockfiles

- Dependencies are resolved against the default FerroPhase registry unless a
  `[registry]` override is present.
- `Ferrophase.lock` records exact versions, checksums, and supported targets per
  dependency.
- Target-specific builds (e.g. `fp build --target python`) prune dependencies
  that opt out of that language.
- The resolver rejects circular package dependencies and enforces semver
  compatibility across all targets.

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

1. Initialize the repository with your package manager (e.g. `magnet init` and
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
