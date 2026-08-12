# Packages and Modules in FerroPhase

This document describes the actual in-memory data model the compiler uses —
`PackageProvider`, `PackageDescriptor`/`PackageSource`, `ModuleDescriptor`,
`PackageItem` — as found in `fp-core/src/package/` and `fp-core/src/module/`.

## Package is the real compilation unit

A **package** is one crate/project — the embedded `std`, a Cargo workspace
member, a `.fp`-native project, etc. It's identified by a `PackageId` (a
plain string wrapper) and described by two structs:

```rust
pub struct PackageDescriptor {
    pub id: PackageId,
    pub name: String,
    pub version: Option<Version>,
    pub manifest_path: VirtualPath,
    pub root: VirtualPath,
    pub metadata: PackageMetadata,   // dependencies, features, ...
    pub modules: Vec<ModuleId>,
}

/// What a `PackageProvider` actually returns for `load_package_source`.
pub struct PackageSource {
    pub package_id: PackageId,
    pub name: String,
    pub graph: PackageGraph,
    pub module_paths: HashSet<QualifiedPath>,
    pub items: Vec<PackageItem>,     // the real payload
}
```

Packages are discovered and parsed by implementations of `PackageProvider`
(`fp-core/src/package/provider.rs`):

```rust
pub trait PackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>>;
    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>>;
    fn refresh(&self) -> ProviderResult<()>;
    fn load_package_source(&self, id: &PackageId) -> ProviderResult<PackageSource>;
}
```

Each source language/layout has its own provider:

- `fp_lang::cargo_provider::CargoWorkspaceProvider` — discovers a Cargo/Magnet
  workspace's member crates and parses every source file in each with
  `FerroFrontend` (the `.fp`-and-Rust-superset parser). This is what
  `magnet transpile`/`fp compile <dir>` actually uses today for directory inputs.
- `fp_lang::provider::FerroPhaseProvider` — serves the embedded `std`/`libc`
  packages (baked into the `fp-lang` binary from `.fp` source at build time;
  see `fp-lang/build.rs` / `embedded_std.rs`).
- `fp_rust::RustPackageProvider` — planned/in-progress provider specifically
  for real `.rs` Cargo projects, with its own `std` backed by real rustc
  source (see `docs/RustStd.md`). Not wired into language detection yet.

`WorkspaceContext::provider_for(package_id)` picks whichever registered
provider's `list_packages()` includes the requested ID — there's no separate
routing table; providers self-report what they own.

## Module is a per-source-file grouping key, not a nested-namespace feature

This is the part worth being explicit about, since "module" strongly suggests
Rust's own `mod`/`pub mod` system. It's a **different, smaller thing** here:

```rust
pub struct ModuleDescriptor {
    pub id: ModuleId,                 // stable string key, e.g. a path key
    pub package: PackageId,           // owning package
    pub language: ModuleLanguage,     // Ferro, Rust, TypeScript, Python, Other(_)
    pub module_path: Vec<String>,     // e.g. ["repo_backend"] for repo_backend.rs
    pub source: VirtualPath,          // the file it came from
    pub exports: Vec<SymbolDescriptor>,
    pub requires_features: Vec<FeatureRef>,
}
```

A `ModuleDescriptor` exists mainly to answer "which source file (and which
language) did this group of items come from" for bookkeeping/diagnostics
purposes (`ModuleLanguage` is used, e.g., to treat Rust crates and embedded
`.fp` std uniformly). It is **not** a nested namespace with its own
declaration syntax at the compiler-infrastructure level — providers construct
one `ModuleDescriptor` per source file (see `CargoWorkspaceProvider::load_package_source`,
which maps each file's relative path to a `QualifiedPath` via
`module_path_from_relative`), not per `pub mod` block.

The actual fine-grained unit the compiler operates on below the module level
is `PackageItem`:

```rust
pub struct PackageItem {
    pub path: QualifiedPath,   // which module (source file) this came from
    pub item: Item,            // one top-level AST item: a fn/struct/enum/impl/...
}
```

`PackageSource::items: Vec<PackageItem>` is a flat list of every top-level
item across every file in the package, each tagged with its originating
module path. Normalization, (optional) typechecking, and serialization all
operate over this flat list — grouped back by `path` where a pass needs
per-file context (e.g. the Kotlin serializer's `serialize_package` groups by
`path.segments.join("/")` to emit one `.kt` file per source module).

### `pub mod` / `use` — a real language feature, separate concern

`.fp` source files do have `pub mod foo;` / `use foo::bar;` syntax (parsed by
`FerroFrontend`), and that's a genuine nested-namespace language feature —
but it's resolved during parsing/normalization into flat items with qualified
names, not preserved as a `ModuleDescriptor` tree. Don't conflate the two: a
`.fp` file with three `pub mod` blocks inside it is still exactly *one*
`ModuleDescriptor` (one source file) containing however many `PackageItem`s
its parsed items flatten into.
