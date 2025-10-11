# Virtual Filesystem & Package/Module Providers

This document proposes an in-memory architecture that models files, packages,
and language modules without tying the higher-level tooling to the physical
filesystem. The design introduces trait-based abstractions so we can plug in
different backends (Unix filesystem, remote stores, in-memory fixtures) and
connect them to package/module providers for Cargo/Rust and FerroPhase.

## 1. Virtual Filesystem (VFS)

### Goals

- All consumers operate on `VirtualPath` objects instead of raw OS paths.
- Support read/write operations, directory traversal, metadata queries, and
  watch notifications entirely in memory.
- Allow incremental views of the filesystem (e.g., overlay a generated tree on
  top of an existing snapshot).

### Core Traits

```rust
/// Normalized UTF-8 path wrapper. Internally stores segments to avoid
/// platform-specific separators.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct VirtualPath(Vec<String>);

/// Pseudo file metadata used across all backends.
pub struct FileMetadata {
    pub kind: FileKind,          // File, Directory, Symlink
    pub size: u64,
    pub modified: Option<DateTime<Utc>>,
    pub hash: Option<u64>,       // Optional content hash for caching
    pub readonly: bool,
}

pub enum FileKind { File, Directory, Symlink }

pub trait VirtualFileSystem: Send + Sync {
    fn read(&self, path: &VirtualPath) -> Result<Vec<u8>, FsError>;
    fn write(&self, path: &VirtualPath, contents: &[u8]) -> Result<(), FsError>;
    fn metadata(&self, path: &VirtualPath) -> Result<FileMetadata, FsError>;
    fn remove(&self, path: &VirtualPath) -> Result<(), FsError>;
    fn read_dir(&self, path: &VirtualPath) -> Result<Vec<DirEntry>, FsError>;
    fn exists(&self, path: &VirtualPath) -> bool;
    fn mkdirp(&self, path: &VirtualPath) -> Result<(), FsError>;

    /// Optional: subscribe to changes. Implementations that cannot watch return
    /// `FsError::Unsupported`.
    fn watch(&self, path: &VirtualPath, tx: WatchSender) -> Result<WatchGuard, FsError>;
}

pub struct DirEntry {
    pub path: VirtualPath,
    pub metadata: FileMetadata,
}

pub type WatchSender = crossbeam_channel::Sender<FsEvent>;
pub enum FsEvent { Created(VirtualPath), Modified(VirtualPath), Removed(VirtualPath) }

#[derive(Debug, thiserror::Error)]
pub enum FsError {
    #[error("{0} not found")]
    NotFound(VirtualPath),
    #[error("permission denied for {0}")]
    PermissionDenied(VirtualPath),
    #[error("operation unsupported")]
    Unsupported,
    #[error(transparent)]
    Io(#[from] std::io::Error),
}
```

### In-Memory Implementation

`InMemoryFileSystem` stores nodes in a `HashMap<VirtualPath, FsNode>` where
`FsNode` tracks contents and metadata. Writes update the map directly; reads and
directory traversals leverage path prefix searches. Useful for tests and
generated overlays.

Features:

- Copy-on-write snapshots for branching builds.
- Optional parent pointer to allow layered overlays (e.g., base snapshot + temp
  build products).
- Version counter to coordinate invalidation with higher-level caches.

### Unix Implementation

`UnixFileSystem` bridges the VFS traits to the host filesystem. It converts
`VirtualPath` to `PathBuf`, delegates to std::fs, and maps IO errors to
`FsError`. Watching integrates with `notify` or `inotify` depending on
platform.

```rust
pub struct UnixFileSystem {
    root: PathBuf,
}

impl VirtualFileSystem for UnixFileSystem {
    fn read(&self, path: &VirtualPath) -> Result<Vec<u8>, FsError> { /* ... */ }
    // ... remaining methods ...
}
```

## 2. Package & Module Structs

All package metadata lives in memory, sourced from manifests/providers.

See `docs/Packages.md` for the full `PackageDescriptor` structure and
`docs/Modules.md` for `ModuleDescriptor`. Both rely on the shared VFS and are
populated by providers described below.

## 3. Provider Traits

Providers translate external formats into in-memory packages/modules using the
shared VFS.

```rust
pub trait PackageProvider {
    fn list_packages(&self) -> Result<Vec<PackageId>, ProviderError>;
    fn load_package(&self, id: &PackageId) -> Result<Arc<PackageDescriptor>, ProviderError>;
    fn refresh(&self) -> Result<(), ProviderError>;
}

pub trait ModuleProvider {
    fn modules_for_package(&self, id: &PackageId) -> Result<Vec<ModuleId>, ProviderError>;
    fn load_module(&self, id: &ModuleId) -> Result<Arc<ModuleDescriptor>, ProviderError>;
}

#[derive(Debug, thiserror::Error)]
pub enum ProviderError {
    #[error("package not found: {0}")]
    PackageNotFound(PackageId),
    #[error("module not found: {0}")]
    ModuleNotFound(ModuleId),
    #[error("invalid manifest: {path} ({message})")]
    InvalidManifest { path: VirtualPath, message: String },
    #[error(transparent)]
    Fs(#[from] FsError),
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}
```

### CargoPackageProvider

- Reads workspace metadata via `cargo metadata` or by parsing `Cargo.toml`.
- Uses the VFS to read manifests, normalizing paths into `VirtualPath`.
- Fills `PackageDescriptor` with Cargo-specific metadata; dependencies record
  features, target filters, and path overrides.
- Supports `refresh()` by re-running metadata when `Cargo.toml` or lockfiles
  change.

### RustModuleProvider

- Consumes packages from `CargoPackageProvider` (or any provider returning Rust
  sources) and scans module trees under `src/`.
- Builds `ModuleDescriptor` objects representing Rust modules; `module_path`
  mirrors Rust’s namespace (`crate::foo::bar`).
- Uses the VFS to read files (for e.g., doc comments, symbol extraction).
- Can cache syntax trees or signatures for later consumption (e.g., FerroPhase
  bridging).

### UnixFileSystem

Already described above; integrates with both providers so they can operate on
real project directories.

## 4. Putting It Together

```
        ┌───────────────────────────────────┐
        │        VirtualFileSystem          │
        │ ┌───────────────────────────────┐ │
        │ │ InMemoryFS   UnixFS (root=.) │ │
        │ └───────────────────────────────┘ │
        └───────────────────────────────────┘
                      │
                      ▼
        ┌───────────────────────────────────┐
        │         PackageProvider           │
        │ ┌───────────────────────────────┐ │
        │ │  CargoPackageProvider        │ │
        │ └───────────────────────────────┘ │
        └───────────────────────────────────┘
                      │
                      ▼
        ┌───────────────────────────────────┐
        │          ModuleProvider           │
        │ ┌───────────────────────────────┐ │
        │ │   RustModuleProvider         │ │
        │ └───────────────────────────────┘ │
        └───────────────────────────────────┘
```

Higher layers (FerroPhase package manager, transpilers, language servers) depend
only on the VFS plus provider traits. Tests can inject an `InMemoryFileSystem`
with synthetic manifests, while production uses `UnixFileSystem` combined with
Cargo adapters.

## Future Extensions

- **OverlayFS**: compose multiple VFS instances to support generated build
  trees or remote caches.
- **Remote Providers**: implement `PackageProvider` backed by a registry API,
  caching manifests locally via the VFS.
- **Language-Agnostic Module Providers**: add providers for FerroPhase modules,
  TypeScript bindings, etc., all sharing the same `ModuleDescriptor` schema.
- **Incremental Update Stream**: expose diff events from providers so tooling
  can update UI state without a full reload.

This design keeps the core data structures immutable, enabling cheap snapshot
sharing and straightforward synchronization between editors, build tools, and
CLI commands.
