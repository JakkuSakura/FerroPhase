use std::collections::{BTreeSet, HashMap, VecDeque};
use std::fmt::{self, Display};
use std::hash::{Hash, Hasher};
use std::io::Read;
use std::path::{Component, Path, PathBuf};
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use std::time::SystemTime;

/// Normalised UTF-8 path representation decoupled from the host OS.
#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct VirtualPath {
    segments: Vec<String>,
    absolute: bool,
}

impl VirtualPath {
    pub fn new_absolute<I, S>(segments: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        Self::new_internal(true, segments)
    }

    pub fn new_relative<I, S>(segments: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        Self::new_internal(false, segments)
    }

    fn new_internal<I, S>(absolute: bool, segments: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        let mut normalised: Vec<String> = Vec::new();
        for segment in segments {
            let raw = segment.into();
            if raw.is_empty() || raw == "." {
                continue;
            }
            if raw == ".." {
                if !normalised.is_empty() {
                    normalised.pop();
                }
            } else {
                normalised.push(raw);
            }
        }
        Self {
            segments: normalised,
            absolute,
        }
    }

    pub fn is_absolute(&self) -> bool {
        self.absolute
    }

    pub fn is_root(&self) -> bool {
        self.absolute && self.segments.is_empty()
    }

    pub fn segments(&self) -> &[String] {
        &self.segments
    }

    pub fn join<S>(&self, segment: S) -> Self
    where
        S: Into<String>,
    {
        let mut cloned = self.segments.clone();
        cloned.push(segment.into());
        Self::new_internal(self.absolute, cloned)
    }

    pub fn with_segments<I, S>(&self, segments: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        Self::new_internal(self.absolute, segments)
    }

    pub fn parent(&self) -> Option<Self> {
        if self.segments.is_empty() {
            return if self.absolute {
                Some(Self::new_internal(true, Vec::<String>::new()))
            } else {
                None
            };
        }
        let mut segments = self.segments.clone();
        segments.pop();
        Some(Self::new_internal(self.absolute, segments))
    }

    pub fn to_path_buf(&self) -> PathBuf {
        let mut buf = if self.absolute {
            PathBuf::from("/")
        } else {
            PathBuf::new()
        };
        for segment in &self.segments {
            buf.push(segment);
        }
        buf
    }

    pub fn from_path<P: AsRef<Path>>(path: P) -> Self {
        let path = path.as_ref();
        let absolute = path.is_absolute();
        let mut segments = Vec::new();
        for component in path.components() {
            match component {
                Component::RootDir => continue,
                Component::CurDir => {}
                Component::ParentDir => {
                    if !segments.is_empty() {
                        segments.pop();
                    }
                }
                Component::Normal(segment) => segments.push(segment.to_string_lossy().to_string()),
                _ => {}
            }
        }
        Self { segments, absolute }
    }
}

impl FromStr for VirtualPath {
    type Err = (); // Parsing never fails; invalid segments are ignored.

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let absolute = s.starts_with('/') || s.starts_with('\\');
        let trimmed = s.trim_matches(|c| c == '/' || c == '\\');
        if trimmed.is_empty() {
            return Ok(Self {
                segments: Vec::new(),
                absolute,
            });
        }
        let segments = trimmed
            .split(|c| c == '/' || c == '\\')
            .map(|segment| segment.trim())
            .filter(|segment| !segment.is_empty())
            .map(|segment| segment.to_string())
            .collect::<Vec<_>>();
        Ok(Self { segments, absolute })
    }
}

impl Display for VirtualPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.absolute {
            write!(f, "/")?;
        }
        let mut first = true;
        for segment in &self.segments {
            if !first {
                write!(f, "/")?;
            }
            first = false;
            write!(f, "{}", segment)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct FileMetadata {
    pub kind: FileKind,
    pub size: u64,
    pub modified: Option<SystemTime>,
    pub hash: Option<u64>,
    pub readonly: bool,
}

impl FileMetadata {
    fn for_file(contents: &[u8]) -> Self {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        contents.hash(&mut hasher);
        Self {
            kind: FileKind::File,
            size: contents.len() as u64,
            modified: Some(SystemTime::now()),
            hash: Some(hasher.finish()),
            readonly: false,
        }
    }

    fn for_directory() -> Self {
        Self {
            kind: FileKind::Directory,
            size: 0,
            modified: Some(SystemTime::now()),
            hash: None,
            readonly: false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FileKind {
    File,
    Directory,
    Symlink,
}

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

#[derive(Clone, Debug)]
pub struct DirEntry {
    pub path: VirtualPath,
    pub metadata: FileMetadata,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FsEvent {
    Created(VirtualPath),
    Modified(VirtualPath),
    Removed(VirtualPath),
}

pub type WatchSender = std::sync::mpsc::Sender<FsEvent>;

pub struct WatchGuard {
    _private: (),
}

impl WatchGuard {
    pub fn new() -> Self {
        Self { _private: () }
    }
}

pub type FsResult<T> = std::result::Result<T, FsError>;

pub trait VirtualFileSystem: Send + Sync {
    fn read(&self, path: &VirtualPath) -> FsResult<Vec<u8>>;
    fn write(&self, path: &VirtualPath, contents: &[u8]) -> FsResult<()>;
    fn metadata(&self, path: &VirtualPath) -> FsResult<FileMetadata>;
    fn remove(&self, path: &VirtualPath) -> FsResult<()>;
    fn read_dir(&self, path: &VirtualPath) -> FsResult<Vec<DirEntry>>;
    fn exists(&self, path: &VirtualPath) -> bool;
    fn mkdirp(&self, path: &VirtualPath) -> FsResult<()>;

    fn watch(&self, _path: &VirtualPath, _sender: WatchSender) -> FsResult<WatchGuard> {
        Err(FsError::Unsupported)
    }
}

// -----------------------------------------------------------------------------
// In-memory filesystem
// -----------------------------------------------------------------------------

#[derive(Clone)]
pub struct InMemoryFileSystem {
    root: Arc<Mutex<HashMap<VirtualPath, FsNode>>>,
}

#[derive(Clone, Debug)]
enum FsNode {
    File {
        contents: Vec<u8>,
        metadata: FileMetadata,
    },
    Directory {
        metadata: FileMetadata,
    },
}

impl InMemoryFileSystem {
    pub fn new() -> Self {
        let mut map = HashMap::new();
        map.insert(
            VirtualPath::new_absolute(Vec::<String>::new()),
            FsNode::Directory {
                metadata: FileMetadata::for_directory(),
            },
        );
        Self {
            root: Arc::new(Mutex::new(map)),
        }
    }

    fn ensure_parent_exists(&self, path: &VirtualPath) -> FsResult<()> {
        if let Some(parent) = path.parent() {
            if !self.exists(&parent) {
                self.mkdirp(&parent)?;
            }
        }
        Ok(())
    }
}

impl Default for InMemoryFileSystem {
    fn default() -> Self {
        Self::new()
    }
}

impl VirtualFileSystem for InMemoryFileSystem {
    fn read(&self, path: &VirtualPath) -> FsResult<Vec<u8>> {
        let guard = match self.root.lock() {
            Ok(g) => g,
            Err(poison) => poison.into_inner(),
        };
        match guard.get(path) {
            Some(FsNode::File { contents, .. }) => Ok(contents.clone()),
            Some(FsNode::Directory { .. }) => Err(FsError::Io(std::io::Error::new(
                std::io::ErrorKind::Other,
                "cannot read directory",
            ))),
            None => Err(FsError::NotFound(path.clone())),
        }
    }

    fn write(&self, path: &VirtualPath, contents: &[u8]) -> FsResult<()> {
        self.ensure_parent_exists(path)?;
        let mut guard = match self.root.lock() {
            Ok(g) => g,
            Err(poison) => poison.into_inner(),
        };
        guard.insert(
            path.clone(),
            FsNode::File {
                contents: contents.to_vec(),
                metadata: FileMetadata::for_file(contents),
            },
        );
        Ok(())
    }

    fn metadata(&self, path: &VirtualPath) -> FsResult<FileMetadata> {
        let guard = match self.root.lock() {
            Ok(g) => g,
            Err(poison) => poison.into_inner(),
        };
        match guard.get(path) {
            Some(FsNode::File { metadata, .. }) | Some(FsNode::Directory { metadata, .. }) => {
                Ok(metadata.clone())
            }
            None => Err(FsError::NotFound(path.clone())),
        }
    }

    fn remove(&self, path: &VirtualPath) -> FsResult<()> {
        let mut guard = match self.root.lock() {
            Ok(g) => g,
            Err(poison) => poison.into_inner(),
        };
        if !guard.contains_key(path) {
            return Err(FsError::NotFound(path.clone()));
        }
        let keys_to_remove: Vec<VirtualPath> = guard
            .keys()
            .filter(|candidate| is_descendant(path, candidate))
            .cloned()
            .collect();
        for key in keys_to_remove {
            guard.remove(&key);
        }
        Ok(())
    }

    fn read_dir(&self, path: &VirtualPath) -> FsResult<Vec<DirEntry>> {
        let guard = match self.root.lock() {
            Ok(g) => g,
            Err(poison) => poison.into_inner(),
        };
        if !guard.contains_key(path) {
            return Err(FsError::NotFound(path.clone()));
        }
        let mut entries = BTreeSet::new();
        for candidate in guard.keys() {
            if let Some(parent) = candidate.parent() {
                if &parent == path {
                    entries.insert(candidate.clone());
                }
            }
        }
        let mut result = Vec::new();
        for entry_path in entries {
            if let Some(node) = guard.get(&entry_path) {
                let metadata = match node {
                    FsNode::File { metadata, .. } | FsNode::Directory { metadata, .. } => {
                        metadata.clone()
                    }
                };
                result.push(DirEntry {
                    path: entry_path,
                    metadata,
                });
            }
        }
        Ok(result)
    }

    fn exists(&self, path: &VirtualPath) -> bool {
        let guard = match self.root.lock() {
            Ok(g) => g,
            Err(poison) => poison.into_inner(),
        };
        guard.contains_key(path)
    }

    fn mkdirp(&self, path: &VirtualPath) -> FsResult<()> {
        let mut guard = match self.root.lock() {
            Ok(g) => g,
            Err(poison) => poison.into_inner(),
        };
        let mut segments = VecDeque::from(path.segments.clone());
        let mut current = if path.absolute {
            VirtualPath::new_absolute(Vec::<String>::new())
        } else {
            VirtualPath::new_relative(Vec::<String>::new())
        };
        guard
            .entry(current.clone())
            .or_insert_with(|| FsNode::Directory {
                metadata: FileMetadata::for_directory(),
            });
        while let Some(segment) = segments.pop_front() {
            current = current.join(segment);
            guard
                .entry(current.clone())
                .or_insert_with(|| FsNode::Directory {
                    metadata: FileMetadata::for_directory(),
                });
        }
        Ok(())
    }
}

fn is_descendant(parent: &VirtualPath, candidate: &VirtualPath) -> bool {
    if parent == candidate {
        return true;
    }
    if parent.segments().len() >= candidate.segments().len() {
        return false;
    }
    candidate.segments().starts_with(parent.segments())
}

// -----------------------------------------------------------------------------
// Unix filesystem implementation
// -----------------------------------------------------------------------------

pub struct UnixFileSystem {
    root: PathBuf,
}

impl UnixFileSystem {
    pub fn new<P: Into<PathBuf>>(root: P) -> Self {
        Self { root: root.into() }
    }

    fn to_native_path(&self, path: &VirtualPath) -> PathBuf {
        let mut buf = self.root.clone();
        for segment in path.segments() {
            buf.push(segment);
        }
        buf
    }
}

impl VirtualFileSystem for UnixFileSystem {
    fn read(&self, path: &VirtualPath) -> FsResult<Vec<u8>> {
        let native = self.to_native_path(path);
        let mut file = std::fs::File::open(&native)?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)?;
        Ok(buffer)
    }

    fn write(&self, path: &VirtualPath, contents: &[u8]) -> FsResult<()> {
        let native = self.to_native_path(path);
        if let Some(parent) = native.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(native, contents)?;
        Ok(())
    }

    fn metadata(&self, path: &VirtualPath) -> FsResult<FileMetadata> {
        let native = self.to_native_path(path);
        let metadata = std::fs::metadata(&native)?;
        let kind = if metadata.is_dir() {
            FileKind::Directory
        } else if metadata.is_file() {
            FileKind::File
        } else {
            FileKind::Symlink
        };
        Ok(FileMetadata {
            kind,
            size: metadata.len(),
            modified: metadata.modified().ok(),
            hash: None,
            readonly: metadata.permissions().readonly(),
        })
    }

    fn remove(&self, path: &VirtualPath) -> FsResult<()> {
        let native = self.to_native_path(path);
        if native.is_dir() {
            std::fs::remove_dir_all(native)?;
        } else {
            std::fs::remove_file(native)?;
        }
        Ok(())
    }

    fn read_dir(&self, path: &VirtualPath) -> FsResult<Vec<DirEntry>> {
        let native = self.to_native_path(path);
        let mut entries = Vec::new();
        for entry in std::fs::read_dir(native)? {
            let entry = entry?;
            let entry_path = entry.path();
            let metadata = entry.metadata()?;
            let kind = if metadata.is_dir() {
                FileKind::Directory
            } else if metadata.is_file() {
                FileKind::File
            } else {
                FileKind::Symlink
            };
            let mut virtual_segments = path.segments.clone();
            if let Some(name) = entry_path.file_name() {
                virtual_segments.push(name.to_string_lossy().to_string());
            }
            entries.push(DirEntry {
                path: VirtualPath::new_internal(path.absolute, virtual_segments),
                metadata: FileMetadata {
                    kind,
                    size: metadata.len(),
                    modified: metadata.modified().ok(),
                    hash: None,
                    readonly: metadata.permissions().readonly(),
                },
            });
        }
        Ok(entries)
    }

    fn exists(&self, path: &VirtualPath) -> bool {
        self.to_native_path(path).exists()
    }

    fn mkdirp(&self, path: &VirtualPath) -> FsResult<()> {
        let native = self.to_native_path(path);
        std::fs::create_dir_all(native)?;
        Ok(())
    }
}
