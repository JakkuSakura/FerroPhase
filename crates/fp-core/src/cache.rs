//! Small, process-independent cache storage shared by compiler providers.
//!
//! The cache deliberately stores opaque bytes.  Serialization belongs to the
//! provider or compiler stage that owns an artifact; this module only owns
//! identity, validation, atomic publication, and filesystem layout.

use std::fs;
use std::hash::{Hash, Hasher};
use std::io;
use std::path::{Path, PathBuf};

const MAGIC: &[u8] = b"FP-CACHE\0";
const FORMAT_VERSION: u8 = 1;

#[derive(Clone, Debug)]
pub struct DiskCache {
    root: PathBuf,
}

impl DiskCache {
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self { root: root.into() }
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    /// Reads an artifact. Missing, corrupt, or incompatible entries are cache
    /// misses; they never turn a successful compilation into a failure.
    pub fn get(&self, key: &str) -> io::Result<Option<Vec<u8>>> {
        let path = self.path_for(key);
        let bytes = match fs::read(&path) {
            Ok(bytes) => bytes,
            Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok(None),
            Err(error) => return Err(error),
        };
        let header_len = MAGIC.len() + 1 + 8;
        if bytes.len() < header_len || &bytes[..MAGIC.len()] != MAGIC {
            let _ = fs::remove_file(path);
            return Ok(None);
        }
        if bytes[MAGIC.len()] != FORMAT_VERSION {
            let _ = fs::remove_file(path);
            return Ok(None);
        }
        let key_len_start = MAGIC.len() + 1;
        let mut key_len_bytes = [0u8; 8];
        key_len_bytes.copy_from_slice(&bytes[key_len_start..key_len_start + 8]);
        let key_len = u64::from_le_bytes(key_len_bytes) as usize;
        let payload_start = header_len.checked_add(key_len).ok_or_else(|| {
            io::Error::new(io::ErrorKind::InvalidData, "cache key length overflow")
        })?;
        if payload_start > bytes.len()
            || String::from_utf8_lossy(&bytes[header_len..payload_start]) != key
        {
            let _ = fs::remove_file(path);
            return Ok(None);
        }
        Ok(Some(bytes[payload_start..].to_vec()))
    }

    /// Publishes an artifact atomically, so readers never observe a partial
    /// serialized value when two compiler processes share a cache directory.
    pub fn put(&self, key: &str, payload: &[u8]) -> io::Result<()> {
        fs::create_dir_all(&self.root)?;
        let path = self.path_for(key);
        let temp = path.with_extension(format!("tmp-{}", std::process::id()));
        let mut bytes = Vec::with_capacity(MAGIC.len() + 1 + 8 + key.len() + payload.len());
        bytes.extend_from_slice(MAGIC);
        bytes.push(FORMAT_VERSION);
        bytes.extend_from_slice(&(key.len() as u64).to_le_bytes());
        bytes.extend_from_slice(key.as_bytes());
        bytes.extend_from_slice(payload);
        fs::write(&temp, bytes)?;
        if let Err(error) = fs::rename(&temp, &path) {
            let _ = fs::remove_file(&temp);
            return Err(error);
        }
        Ok(())
    }

    pub fn clear(&self) -> io::Result<()> {
        match fs::remove_dir_all(&self.root) {
            Ok(()) => Ok(()),
            Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(()),
            Err(error) => Err(error),
        }
    }

    fn path_for(&self, key: &str) -> PathBuf {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        key.hash(&mut hasher);
        self.root.join(format!("{:016x}.bin", hasher.finish()))
    }
}

#[cfg(test)]
mod tests {
    use super::DiskCache;

    #[test]
    fn round_trips_bytes_and_separates_keys() {
        let dir = tempfile::tempdir().unwrap();
        let cache = DiskCache::new(dir.path());
        cache.put("rust/core/ast-v1", b"core").unwrap();
        cache.put("rust/alloc/ast-v1", b"alloc").unwrap();
        assert_eq!(
            cache.get("rust/core/ast-v1").unwrap(),
            Some(b"core".to_vec())
        );
        assert_eq!(
            cache.get("rust/alloc/ast-v1").unwrap(),
            Some(b"alloc".to_vec())
        );
        assert_eq!(cache.get("missing").unwrap(), None);
    }

    #[test]
    fn corrupt_entries_are_misses() {
        let dir = tempfile::tempdir().unwrap();
        let cache = DiskCache::new(dir.path());
        cache.put("artifact", b"valid").unwrap();
        let path = std::fs::read_dir(dir.path())
            .unwrap()
            .next()
            .unwrap()
            .unwrap()
            .path();
        std::fs::write(path, b"corrupt").unwrap();
        assert_eq!(cache.get("artifact").unwrap(), None);
    }

    #[test]
    fn clear_removes_cache_contents() {
        let dir = tempfile::tempdir().unwrap();
        let cache = DiskCache::new(dir.path().join("cache"));
        cache.put("artifact", b"value").unwrap();
        cache.clear().unwrap();
        assert_eq!(cache.get("artifact").unwrap(), None);
    }
}
