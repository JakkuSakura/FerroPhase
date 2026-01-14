use crate::models::{LockIndex, MagnetLock, ManifestModel, PackageGraph, PackageGraphOptions};
use eyre::Result;
use std::path::Path;

pub fn load_manifest(path: &Path) -> Result<ManifestModel> {
    ManifestModel::from_dir(path)
}

pub fn resolve_graph(path: &Path, options: &PackageGraphOptions) -> Result<PackageGraph> {
    PackageGraph::from_path_with_options(path, options)
}

pub fn load_lock_index(root: &Path) -> Result<Option<LockIndex>> {
    let lock_path = root.join("Magnet.lock");
    let lock = MagnetLock::read_from_path(&lock_path)?;
    Ok(lock.as_ref().map(LockIndex::from_lock))
}
