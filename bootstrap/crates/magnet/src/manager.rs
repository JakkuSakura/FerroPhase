use std::path::{Path, PathBuf};

use crate::utils::find_furthest_manifest;

pub fn resolve_manifest_root(path: &Path) -> crate::Result<(PathBuf, PathBuf)> {
    let start = if path.is_file() {
        path.parent().unwrap_or(Path::new(".")).to_path_buf()
    } else {
        path.to_path_buf()
    };
    find_furthest_manifest(&start)
}
