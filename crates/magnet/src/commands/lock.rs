//! Command implementation for generating Magnet.lock

use crate::models::PackageGraphOptions;
use crate::resolver::project::resolve_graph;
use eyre::Result;
use std::path::{Path, PathBuf};
use tracing::info;

pub struct LockOptions {
    pub path: PathBuf,
    pub offline: bool,
    pub cache_dir: Option<PathBuf>,
    pub fetch: bool,
}

pub fn lock(options: &LockOptions) -> Result<()> {
    let offline = options.offline || env_flag_enabled("MAGNET_OFFLINE");
    let graph_options = PackageGraphOptions {
        offline,
        cache_dir: options.cache_dir.clone(),
        include_dependencies: true,
        include_dev_dependencies: true,
        include_build_dependencies: true,
        cargo_fetch: options.fetch,
        resolve_registry: !offline,
        allow_multiple_versions: false,
        use_lock: false,
        refresh_index: false,
        write_lock: true,
        target: None,
    };
    resolve_graph(&options.path, &graph_options)?;
    let root = resolve_root(&options.path);
    info!("wrote {}", root.join("Magnet.lock").display());
    Ok(())
}

fn env_flag_enabled(name: &str) -> bool {
    std::env::var(name)
        .map(|value| matches!(value.as_str(), "1" | "true" | "TRUE" | "yes" | "YES"))
        .unwrap_or(false)
}

fn resolve_root(path: &Path) -> PathBuf {
    if path.is_dir() {
        return path.to_path_buf();
    }
    path.parent()
        .map(|path| path.to_path_buf())
        .unwrap_or_else(|| PathBuf::from("."))
}
