use std::path::{Path, PathBuf};

pub fn resolve_run_path(path: &Path) -> PathBuf {
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("."))
            .join(path)
    }
}

pub fn resolve_start_dir(path: &Path) -> PathBuf {
    if path.is_file() {
        path.parent().unwrap_or(Path::new(".")).to_path_buf()
    } else {
        path.to_path_buf()
    }
}

pub fn output_path_for_entry(entry: &Path, output_dir: &Path) -> PathBuf {
    let stem = entry.file_stem().and_then(|s| s.to_str()).unwrap_or("out");
    let mut path = output_dir.join(stem);
    if cfg!(windows) {
        path.set_extension("exe");
    }
    path
}
