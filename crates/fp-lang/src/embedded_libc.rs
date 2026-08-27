use std::path::{Path, PathBuf};

pub fn root_dir() -> PathBuf {
    crate::embedded_std::package_root("libc")
}

pub fn read(path: &Path) -> Option<&'static str> {
    crate::embedded_std::read(path)
}

pub fn module_paths() -> &'static [&'static str] {
    crate::embedded_std::package_paths("libc")
}
