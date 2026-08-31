use std::path::{Component, Path, PathBuf};

mod generated {
    include!(concat!(env!("OUT_DIR"), "/embedded_std.rs"));
}

const VIRTUAL_ROOT: &str = "<fp-rust-std>";

pub fn root_dir() -> PathBuf {
    PathBuf::from(VIRTUAL_ROOT)
}

pub fn is_embedded_path(path: &Path) -> bool {
    path == Path::new(VIRTUAL_ROOT) || path.starts_with(Path::new(VIRTUAL_ROOT))
}

pub fn contains(path: &Path) -> bool {
    read(path).is_some()
}

pub fn read(path: &Path) -> Option<&'static str> {
    let relative = path.strip_prefix(Path::new(VIRTUAL_ROOT)).ok()?;
    let normalized = normalize_relative_path(relative)?;
    fp_core::embedded_std::read_source::<Bundle>(&normalized)
}

struct Bundle;
impl fp_core::embedded_std::SourceBundle for Bundle {
    fn paths() -> &'static [&'static str] {
        generated::PATHS
    }
    fn get(path: &str) -> Option<std::borrow::Cow<'static, [u8]>> {
        generated::get(path).map(|s| std::borrow::Cow::Borrowed(s.as_bytes()))
    }
}

/// All embedded std source file paths relative to the virtual root, e.g.
/// `["core/option.rs", "alloc/vec/mod.rs", "std/sync/mod.rs", ...]`.
pub fn module_paths() -> &'static [&'static str] {
    generated::PATHS
}

fn normalize_relative_path(path: &Path) -> Option<String> {
    let mut parts = Vec::new();
    for component in path.components() {
        match component {
            Component::Normal(part) => parts.push(part.to_str()?.to_owned()),
            Component::CurDir => {}
            Component::RootDir | Component::ParentDir | Component::Prefix(_) => return None,
        }
    }
    if parts.is_empty() {
        return None;
    }
    Some(parts.join("/"))
}
