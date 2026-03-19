use std::path::{Component, Path, PathBuf};

mod generated {
    include!(concat!(env!("OUT_DIR"), "/embedded_std.rs"));
}

const VIRTUAL_ROOT: &str = "<fp-lang-std>";

pub fn root_dir() -> PathBuf {
    PathBuf::from(VIRTUAL_ROOT)
}

pub fn root_module_path() -> PathBuf {
    root_dir().join("mod.fp")
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
    generated::get(&normalized)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn embedded_std_exposes_root_module() {
        let path = root_module_path();
        let source = read(&path).expect("embedded std root module should exist");
        assert!(source.contains("pub mod collections;"));
    }
}
