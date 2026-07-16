use std::path::{Path, PathBuf};

pub fn default_module_roots(root: &Path) -> Vec<PathBuf> {
    let mut roots = Vec::new();
    for dir in ["src", "examples", "tests", "benches"] {
        let candidate = root.join(dir);
        if candidate.is_dir() {
            roots.push(candidate);
        }
    }
    if roots.is_empty() {
        roots.push(root.to_path_buf());
    }
    roots
}

pub fn estimate_module_path(root: &Path, file_path: &Path) -> Vec<String> {
    estimate_module_path_with_roots(root, &default_module_roots(root), file_path)
}

pub fn estimate_module_path_with_roots(
    root: &Path,
    module_roots: &[PathBuf],
    file_path: &Path,
) -> Vec<String> {
    let module_root = module_roots
        .iter()
        .filter(|candidate| file_path.starts_with(candidate))
        .max_by_key(|candidate| candidate.components().count())
        .cloned()
        .unwrap_or_else(|| root.to_path_buf());
    let rel = file_path
        .strip_prefix(&module_root)
        .or_else(|_| file_path.strip_prefix(root))
        .unwrap_or(file_path);
    let mut parts = rel
        .parent()
        .unwrap_or(Path::new(""))
        .components()
        .filter_map(|component| {
            let segment = component.as_os_str().to_str()?;
            if segment == "." || segment == ".." {
                None
            } else {
                Some(segment.to_string())
            }
        })
        .collect::<Vec<_>>();

    let stem = file_path
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("");
    if !matches!(stem, "mod" | "lib" | "main") {
        parts.push(stem.to_string());
    }
    parts
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rust_style_module_paths_use_src_root() {
        assert_eq!(
            estimate_module_path(Path::new("/proj"), Path::new("/proj/src/graph/view.rs")),
            vec!["graph".to_string(), "view".to_string()]
        );
    }

    #[test]
    fn rust_style_module_paths_fall_back_to_package_root() {
        assert_eq!(
            estimate_module_path(Path::new("/proj"), Path::new("/proj/examples/demo.rs")),
            vec!["examples".to_string(), "demo".to_string()]
        );
    }
}
