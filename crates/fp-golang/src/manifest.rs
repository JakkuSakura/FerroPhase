use eyre::{Context, Result};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct GoDependency {
    pub name: String,
    pub version: Option<String>,
}

#[derive(Debug, Clone)]
pub struct GoModManifest {
    pub module: String,
    pub go_version: Option<String>,
    pub dependencies: Vec<GoDependency>,
}

pub fn read_go_mod(path: impl AsRef<Path>) -> Result<GoModManifest> {
    let path = path.as_ref();
    let contents = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read go.mod from {}", path.display()))?;
    let mut module = None;
    let mut go_version = None;
    let mut dependencies = Vec::new();
    let mut in_require_block = false;

    for raw in contents.lines() {
        let line = raw.trim();
        if line.is_empty() || line.starts_with("//") {
            continue;
        }
        if let Some(rest) = line.strip_prefix("module ") {
            module = Some(rest.trim().to_string());
            continue;
        }
        if let Some(rest) = line.strip_prefix("go ") {
            go_version = Some(rest.trim().to_string());
            continue;
        }
        if line == "require (" {
            in_require_block = true;
            continue;
        }
        if in_require_block && line == ")" {
            in_require_block = false;
            continue;
        }
        let dep_line = if in_require_block {
            Some(line)
        } else {
            line.strip_prefix("require ").map(str::trim)
        };
        let Some(dep_line) = dep_line else {
            continue;
        };
        let dep_line = dep_line.split("//").next().unwrap_or("").trim();
        let mut parts = dep_line.split_whitespace();
        let Some(name) = parts.next() else {
            continue;
        };
        let version = parts.next().map(|part| part.to_string());
        dependencies.push(GoDependency {
            name: name.to_string(),
            version,
        });
    }

    Ok(GoModManifest {
        module: module.ok_or_else(|| eyre::eyre!("{}: missing module line", path.display()))?,
        go_version,
        dependencies,
    })
}

pub fn default_module_roots(root: &Path) -> Vec<PathBuf> {
    let src = root.join("src");
    if src.is_dir() {
        vec![src, root.to_path_buf()]
    } else {
        vec![root.to_path_buf()]
    }
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
    rel.parent()
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
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reads_go_mod_manifest() -> Result<()> {
        let temp = tempfile::tempdir()?;
        let path = temp.path().join("go.mod");
        std::fs::write(
            &path,
            r#"module example.com/demo

go 1.22

require (
    github.com/foo/bar v1.2.3
)
"#,
        )?;

        let manifest = read_go_mod(&path)?;
        assert_eq!(manifest.module, "example.com/demo");
        assert_eq!(manifest.go_version.as_deref(), Some("1.22"));
        assert_eq!(manifest.dependencies.len(), 1);
        assert_eq!(manifest.dependencies[0].name, "github.com/foo/bar");
        Ok(())
    }

    #[test]
    fn estimates_go_module_path_from_directory() {
        assert_eq!(
            estimate_module_path(Path::new("/proj"), Path::new("/proj/internal/service/http.go")),
            vec!["internal".to_string(), "service".to_string()]
        );
    }
}
