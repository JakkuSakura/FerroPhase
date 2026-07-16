use eyre::{Context, Result};
use serde::Deserialize;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Deserialize)]
pub struct PyProjectManifest {
    pub name: String,
    pub version: Option<String>,
    #[serde(default)]
    pub dependencies: Vec<String>,
    #[serde(default)]
    pub optional_dependencies: HashMap<String, Vec<String>>,
}

pub fn read_pyproject(path: impl AsRef<Path>) -> Result<PyProjectManifest> {
    let path = path.as_ref();
    let contents = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read pyproject.toml from {}", path.display()))?;
    let value: toml::Value = toml::from_str(&contents)
        .with_context(|| format!("Failed to parse pyproject.toml from {}", path.display()))?;
    let project = value
        .get("project")
        .and_then(|p| p.as_table())
        .ok_or_else(|| eyre::eyre!("{}: missing [project] table", path.display()))?;
    let name = project
        .get("name")
        .and_then(|v| v.as_str())
        .ok_or_else(|| eyre::eyre!("{}: missing project.name", path.display()))?
        .to_string();
    let version = project
        .get("version")
        .and_then(|v| v.as_str())
        .map(|v| v.to_string());
    let dependencies = project
        .get("dependencies")
        .and_then(|v| v.as_array())
        .map(|values| {
            values
                .iter()
                .filter_map(|value| value.as_str())
                .map(|value| value.to_string())
                .collect()
        })
        .unwrap_or_default();
    let optional_dependencies = project
        .get("optional-dependencies")
        .and_then(|v| v.as_table())
        .map(|table| {
            table
                .iter()
                .map(|(extra, values)| {
                    let deps = values
                        .as_array()
                        .map(|items| {
                            items
                                .iter()
                                .filter_map(|value| value.as_str())
                                .map(|value| value.to_string())
                                .collect()
                        })
                        .unwrap_or_default();
                    (extra.clone(), deps)
                })
                .collect()
        })
        .unwrap_or_default();
    Ok(PyProjectManifest {
        name,
        version,
        dependencies,
        optional_dependencies,
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
    if stem != "__init__" {
        parts.push(stem.to_string());
    }
    parts
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reads_pyproject_manifest() -> Result<()> {
        let temp = tempfile::tempdir()?;
        let path = temp.path().join("pyproject.toml");
        std::fs::write(
            &path,
            r#"[project]
name = "demo"
version = "0.1.0"
dependencies = ["requests>=2.0"]

[project.optional-dependencies]
dev = ["pytest>=7.0"]
"#,
        )?;

        let manifest = read_pyproject(&path)?;
        assert_eq!(manifest.name, "demo");
        assert_eq!(manifest.dependencies, vec!["requests>=2.0"]);
        assert_eq!(manifest.optional_dependencies["dev"], vec!["pytest>=7.0"]);
        Ok(())
    }

    #[test]
    fn estimates_python_module_path() {
        assert_eq!(
            estimate_module_path(Path::new("/proj"), Path::new("/proj/pkg/__init__.py")),
            vec!["pkg".to_string()]
        );
    }
}
