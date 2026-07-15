use eyre::{Context, Result};
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;

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
}
