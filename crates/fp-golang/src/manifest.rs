use eyre::{Context, Result};
use std::path::Path;

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
}
