use crate::models::{DependencyModel, ManifestModel, PackageModel};
use eyre::Result;
use fp_core::package::DependencyDescriptor;
use fp_core::package::provider::{ModuleProvider, ModuleSource, PackageProvider};
use fp_rust::package::{CargoMetadataOptions, CargoPackageProvider, RustModuleProvider};
use fp_typescript::TypeScriptPackageProvider;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::ffi::OsString;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageGraph {
    pub schema_version: u32,
    pub root: PathBuf,
    pub packages: Vec<PackageNode>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub selected_package: Option<String>,
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub build_options: HashMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageNode {
    pub name: String,
    pub version: String,
    pub root: PathBuf,
    pub manifest_path: PathBuf,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub language: Option<String>,
    pub module_roots: Vec<PathBuf>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub entry: Option<PathBuf>,
    pub dependencies: Vec<DependencyEdge>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyEdge {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub package: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<PathBuf>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub git: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rev: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub branch: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tag: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub optional: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub features: Option<Vec<String>>,
}

#[derive(Debug, Clone)]
pub struct PackageGraphOptions {
    pub offline: bool,
    pub cache_dir: Option<PathBuf>,
    pub include_dependencies: bool,
    pub cargo_fetch: bool,
}

impl Default for PackageGraphOptions {
    fn default() -> Self {
        Self {
            offline: false,
            cache_dir: None,
            include_dependencies: true,
            cargo_fetch: true,
        }
    }
}

impl PackageGraph {
    pub fn from_manifest(manifest: &ManifestModel) -> Result<Self> {
        let root = manifest_root_path(manifest);
        let packages = manifest.list_packages()?;
        let packages = packages
            .into_iter()
            .map(package_to_node)
            .collect::<Vec<_>>();

        Ok(Self {
            schema_version: 1,
            root,
            packages,
            selected_package: None,
            build_options: HashMap::new(),
        })
    }

    pub fn from_path(path: &Path) -> Result<Self> {
        Self::from_path_with_options(path, &PackageGraphOptions::default())
    }

    pub fn from_path_with_options(path: &Path, options: &PackageGraphOptions) -> Result<Self> {
        let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        if path.is_dir() {
            let magnet_path = path.join("Magnet.toml");
            let cargo_path = path.join("Cargo.toml");
            let pyproject_path = path.join("pyproject.toml");
            let ts_path = path.join("package.json");
            if magnet_path.exists() {
                let manifest = ManifestModel::from_dir(&path)?;
                return Self::from_manifest(&manifest);
            }
            if cargo_path.exists() {
                return Self::from_cargo_manifest_with_options(&cargo_path, options);
            }
            if pyproject_path.exists() {
                return Self::from_python_manifest(&pyproject_path);
            }
            if ts_path.exists() {
                return Self::from_package_json(&ts_path);
            }
            return Err(eyre::eyre!(
                "No Magnet.toml, Cargo.toml, or package.json found in {}",
                path.display()
            ));
        }

        let file_name = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("");
        match file_name {
            "Magnet.toml" => {
                let root = path.parent().unwrap_or(Path::new("."));
                let manifest = ManifestModel::from_dir(root)?;
                Self::from_manifest(&manifest)
            }
            "Cargo.toml" => Self::from_cargo_manifest_with_options(&path, options),
            "pyproject.toml" => Self::from_python_manifest(&path),
            "package.json" => Self::from_package_json(&path),
            _ => Err(eyre::eyre!(
                "Unsupported manifest path {}; expected Magnet.toml, Cargo.toml, or package.json",
                path.display()
            )),
        }
    }

    pub fn from_cargo_manifest(manifest_path: &Path) -> Result<Self> {
        Self::from_cargo_manifest_with_options(manifest_path, &PackageGraphOptions::default())
    }

    pub fn from_cargo_manifest_with_options(
        manifest_path: &Path,
        options: &PackageGraphOptions,
    ) -> Result<Self> {
        let root = manifest_path
            .parent()
            .ok_or_else(|| eyre::eyre!("Cargo manifest has no parent directory"))?
            .to_path_buf();
        let _guard = apply_cargo_home(resolve_cache_dir(options))?;
        if options.cargo_fetch && !options.offline {
            prefetch_cargo_dependencies(manifest_path)?;
        }
        let provider = Arc::new(CargoPackageProvider::new_with_options(
            root.clone(),
            CargoMetadataOptions {
                offline: options.offline,
                include_dependencies: options.include_dependencies,
            },
        ));
        provider
            .refresh()
            .map_err(|err| eyre::eyre!("Failed to load Cargo workspace: {err}"))?;
        let module_provider = RustModuleProvider::new(provider.clone());

        let mut packages = Vec::new();
        for package_id in provider
            .list_packages()
            .map_err(|err| eyre::eyre!("Failed to list Cargo packages: {err}"))?
        {
            let descriptor = provider
                .load_package(&package_id)
                .map_err(|err| eyre::eyre!("Failed to load package {package_id}: {err}"))?;
            module_provider
                .modules_for_package(&package_id)
                .map_err(|err| eyre::eyre!("Failed to list modules for {package_id}: {err}"))?;
            let module_roots = default_module_roots(&descriptor.root.to_path_buf());
            let entry = detect_entry(&descriptor.root.to_path_buf(), &["rs"]);
            let dependencies = descriptor
                .metadata
                .dependencies
                .iter()
                .map(dependency_descriptor_to_edge)
                .collect();

            let node = PackageNode {
                name: descriptor.name.clone(),
                version: descriptor
                    .version
                    .as_ref()
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "0.0.0".to_string()),
                root: descriptor.root.to_path_buf(),
                manifest_path: descriptor.manifest_path.to_path_buf(),
                language: Some("rust".to_string()),
                module_roots,
                entry,
                dependencies,
            };

            packages.push(node);
        }

        Ok(Self {
            schema_version: 1,
            root,
            packages,
            selected_package: None,
            build_options: HashMap::new(),
        })
    }

    pub fn from_typescript_manifest(manifest_path: &Path) -> Result<Self> {
        let root = manifest_path
            .parent()
            .ok_or_else(|| eyre::eyre!("package.json has no parent directory"))?
            .to_path_buf();
        let provider = Arc::new(TypeScriptPackageProvider::new(root.clone()));
        provider
            .refresh()
            .map_err(|err| eyre::eyre!("Failed to load TypeScript package: {err}"))?;

        let mut packages = Vec::new();
        for package_id in provider
            .list_packages()
            .map_err(|err| eyre::eyre!("Failed to list TypeScript packages: {err}"))?
        {
            let descriptor = provider
                .load_package(&package_id)
                .map_err(|err| eyre::eyre!("Failed to load package {package_id}: {err}"))?;
            let _module_ids = provider
                .modules_for_package(&package_id)
                .map_err(|err| eyre::eyre!("Failed to list modules for {package_id}: {err}"))?;
            let module_roots = default_module_roots(&descriptor.root.to_path_buf());
            let entry = detect_entry(&descriptor.root.to_path_buf(), &["ts", "tsx"]);
            let dependencies = descriptor
                .metadata
                .dependencies
                .iter()
                .map(dependency_descriptor_to_edge)
                .collect();

            packages.push(PackageNode {
                name: descriptor.name.clone(),
                version: descriptor
                    .version
                    .as_ref()
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "0.0.0".to_string()),
                root: descriptor.root.to_path_buf(),
                manifest_path: descriptor.manifest_path.to_path_buf(),
                language: Some("typescript".to_string()),
                module_roots,
                entry,
                dependencies,
            });
        }

        Ok(Self {
            schema_version: 1,
            root,
            packages,
            selected_package: None,
            build_options: HashMap::new(),
        })
    }

    pub fn from_package_json(manifest_path: &Path) -> Result<Self> {
        let root = manifest_path
            .parent()
            .ok_or_else(|| eyre::eyre!("package.json has no parent directory"))?
            .to_path_buf();
        if has_typescript_sources(&root) || root.join("tsconfig.json").exists() {
            return Self::from_typescript_manifest(manifest_path);
        }

        let manifest = read_package_json(manifest_path)?;
        let dependencies = flatten_package_json_dependencies(&manifest);
        let node = PackageNode {
            name: manifest.name,
            version: manifest.version.unwrap_or_else(|| "0.0.0".to_string()),
            root: root.clone(),
            manifest_path: manifest_path.to_path_buf(),
            language: Some("javascript".to_string()),
            module_roots: default_module_roots(&root),
            entry: detect_entry(&root, &["js", "mjs", "cjs"]),
            dependencies,
        };

        Ok(Self {
            schema_version: 1,
            root,
            packages: vec![node],
            selected_package: None,
            build_options: HashMap::new(),
        })
    }

    pub fn from_python_manifest(manifest_path: &Path) -> Result<Self> {
        let root = manifest_path
            .parent()
            .ok_or_else(|| eyre::eyre!("pyproject.toml has no parent directory"))?
            .to_path_buf();
        let manifest = read_pyproject(manifest_path)?;
        let dependencies = manifest
            .dependencies
            .into_iter()
            .map(|dep| DependencyEdge {
                name: dep.name,
                package: None,
                version: dep.version,
                path: None,
                git: None,
                rev: None,
                branch: None,
                tag: None,
                workspace: None,
                optional: None,
                features: None,
            })
            .collect();

        let node = PackageNode {
            name: manifest.name,
            version: manifest.version.unwrap_or_else(|| "0.0.0".to_string()),
            root: root.clone(),
            manifest_path: manifest_path.to_path_buf(),
            language: Some("python".to_string()),
            module_roots: default_module_roots(&root),
            entry: detect_entry(&root, &["py"]),
            dependencies,
        };

        Ok(Self {
            schema_version: 1,
            root,
            packages: vec![node],
            selected_package: None,
            build_options: HashMap::new(),
        })
    }
}

fn prefetch_cargo_dependencies(manifest_path: &Path) -> Result<()> {
    let status = Command::new("cargo")
        .arg("fetch")
        .arg("--manifest-path")
        .arg(manifest_path)
        .status()
        .map_err(|err| eyre::eyre!("Failed to execute cargo fetch: {err}"))?;
    if !status.success() {
        return Err(eyre::eyre!(
            "cargo fetch failed with status {}",
            status.code().unwrap_or(-1)
        ));
    }
    Ok(())
}

struct CargoHomeGuard {
    previous: Option<OsString>,
    applied: bool,
}

impl Drop for CargoHomeGuard {
    fn drop(&mut self) {
        if !self.applied {
            return;
        }
        match &self.previous {
            Some(value) => unsafe {
                std::env::set_var("CARGO_HOME", value);
            },
            None => unsafe {
                std::env::remove_var("CARGO_HOME");
            },
        }
    }
}

fn apply_cargo_home(cache_dir: Option<PathBuf>) -> Result<CargoHomeGuard> {
    let Some(cache_dir) = cache_dir else {
        return Ok(CargoHomeGuard {
            previous: None,
            applied: false,
        });
    };

    std::fs::create_dir_all(&cache_dir).map_err(|err| {
        eyre::eyre!(
            "Failed to create cargo cache directory {}: {err}",
            cache_dir.display()
        )
    })?;
    let previous = std::env::var_os("CARGO_HOME");
    // Safe: we control the value and restore the previous setting in the guard.
    unsafe {
        std::env::set_var("CARGO_HOME", &cache_dir);
    }
    Ok(CargoHomeGuard {
        previous,
        applied: true,
    })
}

fn resolve_cache_dir(options: &PackageGraphOptions) -> Option<PathBuf> {
    if let Some(cache_dir) = options.cache_dir.clone() {
        return Some(cache_dir);
    }
    if let Some(cache_dir) = std::env::var_os("MAGNET_CACHE_DIR") {
        return Some(PathBuf::from(cache_dir));
    }
    if std::env::var_os("CARGO_HOME").is_some() {
        return None;
    }
    let home = std::env::var_os("HOME")
        .or_else(|| std::env::var_os("USERPROFILE"))
        .map(PathBuf::from)?;
    Some(home.join(".cache").join("magnet").join("cargo"))
}

fn package_to_node(package: PackageModel) -> PackageNode {
    let module_root = package.root_path.join("src");
    let entry = {
        let path = module_root.join("main.fp");
        if path.exists() { Some(path) } else { None }
    };

    PackageNode {
        name: package.name,
        version: package.version,
        root: package.root_path.clone(),
        manifest_path: package.source_path,
        language: None,
        module_roots: vec![module_root],
        entry,
        dependencies: package
            .dependencies
            .into_iter()
            .map(|(name, dep)| dependency_to_edge(name, dep))
            .collect(),
    }
}

fn dependency_to_edge(name: String, dep: DependencyModel) -> DependencyEdge {
    DependencyEdge {
        name,
        package: dep.package,
        version: dep.version,
        path: dep.path,
        git: dep.git,
        rev: dep.rev,
        branch: dep.branch,
        tag: dep.tag,
        workspace: dep.workspace,
        optional: dep.optional,
        features: dep.features,
    }
}

fn dependency_descriptor_to_edge(dep: &DependencyDescriptor) -> DependencyEdge {
    DependencyEdge {
        name: dep.package.clone(),
        package: None,
        version: dep.constraint.as_ref().map(|v| v.to_string()),
        path: None,
        git: None,
        rev: None,
        branch: None,
        tag: None,
        workspace: None,
        optional: Some(dep.optional),
        features: Some(dep.features.clone()),
    }
}

#[derive(Debug, Deserialize)]
struct PackageJsonManifest {
    name: String,
    #[serde(default)]
    version: Option<String>,
    #[serde(default)]
    dependencies: Option<HashMap<String, serde_json::Value>>,
    #[serde(rename = "devDependencies", default)]
    dev_dependencies: Option<HashMap<String, serde_json::Value>>,
    #[serde(rename = "optionalDependencies", default)]
    optional_dependencies: Option<HashMap<String, serde_json::Value>>,
}

fn read_package_json(path: &Path) -> Result<PackageJsonManifest> {
    let contents =
        std::fs::read_to_string(path).map_err(|err| eyre::eyre!("{}: {err}", path.display()))?;
    let manifest: PackageJsonManifest =
        serde_json::from_str(&contents).map_err(|err| eyre::eyre!("{}: {err}", path.display()))?;
    Ok(manifest)
}

fn flatten_package_json_dependencies(manifest: &PackageJsonManifest) -> Vec<DependencyEdge> {
    let mut deps = Vec::new();
    deps.extend(dependencies_from_map(&manifest.dependencies));
    deps.extend(dependencies_from_map(&manifest.dev_dependencies));
    deps.extend(dependencies_from_map(&manifest.optional_dependencies));
    deps
}

fn dependencies_from_map(map: &Option<HashMap<String, serde_json::Value>>) -> Vec<DependencyEdge> {
    let mut deps = Vec::new();
    if let Some(values) = map {
        for (name, value) in values {
            let version = value.as_str().map(|v| v.to_string()).or_else(|| {
                value
                    .as_object()
                    .and_then(|obj| obj.get("version").and_then(|v| v.as_str()))
                    .map(|v| v.to_string())
            });
            deps.push(DependencyEdge {
                name: name.clone(),
                package: None,
                version,
                path: None,
                git: None,
                rev: None,
                branch: None,
                tag: None,
                workspace: None,
                optional: None,
                features: None,
            });
        }
    }
    deps
}

#[derive(Debug)]
struct PyProjectManifest {
    name: String,
    version: Option<String>,
    dependencies: Vec<PyDependency>,
}

#[derive(Debug)]
struct PyDependency {
    name: String,
    version: Option<String>,
}

fn read_pyproject(path: &Path) -> Result<PyProjectManifest> {
    let contents =
        std::fs::read_to_string(path).map_err(|err| eyre::eyre!("{}: {err}", path.display()))?;
    let value: toml::Value =
        toml::from_str(&contents).map_err(|err| eyre::eyre!("{}: {err}", path.display()))?;
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
        .map(parse_python_dependencies)
        .unwrap_or_default();
    Ok(PyProjectManifest {
        name,
        version,
        dependencies,
    })
}

fn parse_python_dependencies(values: &Vec<toml::Value>) -> Vec<PyDependency> {
    values
        .iter()
        .filter_map(|value| value.as_str())
        .map(|raw| {
            let mut name = String::new();
            let mut version = String::new();
            for ch in raw.chars() {
                if name.is_empty() {
                    if ch.is_whitespace() || is_version_marker(ch) {
                        continue;
                    }
                    if is_name_char(ch) {
                        name.push(ch);
                    }
                    continue;
                }
                if version.is_empty() {
                    if is_name_char(ch) {
                        name.push(ch);
                    } else {
                        version.push(ch);
                    }
                } else {
                    version.push(ch);
                }
            }
            let name = name.trim().to_string();
            let version = if version.trim().is_empty() {
                None
            } else {
                Some(version.trim().to_string())
            };
            PyDependency { name, version }
        })
        .filter(|dep| !dep.name.is_empty())
        .collect()
}

fn is_version_marker(ch: char) -> bool {
    matches!(ch, '<' | '>' | '=' | '!' | '~' | '^')
}

fn is_name_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_' || ch == '-'
}

fn manifest_root_path(manifest: &ManifestModel) -> PathBuf {
    match manifest {
        ManifestModel::Nexus(nexus) => nexus.root_path.clone(),
        ManifestModel::Workspace(workspace) => workspace.root_path.clone(),
        ManifestModel::Package(package) => package.root_path.clone(),
    }
}

fn default_module_roots(root: &Path) -> Vec<PathBuf> {
    let src = root.join("src");
    if src.is_dir() {
        vec![src]
    } else {
        vec![root.to_path_buf()]
    }
}

fn detect_entry(root: &Path, extensions: &[&str]) -> Option<PathBuf> {
    let src = root.join("src");
    let candidates = if src.is_dir() {
        vec![src]
    } else {
        vec![root.to_path_buf()]
    };
    for base in candidates {
        for stem in ["main", "index", "lib"] {
            for ext in extensions {
                let candidate = base.join(format!("{}.{}", stem, ext));
                if candidate.exists() {
                    return Some(candidate);
                }
            }
        }
    }
    None
}

fn has_typescript_sources(root: &Path) -> bool {
    let src = root.join("src");
    if !src.exists() {
        return false;
    }
    let mut stack = vec![src];
    while let Some(dir) = stack.pop() {
        if let Ok(entries) = std::fs::read_dir(&dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    if path.file_name().and_then(|n| n.to_str()) == Some("node_modules") {
                        continue;
                    }
                    stack.push(path);
                } else if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
                    if ext == "ts" || ext == "tsx" {
                        return true;
                    }
                }
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::ManifestModel;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn build_graph_from_package_manifest() -> Result<()> {
        let temp = tempdir()?;
        let package_dir = temp.path();
        fs::create_dir_all(package_dir.join("src"))?;
        fs::write(
            package_dir.join("Magnet.toml"),
            "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n",
        )?;

        let manifest = ManifestModel::from_dir(package_dir)?;
        let graph = PackageGraph::from_manifest(&manifest)?;

        assert_eq!(graph.packages.len(), 1);
        assert_eq!(graph.packages[0].name, "demo");
        Ok(())
    }

    #[test]
    fn build_graph_from_cargo_manifest() -> Result<()> {
        let temp = tempdir()?;
        let root = temp.path();
        fs::create_dir_all(root.join("src"))?;
        fs::write(
            root.join("Cargo.toml"),
            r#"[package]
name = "cargo-demo"
version = "0.1.0"
edition = "2021"
"#,
        )?;
        fs::write(root.join("src").join("lib.rs"), "pub fn demo() {}")?;

        let graph = PackageGraph::from_cargo_manifest(&root.join("Cargo.toml"))?;
        assert_eq!(graph.packages.len(), 1);
        assert_eq!(graph.packages[0].name, "cargo-demo");
        Ok(())
    }

    #[test]
    fn build_graph_from_package_json() -> Result<()> {
        let temp = tempdir()?;
        let root = temp.path();
        fs::create_dir_all(root.join("src"))?;
        fs::write(
            root.join("package.json"),
            r#"{"name": "ts-demo", "version": "1.0.0"}"#,
        )?;
        fs::write(root.join("src").join("index.ts"), "export const value = 1;")?;

        let graph = PackageGraph::from_typescript_manifest(&root.join("package.json"))?;
        assert_eq!(graph.packages.len(), 1);
        assert_eq!(graph.packages[0].name, "ts-demo");
        Ok(())
    }

    #[test]
    fn build_graph_from_javascript_package_json() -> Result<()> {
        let temp = tempdir()?;
        let root = temp.path();
        fs::create_dir_all(root.join("src"))?;
        fs::write(
            root.join("package.json"),
            r#"{"name": "js-demo", "version": "1.0.0"}"#,
        )?;
        fs::write(root.join("src").join("index.js"), "console.log('demo');")?;

        let graph = PackageGraph::from_package_json(&root.join("package.json"))?;
        assert_eq!(graph.packages.len(), 1);
        assert_eq!(graph.packages[0].name, "js-demo");
        assert_eq!(graph.packages[0].language.as_deref(), Some("javascript"));
        Ok(())
    }

    #[test]
    fn build_graph_from_pyproject() -> Result<()> {
        let temp = tempdir()?;
        let root = temp.path();
        fs::create_dir_all(root.join("src"))?;
        fs::write(
            root.join("pyproject.toml"),
            r#"[project]
name = "py-demo"
version = "0.1.0"
dependencies = ["requests>=2.0"]
"#,
        )?;
        fs::write(root.join("src").join("main.py"), "print('demo')")?;

        let graph = PackageGraph::from_python_manifest(&root.join("pyproject.toml"))?;
        assert_eq!(graph.packages.len(), 1);
        assert_eq!(graph.packages[0].name, "py-demo");
        assert_eq!(graph.packages[0].language.as_deref(), Some("python"));
        Ok(())
    }

    #[test]
    fn build_graph_from_magnet_workspace_with_dependencies() -> Result<()> {
        let temp = tempdir()?;
        let root = temp.path();
        fs::write(
            root.join("Magnet.toml"),
            r#"[workspace]
members = ["crates/*"]
"#,
        )?;

        let crates_dir = root.join("crates");
        fs::create_dir_all(&crates_dir)?;

        let shared_dir = crates_dir.join("shared");
        fs::create_dir_all(shared_dir.join("src"))?;
        fs::write(
            shared_dir.join("Magnet.toml"),
            r#"[package]
name = "shared"
version = "0.1.0"
"#,
        )?;
        fs::write(shared_dir.join("src").join("lib.fp"), "pub fn shared() {}")?;

        let app_dir = crates_dir.join("app");
        fs::create_dir_all(app_dir.join("src"))?;
        fs::write(
            app_dir.join("Magnet.toml"),
            r#"[package]
name = "app"
version = "0.1.0"

[dependencies]
shared = { path = "../shared" }
"#,
        )?;
        fs::write(app_dir.join("src").join("main.fp"), "fn main() {}")?;

        let graph = PackageGraph::from_path(root)?;
        let app = graph
            .packages
            .iter()
            .find(|pkg| pkg.name == "app")
            .expect("missing app package");
        assert!(app.dependencies.iter().any(|dep| dep.name == "shared"));
        Ok(())
    }
}
