use crate::models::{DependencyModel, ManifestModel, PackageModel};
use eyre::Result;
use fp_core::package::provider::{ModuleSource, PackageProvider};
use fp_core::package::DependencyDescriptor;
use fp_rust::package::{CargoPackageProvider, RustModuleProvider};
use fp_typescript::TypeScriptPackageProvider;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::path::PathBuf;
use std::path::Path;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageGraph {
    pub schema_version: u32,
    pub root: PathBuf,
    pub packages: Vec<PackageNode>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub selected_package: Option<String>,
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
        })
    }

    pub fn from_path(path: &Path) -> Result<Self> {
        let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        if path.is_dir() {
            let magnet_path = path.join("Magnet.toml");
            let cargo_path = path.join("Cargo.toml");
            let ts_path = path.join("package.json");
            if magnet_path.exists() {
                let manifest = ManifestModel::from_dir(&path)?;
                return Self::from_manifest(&manifest);
            }
            if cargo_path.exists() {
                return Self::from_cargo_manifest(&cargo_path);
            }
            if ts_path.exists() {
                return Self::from_typescript_manifest(&ts_path);
            }
            return Err(eyre::eyre!(
                "No Magnet.toml, Cargo.toml, or package.json found in {}",
                path.display()
            ));
        }

        let file_name = path.file_name().and_then(|name| name.to_str()).unwrap_or("");
        match file_name {
            "Magnet.toml" => {
                let root = path.parent().unwrap_or(Path::new("."));
                let manifest = ManifestModel::from_dir(root)?;
                Self::from_manifest(&manifest)
            }
            "Cargo.toml" => Self::from_cargo_manifest(&path),
            "package.json" => Self::from_typescript_manifest(&path),
            _ => Err(eyre::eyre!(
                "Unsupported manifest path {}; expected Magnet.toml, Cargo.toml, or package.json",
                path.display()
            )),
        }
    }

    pub fn from_cargo_manifest(manifest_path: &Path) -> Result<Self> {
        let root = manifest_path
            .parent()
            .ok_or_else(|| eyre::eyre!("Cargo manifest has no parent directory"))?
            .to_path_buf();
        let provider = Arc::new(CargoPackageProvider::new(root.clone()));
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
        })
    }
}

fn package_to_node(package: PackageModel) -> PackageNode {
    let module_root = package.root_path.join("src");
    let entry = {
        let path = module_root.join("main.fp");
        if path.exists() {
            Some(path)
        } else {
            None
        }
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
    let candidates = if src.is_dir() { vec![src] } else { vec![root.to_path_buf()] };
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
}
