use crate::models::{
    DependencyModel, DependencyModelMap, IdentityCandidate, LanguageKind, ManifestKind,
    ManifestModel, PackageModel, PatchMap, WorkspaceModel,
};
use eyre::{ContextCompat, Result};
use fp_golang::read_go_mod;
use fp_python::read_pyproject;
use fp_typescript::read_package_json;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

pub fn discover(dir: &Path) -> Result<Vec<IdentityCandidate>> {
    let mut candidates = Vec::new();
    if dir.join("Magnet.toml").exists() {
        candidates.push(magnet_candidate(dir)?);
    }
    if dir.join("Cargo.toml").exists() {
        candidates.extend(cargo_candidates(dir)?);
    }
    if dir.join("pyproject.toml").exists() {
        candidates.push(pyproject_candidate(&dir.join("pyproject.toml"))?);
    }
    if dir.join("package.json").exists() {
        candidates.push(package_json_candidate(&dir.join("package.json"))?);
    }
    if dir.join("go.mod").exists() {
        candidates.push(go_mod_candidate(&dir.join("go.mod"))?);
    }
    Ok(candidates)
}

pub fn package_from_candidate(candidate: &IdentityCandidate) -> Result<PackageModel> {
    match candidate.language {
        LanguageKind::Magnet | LanguageKind::Rust => PackageModel::from_dir(&candidate.root_path),
        LanguageKind::Python => package_from_pyproject(&candidate.manifest_path),
        LanguageKind::JavaScript | LanguageKind::TypeScript => {
            package_from_package_json(&candidate.manifest_path)
        }
        LanguageKind::Go => package_from_go_mod(&candidate.manifest_path),
    }
}

fn magnet_candidate(dir: &Path) -> Result<IdentityCandidate> {
    let manifest = ManifestModel::from_dir(dir)?;
    manifest_candidate(LanguageKind::Magnet, manifest)
}

fn cargo_candidates(dir: &Path) -> Result<Vec<IdentityCandidate>> {
    let manifest = ManifestModel::from_dir(dir)?;
    let mut candidates = vec![manifest_candidate(LanguageKind::Rust, manifest)?];
    if let Ok(package) = PackageModel::from_dir(dir) {
        let candidate = package_candidate(LanguageKind::Rust, &package);
        if !candidates
            .iter()
            .any(|existing| existing.manifest_kind == candidate.manifest_kind)
        {
            candidates.push(candidate);
        }
    }
    Ok(candidates)
}

fn manifest_candidate(
    language: LanguageKind,
    manifest: ManifestModel,
) -> Result<IdentityCandidate> {
    match manifest {
        ManifestModel::Nexus(nexus) => Ok(IdentityCandidate {
            language,
            manifest_kind: ManifestKind::Nexus,
            name: nexus.name,
            version: None,
            root_path: nexus.root_path,
            manifest_path: nexus.source_path,
            source_roots: Vec::new(),
        }),
        ManifestModel::Workspace(workspace) => Ok(workspace_candidate(language, &workspace)),
        ManifestModel::Package(package) => Ok(package_candidate(language, &package)),
    }
}

fn workspace_candidate(language: LanguageKind, workspace: &WorkspaceModel) -> IdentityCandidate {
    IdentityCandidate {
        language,
        manifest_kind: ManifestKind::Workspace,
        name: workspace.name.clone(),
        version: None,
        root_path: workspace.root_path.clone(),
        manifest_path: workspace.source_path.clone(),
        source_roots: Vec::new(),
    }
}

fn package_candidate(language: LanguageKind, package: &PackageModel) -> IdentityCandidate {
    IdentityCandidate {
        language,
        manifest_kind: ManifestKind::Package,
        name: package.name.clone(),
        version: Some(package.version.clone()),
        root_path: package.root_path.clone(),
        manifest_path: package.source_path.clone(),
        source_roots: default_source_roots(&package.root_path, language),
    }
}

fn package_json_candidate(path: &Path) -> Result<IdentityCandidate> {
    let root = manifest_parent(path, "package.json")?;
    let manifest =
        read_package_json(path).map_err(|err| eyre::eyre!("{}: {err}", path.display()))?;
    let language = if root.join("tsconfig.json").exists() || has_typescript_sources(&root) {
        LanguageKind::TypeScript
    } else {
        LanguageKind::JavaScript
    };
    Ok(IdentityCandidate {
        language,
        manifest_kind: ManifestKind::Package,
        name: manifest.name,
        version: manifest.version,
        root_path: root.clone(),
        manifest_path: path.to_path_buf(),
        source_roots: default_source_roots(&root, language),
    })
}

fn package_from_package_json(path: &Path) -> Result<PackageModel> {
    let root = manifest_parent(path, "package.json")?;
    let manifest =
        read_package_json(path).map_err(|err| eyre::eyre!("{}: {err}", path.display()))?;
    let mut dependencies = dependency_map_from_json(&manifest.dependencies, false);
    merge_dependency_maps(
        &mut dependencies,
        dependency_map_from_json(&manifest.optional_dependencies, true),
    );
    Ok(PackageModel {
        name: manifest.name,
        version: manifest.version.unwrap_or_else(|| "0.0.0".to_string()),
        edition: String::new(),
        description: String::new(),
        authors: Vec::new(),
        homepage: None,
        repository: None,
        documentation: None,
        license: None,
        custom: HashMap::new(),
        dependencies,
        dev_dependencies: dependency_map_from_json(&manifest.dev_dependencies, false),
        build_dependencies: DependencyModelMap::new(),
        target_dependencies: Vec::new(),
        patch: PatchMap::new(),
        root_path: root,
        source_path: path.to_path_buf(),
    })
}

fn dependency_map_from_json(
    map: &Option<HashMap<String, serde_json::Value>>,
    optional: bool,
) -> DependencyModelMap {
    let mut deps = DependencyModelMap::new();
    if let Some(values) = map {
        for (name, value) in values {
            let version = value.as_str().map(|v| v.to_string()).or_else(|| {
                value
                    .as_object()
                    .and_then(|obj| obj.get("version").and_then(|v| v.as_str()))
                    .map(|v| v.to_string())
            });
            deps.insert(
                name.clone(),
                DependencyModel {
                    version,
                    optional: optional.then_some(true),
                    ..DependencyModel::default()
                },
            );
        }
    }
    deps
}

fn merge_dependency_maps(base: &mut DependencyModelMap, extra: DependencyModelMap) {
    for (name, dep) in extra {
        base.entry(name).or_insert(dep);
    }
}

fn pyproject_candidate(path: &Path) -> Result<IdentityCandidate> {
    let root = manifest_parent(path, "pyproject.toml")?;
    let manifest = read_pyproject(path).map_err(|err| eyre::eyre!("{}: {err}", path.display()))?;
    Ok(IdentityCandidate {
        language: LanguageKind::Python,
        manifest_kind: ManifestKind::Package,
        name: manifest.name,
        version: manifest.version,
        root_path: root.clone(),
        manifest_path: path.to_path_buf(),
        source_roots: default_source_roots(&root, LanguageKind::Python),
    })
}

fn package_from_pyproject(path: &Path) -> Result<PackageModel> {
    let root = manifest_parent(path, "pyproject.toml")?;
    let manifest = read_pyproject(path).map_err(|err| eyre::eyre!("{}: {err}", path.display()))?;
    let mut dependencies = DependencyModelMap::new();
    for dep in manifest.dependencies {
        let (name, version) = split_dependency_string(&dep);
        dependencies.insert(
            name,
            DependencyModel {
                version,
                ..DependencyModel::default()
            },
        );
    }
    for deps in manifest.optional_dependencies.values() {
        for dep in deps {
            let (name, version) = split_dependency_string(&dep);
            dependencies
                .entry(name)
                .and_modify(|existing| existing.optional = Some(true))
                .or_insert(DependencyModel {
                    version,
                    optional: Some(true),
                    ..DependencyModel::default()
                });
        }
    }
    Ok(PackageModel {
        name: manifest.name,
        version: manifest.version.unwrap_or_else(|| "0.0.0".to_string()),
        edition: String::new(),
        description: String::new(),
        authors: Vec::new(),
        homepage: None,
        repository: None,
        documentation: None,
        license: None,
        custom: HashMap::new(),
        dependencies,
        dev_dependencies: DependencyModelMap::new(),
        build_dependencies: DependencyModelMap::new(),
        target_dependencies: Vec::new(),
        patch: PatchMap::new(),
        root_path: root,
        source_path: path.to_path_buf(),
    })
}

fn go_mod_candidate(path: &Path) -> Result<IdentityCandidate> {
    let root = manifest_parent(path, "go.mod")?;
    let manifest = read_go_mod(path).map_err(|err| eyre::eyre!("{}: {err}", path.display()))?;
    Ok(IdentityCandidate {
        language: LanguageKind::Go,
        manifest_kind: ManifestKind::Package,
        name: manifest.module,
        version: None,
        root_path: root.clone(),
        manifest_path: path.to_path_buf(),
        source_roots: default_source_roots(&root, LanguageKind::Go),
    })
}

fn package_from_go_mod(path: &Path) -> Result<PackageModel> {
    let root = manifest_parent(path, "go.mod")?;
    let manifest = read_go_mod(path).map_err(|err| eyre::eyre!("{}: {err}", path.display()))?;
    Ok(PackageModel {
        name: manifest.module,
        version: "0.0.0".to_string(),
        edition: manifest.go_version.unwrap_or_default(),
        description: String::new(),
        authors: Vec::new(),
        homepage: None,
        repository: None,
        documentation: None,
        license: None,
        custom: HashMap::new(),
        dependencies: manifest
            .dependencies
            .into_iter()
            .map(|dep| {
                (
                    dep.name,
                    DependencyModel {
                        version: dep.version,
                        ..DependencyModel::default()
                    },
                )
            })
            .collect(),
        dev_dependencies: DependencyModelMap::new(),
        build_dependencies: DependencyModelMap::new(),
        target_dependencies: Vec::new(),
        patch: PatchMap::new(),
        root_path: root,
        source_path: path.to_path_buf(),
    })
}

fn manifest_parent(path: &Path, manifest_name: &str) -> Result<PathBuf> {
    path.parent()
        .with_context(|| format!("{manifest_name} has no parent directory"))
        .map(|parent| parent.to_path_buf())
}

fn default_source_roots(root: &Path, language: LanguageKind) -> Vec<PathBuf> {
    let src = root.join("src");
    match language {
        LanguageKind::Magnet
        | LanguageKind::Rust
        | LanguageKind::JavaScript
        | LanguageKind::TypeScript => {
            if src.is_dir() {
                vec![src]
            } else {
                vec![root.to_path_buf()]
            }
        }
        LanguageKind::Python | LanguageKind::Go => {
            if src.is_dir() {
                vec![src, root.to_path_buf()]
            } else {
                vec![root.to_path_buf()]
            }
        }
    }
}

fn split_dependency_string(raw: &str) -> (String, Option<String>) {
    let mut name = String::new();
    let mut version = String::new();
    for ch in raw.chars() {
        if version.is_empty() && is_name_char(ch) {
            name.push(ch);
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
    (name, version)
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
                } else if let Some(ext) = path.extension().and_then(|ext| ext.to_str()) {
                    if ext == "ts" || ext == "tsx" {
                        return true;
                    }
                }
            }
        }
    }
    false
}

fn is_name_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_' || ch == '-' || ch == '.'
}
