use crate::models::{DependencyEdge, PackageGraph, PackageNode};
use eyre::{Context, Result, eyre};
use fp_core::workspace::{
    WorkspaceDependency, WorkspaceDocument, WorkspaceModule, WorkspacePackage,
};
use std::collections::{HashSet, VecDeque};
use std::path::{Path, PathBuf};

pub fn build_workspace_document(graph: &PackageGraph) -> Result<WorkspaceDocument> {
    let manifest = detect_workspace_manifest(&graph.root)?;
    let mut packages = Vec::new();
    for package in &graph.packages {
        packages.push(package_to_workspace(package)?);
    }
    Ok(WorkspaceDocument::new(manifest.to_string_lossy()).with_packages(packages))
}

pub fn write_workspace_graph(graph: &PackageGraph, output_path: &Path) -> Result<()> {
    let workspace = build_workspace_document(graph)?;
    let payload =
        serde_json::to_string_pretty(&workspace).context("Failed to serialize workspace graph")?;
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create {}", parent.display()))?;
    }
    std::fs::write(output_path, payload)
        .with_context(|| format!("Failed to write {}", output_path.display()))?;
    Ok(())
}

fn detect_workspace_manifest(root: &Path) -> Result<PathBuf> {
    let candidates = [
        root.join("Magnet.toml"),
        root.join("Cargo.toml"),
        root.join("pyproject.toml"),
        root.join("package.json"),
    ];
    for path in candidates {
        if path.exists() {
            return Ok(path);
        }
    }
    Err(eyre!(
        "No workspace manifest found under {}; expected Magnet.toml, Cargo.toml, pyproject.toml, or package.json",
        root.display()
    ))
}

fn package_to_workspace(package: &PackageNode) -> Result<WorkspacePackage> {
    let modules = discover_modules(package)?;
    let dependencies = package
        .dependencies
        .iter()
        .map(dependency_to_workspace)
        .collect();
    Ok(WorkspacePackage::new(
        package.name.clone(),
        package.manifest_path.to_string_lossy(),
        package.root.to_string_lossy(),
    )
    .with_version(Some(package.version.clone()))
    .with_modules(modules)
    .with_dependencies(dependencies))
}

fn dependency_to_workspace(dep: &DependencyEdge) -> WorkspaceDependency {
    WorkspaceDependency::new(dep.name.clone(), None)
}

fn discover_modules(package: &PackageNode) -> Result<Vec<WorkspaceModule>> {
    let mut modules = Vec::new();
    let mut seen_paths: HashSet<Vec<String>> = HashSet::new();
    for module_root in &package.module_roots {
        if !module_root.is_dir() {
            return Err(eyre!(
                "Module root {} does not exist for package {}",
                module_root.display(),
                package.name
            ));
        }
        collect_modules_from_root(package, module_root, &mut modules, &mut seen_paths)?;
    }
    Ok(modules)
}

fn collect_modules_from_root(
    package: &PackageNode,
    root: &Path,
    modules: &mut Vec<WorkspaceModule>,
    seen_paths: &mut HashSet<Vec<String>>,
) -> Result<()> {
    let mut queue = VecDeque::new();
    queue.push_back(root.to_path_buf());
    while let Some(dir) = queue.pop_front() {
        let entries = std::fs::read_dir(&dir)
            .with_context(|| format!("Failed to read directory {}", dir.display()))?;
        for entry in entries {
            let entry = entry.with_context(|| {
                format!("Failed to read directory entry under {}", dir.display())
            })?;
            let path = entry.path();
            if path.is_dir() {
                if should_skip_dir(&path) {
                    continue;
                }
                queue.push_back(path);
                continue;
            }
            let Some(language) = module_language_for_path(&path) else {
                continue;
            };
            let module_path = module_path_for_file(root, &path)?;
            if !seen_paths.insert(module_path.clone()) {
                return Err(eyre!(
                    "Duplicate module path {} in package {}",
                    module_path.join("::"),
                    package.name
                ));
            }
            let id = if module_path.is_empty() {
                package.name.clone()
            } else {
                format!("{}::{}", package.name, module_path.join("::"))
            };
            modules.push(
                WorkspaceModule::new(id, path.to_string_lossy())
                    .with_module_path(module_path)
                    .with_language(Some(language.to_string())),
            );
        }
    }
    Ok(())
}

fn should_skip_dir(path: &Path) -> bool {
    let Some(name) = path.file_name().and_then(|name| name.to_str()) else {
        return true;
    };
    matches!(name, ".git" | "target" | "node_modules" | "dist" | "build")
}

fn module_language_for_path(path: &Path) -> Option<&'static str> {
    match path.extension().and_then(|ext| ext.to_str()) {
        Some("fp") => Some("ferro"),
        Some("rs") => Some("rust"),
        Some("ts") | Some("tsx") => Some("typescript"),
        Some("js") | Some("mjs") | Some("cjs") => Some("javascript"),
        Some("py") => Some("python"),
        _ => None,
    }
}

fn module_path_for_file(root: &Path, file_path: &Path) -> Result<Vec<String>> {
    let rel = file_path.strip_prefix(root).with_context(|| {
        format!(
            "Module file {} is not under module root {}",
            file_path.display(),
            root.display()
        )
    })?;
    let Some(file_name) = rel.file_name().and_then(|name| name.to_str()) else {
        return Err(eyre!(
            "Module file {} has no file name",
            file_path.display()
        ));
    };
    let stem = Path::new(file_name)
        .file_stem()
        .and_then(|stem| stem.to_str())
        .ok_or_else(|| eyre!("Module file {} has no stem", file_path.display()))?;
    let mut segments = Vec::new();
    if let Some(parent) = rel.parent() {
        for component in parent.components() {
            let name = component
                .as_os_str()
                .to_str()
                .ok_or_else(|| eyre!("Module path contains non-utf8 segment"))?;
            if !name.is_empty() {
                segments.push(name.to_string());
            }
        }
    }
    if stem != "mod" {
        segments.push(stem.to_string());
    }
    Ok(segments)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    fn write_file(path: &Path) {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }
        std::fs::write(path, "fn main() {}\n").unwrap();
    }

    #[test]
    fn builds_workspace_document_from_module_roots() -> Result<()> {
        let temp = tempdir()?;
        let root = temp.path();
        std::fs::write(root.join("Magnet.toml"), "[package]\nname = \"demo\"\n")?;
        let src = root.join("src");
        write_file(&src.join("mod.fp"));
        write_file(&src.join("modules/mod.fp"));
        write_file(&src.join("modules/helpers.fp"));
        write_file(&src.join("modules/utils/math.fp"));

        let graph = PackageGraph {
            schema_version: 1,
            root: root.to_path_buf(),
            packages: vec![PackageNode {
                name: "demo".to_string(),
                version: "0.1.0".to_string(),
                root: root.to_path_buf(),
                manifest_path: root.join("Magnet.toml"),
                language: Some("ferro".to_string()),
                checksum: None,
                source: None,
                module_roots: vec![src.clone()],
                entry: Some(src.join("mod.fp")),
                dependencies: Vec::new(),
            }],
            selected_package: None,
            build_options: Default::default(),
        };

        let workspace = build_workspace_document(&graph)?;
        let package = workspace.packages.first().unwrap();
        let mut paths = package
            .modules
            .iter()
            .map(|module| module.module_path.clone())
            .collect::<Vec<_>>();
        paths.sort();
        assert_eq!(paths.len(), 4);
        assert!(paths.contains(&vec![]));
        assert!(paths.contains(&vec!["modules".to_string()]));
        assert!(paths.contains(&vec!["modules".to_string(), "helpers".to_string()]));
        assert!(paths.contains(&vec![
            "modules".to_string(),
            "utils".to_string(),
            "math".to_string()
        ]));
        Ok(())
    }

    #[test]
    fn rejects_duplicate_module_paths() -> Result<()> {
        let temp = tempdir()?;
        let root = temp.path();
        std::fs::write(root.join("Magnet.toml"), "[package]\nname = \"demo\"\n")?;
        let src = root.join("src");
        write_file(&src.join("foo.fp"));
        write_file(&src.join("foo/mod.fp"));

        let graph = PackageGraph {
            schema_version: 1,
            root: root.to_path_buf(),
            packages: vec![PackageNode {
                name: "demo".to_string(),
                version: "0.1.0".to_string(),
                root: root.to_path_buf(),
                manifest_path: root.join("Magnet.toml"),
                language: Some("ferro".to_string()),
                checksum: None,
                source: None,
                module_roots: vec![src.clone()],
                entry: Some(src.join("foo.fp")),
                dependencies: Vec::new(),
            }],
            selected_package: None,
            build_options: Default::default(),
        };

        let err = build_workspace_document(&graph).unwrap_err();
        assert!(err.to_string().contains("Duplicate module path"));
        Ok(())
    }
}
