use crate::models::{
    DependencyModel, LockIndex, MagnetLock, ManifestModel, PackageGraph, PackageGraphOptions,
};
use eyre::Result;
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::fs;
use std::path::Path;
use std::path::PathBuf;

pub fn load_manifest(path: &Path) -> Result<ManifestModel> {
    ManifestModel::from_dir(path)
}

pub fn resolve_graph(path: &Path, options: &PackageGraphOptions) -> Result<PackageGraph> {
    let root = resolve_root(path);
    let (options, fingerprint) = adjust_options_for_deps(&root, options)?;
    let graph = PackageGraph::from_path_with_options(path, &options)?;
    if let Some(fingerprint) = fingerprint {
        write_deps_fingerprint(&root, &fingerprint)?;
    }
    Ok(graph)
}

pub fn load_lock_index(root: &Path) -> Result<Option<LockIndex>> {
    let lock_path = root.join("Magnet.lock");
    let lock = MagnetLock::read_from_path(&lock_path)?;
    Ok(lock.as_ref().map(LockIndex::from_lock))
}

fn adjust_options_for_deps(
    root: &Path,
    options: &PackageGraphOptions,
) -> Result<(PackageGraphOptions, Option<String>)> {
    if !options.use_lock {
        return Ok((options.clone(), None));
    }
    if !root.join("Magnet.toml").exists() && !root.join("Cargo.toml").exists() {
        return Ok((options.clone(), None));
    }
    let manifest = load_manifest(root)?;
    let fingerprint = deps_fingerprint(&manifest)?;
    if deps_fingerprint_matches(root, &fingerprint)? {
        let mut adjusted = options.clone();
        adjusted.offline = true;
        adjusted.resolve_registry = false;
        adjusted.write_lock = false;
        adjusted.cargo_fetch = false;
        return Ok((adjusted, None));
    }
    if options.offline {
        return Ok((options.clone(), None));
    }
    Ok((options.clone(), Some(fingerprint)))
}

fn resolve_root(path: &Path) -> PathBuf {
    if path.is_dir() {
        return path.to_path_buf();
    }
    path.parent()
        .map(|path| path.to_path_buf())
        .unwrap_or_else(|| PathBuf::from("."))
}

fn deps_fingerprint_matches(root: &Path, fingerprint: &str) -> Result<bool> {
    let path = deps_fingerprint_path(root)?;
    if !path.exists() {
        return Ok(false);
    }
    let existing = fs::read_to_string(&path)?;
    Ok(existing.trim() == fingerprint)
}

fn write_deps_fingerprint(root: &Path, fingerprint: &str) -> Result<()> {
    let path = deps_fingerprint_path(root)?;
    write_atomic(&path, fingerprint)?;
    Ok(())
}

fn deps_fingerprint_path(root: &Path) -> Result<PathBuf> {
    let cache_dir = resolve_cache_dir()?;
    let root_path = root
        .canonicalize()
        .unwrap_or_else(|_| root.to_path_buf());
    let root_hash = hex::encode(Sha256::digest(root_path.to_string_lossy().as_bytes()));
    Ok(cache_dir.join("deps").join(format!("{root_hash}.txt")))
}

fn resolve_cache_dir() -> Result<PathBuf> {
    if let Some(cache_dir) = std::env::var_os("MAGNET_CACHE_DIR") {
        return Ok(PathBuf::from(cache_dir));
    }
    let home = std::env::var_os("HOME")
        .or_else(|| std::env::var_os("USERPROFILE"))
        .ok_or_else(|| eyre::eyre!("could not resolve home directory"))?;
    Ok(PathBuf::from(home).join(".cache").join("magnet"))
}

fn deps_fingerprint(manifest: &ManifestModel) -> Result<String> {
    let mut hasher = Sha256::new();
    let mut packages = manifest.list_packages()?;
    packages.sort_by(|a, b| a.name.cmp(&b.name).then(a.source_path.cmp(&b.source_path)));
    for package in packages {
        hash_package_deps(&mut hasher, "dep", &package.dependencies);
        hash_package_deps(&mut hasher, "dev", &package.dev_dependencies);
        hash_package_deps(&mut hasher, "build", &package.build_dependencies);
    }
    Ok(hex::encode(hasher.finalize()))
}

fn hash_package_deps(
    hasher: &mut Sha256,
    kind: &str,
    deps: &std::collections::HashMap<String, DependencyModel>,
) {
    let mut entries: Vec<(&String, &DependencyModel)> = deps.iter().collect();
    entries.sort_by(|a, b| a.0.cmp(b.0));
    for (name, model) in entries {
        hasher.update(kind.as_bytes());
        hasher.update(b":");
        hasher.update(name.as_bytes());
        hasher.update(b":");
        let canonical = canonical_dependency(model);
        hasher.update(canonical.as_bytes());
        hasher.update(b";");
    }
}

fn canonical_dependency(model: &DependencyModel) -> String {
    let mut map: BTreeMap<&'static str, Option<String>> = BTreeMap::new();
    map.insert("version", model.version.clone());
    map.insert(
        "path",
        model.path.as_ref().and_then(|p| p.to_str().map(|s| s.to_string())),
    );
    map.insert("nexus", model.nexus.map(|v| v.to_string()));
    map.insert("git", model.git.clone());
    map.insert("branch", model.branch.clone());
    map.insert("tag", model.tag.clone());
    map.insert("rev", model.rev.clone());
    map.insert(
        "features",
        model.features.as_ref().map(|v| v.join(",")),
    );
    map.insert(
        "default_features",
        model.default_features.map(|v| v.to_string()),
    );
    map.insert("workspace", model.workspace.map(|v| v.to_string()));
    map.insert("optional", model.optional.map(|v| v.to_string()));
    map.insert("package", model.package.clone());
    map.insert("registry", model.registry.clone());
    map.insert("artifact", model.artifact.clone());
    map.insert("target", model.target.clone());
    let mut out = String::new();
    for (key, value) in map {
        if let Some(value) = value {
            out.push_str(key);
            out.push('=');
            out.push_str(&value);
            out.push(';');
        }
    }
    if !model.custom.is_empty() {
        let mut custom: Vec<_> = model.custom.iter().collect();
        custom.sort_by(|a, b| a.0.cmp(b.0));
        for (key, value) in custom {
            out.push_str("custom.");
            out.push_str(key);
            out.push('=');
            out.push_str(&format!("{value:?}"));
            out.push(';');
        }
    }
    out
}

fn write_atomic(path: &Path, payload: &str) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    let tmp_path = path.with_extension(format!("tmp.{}", std::process::id()));
    fs::write(&tmp_path, payload)?;
    fs::rename(&tmp_path, path)?;
    Ok(())
}
