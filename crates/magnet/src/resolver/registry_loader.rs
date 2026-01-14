use crate::models::DependencyModel;
use crate::registry::{RegistryClient, RegistryOptions, ResolvedCrate};
use crate::resolver::types::{
    RegistryDeps, RegistryManifestKey, RegistryReqKey, ResolvedRegistry,
};
use eyre::Result;
use parking_lot::RwLock;
use semver::{Version, VersionReq};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::sync::Mutex;

#[derive(Clone)]
pub struct RegistryLoader {
    client: Arc<RegistryClient>,
    cache: Arc<RegistryLoaderCache>,
}

struct RegistryLoaderCache {
    resolved_by_req: RwLock<HashMap<RegistryReqKey, ResolvedCrate>>,
    resolved_by_version: RwLock<HashMap<RegistryManifestKey, ResolvedCrate>>,
    manifests: RwLock<HashMap<RegistryManifestKey, RegistryDeps>>,
    resolved_index: RwLock<ResolvedIndex>,
    resolved_index_path: Option<PathBuf>,
    inflight_req: RwLock<HashMap<RegistryReqKey, Arc<Mutex<()>>>>,
    inflight_manifest: RwLock<HashMap<RegistryManifestKey, Arc<Mutex<()>>>>,
    inflight_index: Mutex<()>,
}

impl RegistryLoader {
    pub fn new(options: RegistryOptions) -> Result<Self> {
        let resolved_index_path = resolved_index_path();
        let resolved_index = load_resolved_index(resolved_index_path.as_ref())?;
        Ok(Self {
            client: Arc::new(RegistryClient::new(options)?),
            cache: Arc::new(RegistryLoaderCache {
                resolved_by_req: RwLock::new(HashMap::new()),
                resolved_by_version: RwLock::new(HashMap::new()),
                manifests: RwLock::new(HashMap::new()),
                resolved_index: RwLock::new(resolved_index),
                resolved_index_path,
                inflight_req: RwLock::new(HashMap::new()),
                inflight_manifest: RwLock::new(HashMap::new()),
                inflight_index: Mutex::new(()),
            }),
        })
    }

    pub fn resolve(&self, name: &str, version_req: Option<&str>) -> Result<ResolvedCrate> {
        let version_req = version_req.ok_or_else(|| eyre::eyre!("missing version for {name}"))?;
        let key = RegistryReqKey {
            name: name.to_string(),
            version_req: version_req.to_string(),
        };
        if let Some(resolved) = self.cache.resolved_by_req.read().get(&key) {
            return Ok(resolved.clone());
        }
        if let Some(resolved) = self.resolve_from_index(name, version_req)? {
            self.cache
                .resolved_by_version
                .write()
                .insert(
                    RegistryManifestKey {
                        name: resolved.name.clone(),
                        version: resolved.version.clone(),
                    },
                    resolved.clone(),
                );
            self.cache.resolved_by_req.write().insert(key.clone(), resolved.clone());
            return Ok(resolved);
        }
        if let Ok(version) = Version::parse(version_req) {
            let resolved = self.resolve_locked(name, &version.to_string(), None)?;
            self.cache
                .resolved_by_version
                .write()
                .insert(
                    RegistryManifestKey {
                        name: resolved.name.clone(),
                        version: resolved.version.clone(),
                    },
                    resolved.clone(),
                );
            self.cache.resolved_by_req.write().insert(key.clone(), resolved.clone());
            self.cache.inflight_req.write().remove(&key);
            self.update_resolved_index_blocking(name, version_req, &resolved)?;
            return Ok(resolved);
        }
        let lock = self.inflight_req_lock(&key);
        let _guard = lock.blocking_lock();
        if let Some(resolved) = self.cache.resolved_by_req.read().get(&key) {
            return Ok(resolved.clone());
        }
        if let Some(resolved) = self.resolve_from_index(name, version_req)? {
            self.cache
                .resolved_by_version
                .write()
                .insert(
                    RegistryManifestKey {
                        name: resolved.name.clone(),
                        version: resolved.version.clone(),
                    },
                    resolved.clone(),
                );
            self.cache.resolved_by_req.write().insert(key.clone(), resolved.clone());
            self.cache.inflight_req.write().remove(&key);
            return Ok(resolved);
        }
        let resolved = self.client.resolve(name, Some(version_req))?;
        self.cache
            .resolved_by_version
            .write()
            .insert(
                RegistryManifestKey {
                    name: resolved.name.clone(),
                    version: resolved.version.clone(),
                },
                resolved.clone(),
            );
        self.cache.resolved_by_req.write().insert(key.clone(), resolved.clone());
        self.cache.inflight_req.write().remove(&key);
        self.update_resolved_index_blocking(name, version_req, &resolved)?;
        Ok(resolved)
    }

    pub fn resolve_with_deps(
        &self,
        name: &str,
        version_req: Option<&str>,
    ) -> Result<ResolvedRegistry> {
        let resolved = self.resolve(name, version_req)?;
        let deps = self.load_manifest_deps(
            &resolved.name,
            &resolved.version,
            &resolved.manifest_path,
        )?;
        Ok(ResolvedRegistry { resolved, deps })
    }

    pub fn resolve_locked_with_deps(
        &self,
        name: &str,
        version: &str,
        checksum: Option<&str>,
    ) -> Result<ResolvedRegistry> {
        let resolved = self.resolve_locked(name, version, checksum)?;
        let deps = self.load_manifest_deps(
            &resolved.name,
            &resolved.version,
            &resolved.manifest_path,
        )?;
        Ok(ResolvedRegistry { resolved, deps })
    }

    pub fn resolve_locked(
        &self,
        name: &str,
        version: &str,
        checksum: Option<&str>,
    ) -> Result<ResolvedCrate> {
        let key = RegistryManifestKey {
            name: name.to_string(),
            version: version.to_string(),
        };
        if let Some(resolved) = self.cache.resolved_by_version.read().get(&key) {
            return Ok(resolved.clone());
        }
        let lock = self.inflight_manifest_lock(&key);
        let _guard = lock.blocking_lock();
        if let Some(resolved) = self.cache.resolved_by_version.read().get(&key) {
            return Ok(resolved.clone());
        }
        let resolved = self.client.resolve_locked(name, version, checksum)?;
        self.cache
            .resolved_by_version
            .write()
            .insert(key.clone(), resolved.clone());
        self.cache.inflight_manifest.write().remove(&key);
        self.update_resolved_index_blocking(name, version, &resolved)?;
        Ok(resolved)
    }

    fn load_manifest_deps(
        &self,
        name: &str,
        version: &str,
        manifest_path: &Path,
    ) -> Result<RegistryDeps> {
        let key = RegistryManifestKey {
            name: name.to_string(),
            version: version.to_string(),
        };
        if let Some(deps) = self.cache.manifests.read().get(&key) {
            return Ok(deps.clone());
        }
        let lock = self.inflight_manifest_lock(&key);
        let _guard = lock.blocking_lock();
        if let Some(deps) = self.cache.manifests.read().get(&key) {
            return Ok(deps.clone());
        }
        let deps = parse_cargo_manifest(manifest_path)?;
        self.cache.manifests.write().insert(key, deps.clone());
        Ok(deps)
    }

    fn inflight_req_lock(&self, key: &RegistryReqKey) -> Arc<Mutex<()>> {
        if let Some(lock) = self.cache.inflight_req.read().get(key) {
            return Arc::clone(lock);
        }
        let mut inflight = self.cache.inflight_req.write();
        inflight
            .entry(key.clone())
            .or_insert_with(|| Arc::new(Mutex::new(())))
            .clone()
    }

    fn inflight_manifest_lock(&self, key: &RegistryManifestKey) -> Arc<Mutex<()>> {
        if let Some(lock) = self.cache.inflight_manifest.read().get(key) {
            return Arc::clone(lock);
        }
        let mut inflight = self.cache.inflight_manifest.write();
        inflight
            .entry(key.clone())
            .or_insert_with(|| Arc::new(Mutex::new(())))
            .clone()
    }

    pub async fn resolve_async(
        &self,
        name: &str,
        version_req: Option<&str>,
    ) -> Result<ResolvedCrate> {
        let version_req = version_req.ok_or_else(|| eyre::eyre!("missing version for {name}"))?;
        let key = RegistryReqKey {
            name: name.to_string(),
            version_req: version_req.to_string(),
        };
        if let Some(resolved) = self.cache.resolved_by_req.read().get(&key) {
            return Ok(resolved.clone());
        }
        if let Some(resolved) = self.resolve_from_index_async(name, version_req).await? {
            self.cache
                .resolved_by_version
                .write()
                .insert(
                    RegistryManifestKey {
                        name: resolved.name.clone(),
                        version: resolved.version.clone(),
                    },
                    resolved.clone(),
                );
            self.cache.resolved_by_req.write().insert(key.clone(), resolved.clone());
            return Ok(resolved);
        }
        if let Ok(version) = Version::parse(version_req) {
            let resolved = self.resolve_locked_async(name, &version.to_string(), None).await?;
            self.cache
                .resolved_by_version
                .write()
                .insert(
                    RegistryManifestKey {
                        name: resolved.name.clone(),
                        version: resolved.version.clone(),
                    },
                    resolved.clone(),
                );
            self.cache.resolved_by_req.write().insert(key.clone(), resolved.clone());
            self.cache.inflight_req.write().remove(&key);
        self.update_resolved_index_blocking(name, version_req, &resolved)?;
            return Ok(resolved);
        }
        let lock = self.inflight_req_lock(&key);
        let _guard = lock.lock().await;
        if let Some(resolved) = self.cache.resolved_by_req.read().get(&key) {
            return Ok(resolved.clone());
        }
        if let Some(resolved) = self.resolve_from_index_async(name, version_req).await? {
            self.cache
                .resolved_by_version
                .write()
                .insert(
                    RegistryManifestKey {
                        name: resolved.name.clone(),
                        version: resolved.version.clone(),
                    },
                    resolved.clone(),
                );
            self.cache.resolved_by_req.write().insert(key.clone(), resolved.clone());
            self.cache.inflight_req.write().remove(&key);
            return Ok(resolved);
        }
        let resolved = self.client.resolve_async(name, Some(version_req)).await?;
        self.cache
            .resolved_by_version
            .write()
            .insert(
                RegistryManifestKey {
                    name: resolved.name.clone(),
                    version: resolved.version.clone(),
                },
                resolved.clone(),
            );
        self.cache.resolved_by_req.write().insert(key.clone(), resolved.clone());
        self.cache.inflight_req.write().remove(&key);
        self.update_resolved_index_async(name, version_req, &resolved).await?;
        Ok(resolved)
    }

    pub async fn resolve_locked_async(
        &self,
        name: &str,
        version: &str,
        checksum: Option<&str>,
    ) -> Result<ResolvedCrate> {
        let key = RegistryManifestKey {
            name: name.to_string(),
            version: version.to_string(),
        };
        if let Some(resolved) = self.cache.resolved_by_version.read().get(&key) {
            return Ok(resolved.clone());
        }
        let lock = self.inflight_manifest_lock(&key);
        let _guard = lock.lock().await;
        if let Some(resolved) = self.cache.resolved_by_version.read().get(&key) {
            return Ok(resolved.clone());
        }
        let resolved = self
            .client
            .resolve_locked_async(name, version, checksum)
            .await?;
        self.cache
            .resolved_by_version
            .write()
            .insert(key.clone(), resolved.clone());
        self.cache.inflight_manifest.write().remove(&key);
        self.update_resolved_index_async(name, version, &resolved).await?;
        Ok(resolved)
    }

    fn resolve_from_index(&self, name: &str, version_req: &str) -> Result<Option<ResolvedCrate>> {
        let index = self.cache.resolved_index.read();
        let Some(entries) = index.entries.get(name) else {
            return Ok(None);
        };
        let Some(entry) = entries.get(version_req) else {
            return Ok(None);
        };
        let req = VersionReq::parse(version_req)
            .map_err(|err| eyre::eyre!("invalid version requirement '{version_req}': {err}"))?;
        let version = Version::parse(&entry.version)
            .map_err(|err| eyre::eyre!("invalid cached version '{}': {err}", entry.version))?;
        if !req.matches(&version) {
            return Ok(None);
        }
        let resolved = self.resolve_locked(name, &entry.version, entry.checksum.as_deref())?;
        Ok(Some(resolved))
    }

    async fn resolve_from_index_async(
        &self,
        name: &str,
        version_req: &str,
    ) -> Result<Option<ResolvedCrate>> {
        let entry = {
            let index = self.cache.resolved_index.read();
            let Some(entries) = index.entries.get(name) else {
                return Ok(None);
            };
            let Some(entry) = entries.get(version_req) else {
                return Ok(None);
            };
            entry.clone()
        };
        let req = VersionReq::parse(version_req)
            .map_err(|err| eyre::eyre!("invalid version requirement '{version_req}': {err}"))?;
        let version = Version::parse(&entry.version)
            .map_err(|err| eyre::eyre!("invalid cached version '{}': {err}", entry.version))?;
        if !req.matches(&version) {
            return Ok(None);
        }
        let resolved = self
            .resolve_locked_async(name, &entry.version, entry.checksum.as_deref())
            .await?;
        Ok(Some(resolved))
    }

    fn update_resolved_index_blocking(
        &self,
        name: &str,
        version_req: &str,
        resolved: &ResolvedCrate,
    ) -> Result<()> {
        let mut index = self.cache.resolved_index.write();
        let entry = ResolvedIndexEntry {
            version: resolved.version.clone(),
            checksum: resolved.checksum.clone(),
        };
        index
            .entries
            .entry(name.to_string())
            .or_default()
            .insert(version_req.to_string(), entry);
        drop(index);
        self.write_resolved_index_blocking()
    }

    async fn update_resolved_index_async(
        &self,
        name: &str,
        version_req: &str,
        resolved: &ResolvedCrate,
    ) -> Result<()> {
        {
            let mut index = self.cache.resolved_index.write();
            let entry = ResolvedIndexEntry {
                version: resolved.version.clone(),
                checksum: resolved.checksum.clone(),
            };
            index
                .entries
                .entry(name.to_string())
                .or_default()
                .insert(version_req.to_string(), entry);
        }
        self.write_resolved_index_async().await
    }

    fn write_resolved_index_blocking(&self) -> Result<()> {
        let Some(path) = self.cache.resolved_index_path.as_ref() else {
            return Ok(());
        };
        let _guard = self.cache.inflight_index.blocking_lock();
        let payload = {
            let index = self.cache.resolved_index.read();
            serde_json::to_string(&*index).map_err(|err| eyre::eyre!("failed to serialize registry cache: {err}"))?
        };
        write_atomic(path, payload)?;
        Ok(())
    }

    async fn write_resolved_index_async(&self) -> Result<()> {
        let Some(path) = self.cache.resolved_index_path.clone() else {
            return Ok(());
        };
        let _guard = self.cache.inflight_index.lock().await;
        let payload = {
            let index = self.cache.resolved_index.read();
            serde_json::to_string(&*index).map_err(|err| eyre::eyre!("failed to serialize registry cache: {err}"))?
        };
        tokio::task::spawn_blocking(move || write_atomic(&path, payload))
            .await
            .map_err(|err| eyre::eyre!("failed to write registry cache: {err}"))??;
        Ok(())
    }
}

#[derive(Clone)]
pub struct RegistryLoaderHandle {
    loader: RegistryLoader,
}

impl RegistryLoaderHandle {
    pub fn new(loader: RegistryLoader) -> Self {
        Self { loader }
    }

    pub async fn resolve_async(
        &self,
        name: String,
        version_req: Option<String>,
    ) -> Result<ResolvedCrate> {
        let name_err = name.clone();
        self.loader
            .resolve_async(&name, version_req.as_deref())
            .await
            .map_err(|err| eyre::eyre!("failed to resolve crate {name_err}: {err}"))
    }

    pub async fn resolve_locked_async(
        &self,
        name: String,
        version: String,
        checksum: Option<String>,
    ) -> Result<ResolvedCrate> {
        let name_err = name.clone();
        self.loader
            .resolve_locked_async(&name, &version, checksum.as_deref())
            .await
            .map_err(|err| eyre::eyre!("failed to resolve crate {name_err}: {err}"))
    }
}

fn parse_cargo_manifest(path: &Path) -> Result<RegistryDeps> {
    let content = std::fs::read_to_string(path)?;
    let value: toml::Value = toml::from_str(&content)?;
    let dependencies = parse_cargo_deps(value.get("dependencies"));
    let dev_dependencies = parse_cargo_deps(value.get("dev-dependencies"));
    let build_dependencies = parse_cargo_deps(value.get("build-dependencies"));
    Ok(RegistryDeps {
        dependencies,
        dev_dependencies,
        build_dependencies,
    })
}

fn resolved_index_path() -> Option<PathBuf> {
    let cache_dir = resolve_cache_dir()?;
    Some(cache_dir.join("registry").join("resolved-index.json"))
}

fn resolve_cache_dir() -> Option<PathBuf> {
    if let Some(cache_dir) = std::env::var_os("MAGNET_CACHE_DIR") {
        return Some(PathBuf::from(cache_dir));
    }
    let home = std::env::var_os("HOME")
        .or_else(|| std::env::var_os("USERPROFILE"))
        .map(PathBuf::from)?;
    Some(home.join(".cache").join("magnet"))
}

#[derive(Debug, Default, Serialize, Deserialize)]
struct ResolvedIndex {
    entries: HashMap<String, HashMap<String, ResolvedIndexEntry>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ResolvedIndexEntry {
    version: String,
    checksum: Option<String>,
}

fn load_resolved_index(path: Option<&PathBuf>) -> Result<ResolvedIndex> {
    let Some(path) = path else {
        return Ok(ResolvedIndex::default());
    };
    if !path.exists() {
        return Ok(ResolvedIndex::default());
    }
    let content = std::fs::read_to_string(path)?;
    let index = serde_json::from_str(&content)
        .map_err(|err| eyre::eyre!("failed to parse registry cache: {err}"))?;
    Ok(index)
}

fn write_atomic(path: &Path, payload: String) -> Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let tmp_path = path.with_extension(format!("tmp.{}", std::process::id()));
    std::fs::write(&tmp_path, payload)?;
    std::fs::rename(&tmp_path, path)?;
    Ok(())
}

fn parse_cargo_deps(section: Option<&toml::Value>) -> HashMap<String, DependencyModel> {
    let mut out = HashMap::new();
    let Some(table) = section.and_then(|v| v.as_table()) else {
        return out;
    };
    for (name, value) in table {
        let mut model = DependencyModel::default();
        match value {
            toml::Value::String(version) => {
                model.version = Some(version.to_string());
            }
            toml::Value::Table(table) => {
                if let Some(version) = table.get("version").and_then(|v| v.as_str()) {
                    model.version = Some(version.to_string());
                }
                if let Some(path) = table.get("path").and_then(|v| v.as_str()) {
                    model.path = Some(Path::new(path).to_path_buf());
                }
                if let Some(git) = table.get("git").and_then(|v| v.as_str()) {
                    model.git = Some(git.to_string());
                }
                if let Some(branch) = table.get("branch").and_then(|v| v.as_str()) {
                    model.branch = Some(branch.to_string());
                }
                if let Some(tag) = table.get("tag").and_then(|v| v.as_str()) {
                    model.tag = Some(tag.to_string());
                }
                if let Some(rev) = table.get("rev").and_then(|v| v.as_str()) {
                    model.rev = Some(rev.to_string());
                }
                if let Some(features) = table.get("features").and_then(|v| v.as_array()) {
                    let list = features
                        .iter()
                        .filter_map(|v| v.as_str())
                        .map(|s| s.to_string())
                        .collect::<Vec<_>>();
                    if !list.is_empty() {
                        model.features = Some(list);
                    }
                }
                if let Some(default_features) = table.get("default-features").and_then(|v| v.as_bool()) {
                    model.default_features = Some(default_features);
                }
                if let Some(optional) = table.get("optional").and_then(|v| v.as_bool()) {
                    model.optional = Some(optional);
                }
                if let Some(package) = table.get("package").and_then(|v| v.as_str()) {
                    model.package = Some(package.to_string());
                }
                if let Some(registry) = table.get("registry").and_then(|v| v.as_str()) {
                    model.registry = Some(registry.to_string());
                }
            }
            _ => {}
        }
        out.insert(name.to_string(), model);
    }
    out
}
