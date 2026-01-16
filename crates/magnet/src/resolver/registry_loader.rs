use crate::models::parse_cargo_dependencies;
use crate::registry::{RegistryClient, RegistryOptions, ResolvedCrate};
use crate::resolver::types::{RegistryDeps, RegistryManifestKey, RegistryReqKey, ResolvedRegistry};
use eyre::Result;
use parking_lot::RwLock;
use semver::VersionReq;
use std::collections::HashMap;
use std::path::Path;
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
    inflight_req: RwLock<HashMap<RegistryReqKey, Arc<Mutex<()>>>>,
    inflight_manifest: RwLock<HashMap<RegistryManifestKey, Arc<Mutex<()>>>>,
}

impl RegistryLoader {
    pub fn new(options: RegistryOptions) -> Result<Self> {
        Ok(Self {
            client: Arc::new(RegistryClient::new(options)?),
            cache: Arc::new(RegistryLoaderCache {
                resolved_by_req: RwLock::new(HashMap::new()),
                resolved_by_version: RwLock::new(HashMap::new()),
                manifests: RwLock::new(HashMap::new()),
                inflight_req: RwLock::new(HashMap::new()),
                inflight_manifest: RwLock::new(HashMap::new()),
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
        let lock = self.inflight_req_lock(&key);
        let _guard = lock.blocking_lock();
        if let Some(resolved) = self.cache.resolved_by_req.read().get(&key) {
            return Ok(resolved.clone());
        }
        let resolved = self.client.resolve(name, Some(version_req))?;
        self.cache.resolved_by_version.write().insert(
            RegistryManifestKey {
                name: resolved.name.clone(),
                version: resolved.version.clone(),
            },
            resolved.clone(),
        );
        self.cache
            .resolved_by_req
            .write()
            .insert(key.clone(), resolved.clone());
        self.cache.inflight_req.write().remove(&key);
        Ok(resolved)
    }

    pub fn resolve_with_deps(
        &self,
        name: &str,
        version_req: Option<&str>,
    ) -> Result<ResolvedRegistry> {
        let resolved = self.resolve(name, version_req)?;
        let deps =
            self.load_manifest_deps(&resolved.name, &resolved.version, &resolved.manifest_path)?;
        Ok(ResolvedRegistry { resolved, deps })
    }

    pub fn resolve_locked_with_deps(
        &self,
        name: &str,
        version: &str,
        checksum: Option<&str>,
    ) -> Result<ResolvedRegistry> {
        let resolved = self.resolve_locked(name, version, checksum)?;
        let deps =
            self.load_manifest_deps(&resolved.name, &resolved.version, &resolved.manifest_path)?;
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
        let lock = self.inflight_req_lock(&key);
        let _guard = lock.lock().await;
        if let Some(resolved) = self.cache.resolved_by_req.read().get(&key) {
            return Ok(resolved.clone());
        }
        let resolved = self.client.resolve_async(name, Some(version_req)).await?;
        self.cache.resolved_by_version.write().insert(
            RegistryManifestKey {
                name: resolved.name.clone(),
                version: resolved.version.clone(),
            },
            resolved.clone(),
        );
        self.cache
            .resolved_by_req
            .write()
            .insert(key.clone(), resolved.clone());
        self.cache.inflight_req.write().remove(&key);
        Ok(resolved)
    }

    pub async fn resolve_with_reqs_async(
        &self,
        name: &str,
        reqs: &[VersionReq],
    ) -> Result<ResolvedCrate> {
        let resolved = self.client.resolve_with_reqs_async(name, reqs).await?;
        self.cache.resolved_by_version.write().insert(
            RegistryManifestKey {
                name: resolved.name.clone(),
                version: resolved.version.clone(),
            },
            resolved.clone(),
        );
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
        Ok(resolved)
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

    pub async fn resolve_with_reqs_async(
        &self,
        name: String,
        reqs: Vec<VersionReq>,
    ) -> Result<ResolvedCrate> {
        let name_err = name.clone();
        self.loader
            .resolve_with_reqs_async(&name, &reqs)
            .await
            .map_err(|err| eyre::eyre!("failed to resolve crate {name_err}: {err}"))
    }
}

fn parse_cargo_manifest(path: &Path) -> Result<RegistryDeps> {
    let deps = parse_cargo_dependencies(path)?;
    Ok(RegistryDeps {
        dependencies: deps.dependencies,
        dev_dependencies: deps.dev_dependencies,
        build_dependencies: deps.build_dependencies,
        target_dependencies: deps.target_dependencies,
    })
}
