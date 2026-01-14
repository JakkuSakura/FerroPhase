use crate::models::DependencyModel;
use crate::registry::{RegistryClient, RegistryOptions, ResolvedCrate};
use crate::resolver::types::{
    RegistryDeps, RegistryManifestKey, RegistryReqKey, ResolvedRegistry,
};
use eyre::Result;
use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, Mutex};

pub struct RegistryLoader {
    client: RegistryClient,
    resolved: HashMap<RegistryReqKey, ResolvedCrate>,
    manifests: HashMap<RegistryManifestKey, RegistryDeps>,
}

impl RegistryLoader {
    pub fn new(options: RegistryOptions) -> Result<Self> {
        Ok(Self {
            client: RegistryClient::new(options)?,
            resolved: HashMap::new(),
            manifests: HashMap::new(),
        })
    }

    pub fn resolve(&mut self, name: &str, version_req: Option<&str>) -> Result<ResolvedCrate> {
        let version_req = version_req.ok_or_else(|| eyre::eyre!("missing version for {name}"))?;
        let key = RegistryReqKey {
            name: name.to_string(),
            version_req: version_req.to_string(),
        };
        if let Some(resolved) = self.resolved.get(&key) {
            return Ok(resolved.clone());
        }
        let resolved = self.client.resolve(name, Some(version_req))?;
        self.resolved.insert(key, resolved.clone());
        Ok(resolved)
    }

    pub fn resolve_with_deps(
        &mut self,
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
        &mut self,
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
        &mut self,
        name: &str,
        version: &str,
        checksum: Option<&str>,
    ) -> Result<ResolvedCrate> {
        let key = RegistryReqKey {
            name: name.to_string(),
            version_req: version.to_string(),
        };
        if let Some(resolved) = self.resolved.get(&key) {
            return Ok(resolved.clone());
        }
        let resolved = self.client.resolve_locked(name, version, checksum)?;
        self.resolved.insert(key, resolved.clone());
        Ok(resolved)
    }

    fn load_manifest_deps(
        &mut self,
        name: &str,
        version: &str,
        manifest_path: &Path,
    ) -> Result<RegistryDeps> {
        let key = RegistryManifestKey {
            name: name.to_string(),
            version: version.to_string(),
        };
        if let Some(deps) = self.manifests.get(&key) {
            return Ok(deps.clone());
        }
        let deps = parse_cargo_manifest(manifest_path)?;
        self.manifests.insert(key, deps.clone());
        Ok(deps)
    }
}

#[derive(Clone)]
pub struct RegistryLoaderHandle {
    inner: Arc<Mutex<RegistryLoader>>,
}

impl RegistryLoaderHandle {
    pub fn new(loader: RegistryLoader) -> Self {
        Self {
            inner: Arc::new(Mutex::new(loader)),
        }
    }

    pub async fn resolve_async(
        &self,
        name: String,
        version_req: Option<String>,
    ) -> Result<ResolvedCrate> {
        let handle = self.inner.clone();
        let name_err = name.clone();
        tokio::task::spawn_blocking(move || {
            let mut loader = handle
                .lock()
                .map_err(|_| eyre::eyre!("registry loader lock poisoned"))?;
            loader.resolve(&name, version_req.as_deref())
        })
        .await
        .map_err(|err| eyre::eyre!("failed to resolve crate {name_err}: {err}"))?
    }

    pub async fn resolve_locked_async(
        &self,
        name: String,
        version: String,
        checksum: Option<String>,
    ) -> Result<ResolvedCrate> {
        let handle = self.inner.clone();
        let name_err = name.clone();
        tokio::task::spawn_blocking(move || {
            let mut loader = handle
                .lock()
                .map_err(|_| eyre::eyre!("registry loader lock poisoned"))?;
            loader.resolve_locked(&name, &version, checksum.as_deref())
        })
        .await
        .map_err(|err| eyre::eyre!("failed to resolve crate {name_err}: {err}"))?
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
