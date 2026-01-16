use crates_index::SparseIndex;
use futures_util::StreamExt;
use http;
use eyre::Result;
use flate2::read::GzDecoder;
use parking_lot::RwLock;
use reqwest::Client;
use semver::{Version, VersionReq};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::ffi::OsString;
use std::fs::{self, File};
use std::io::{Read, Write};
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;
use tokio::io::AsyncWriteExt;

#[derive(Debug, Clone)]
pub struct RegistryOptions {
    pub offline: bool,
    pub cache_dir: Option<PathBuf>,
    pub refresh_index: bool,
}

#[derive(Debug, Clone)]
pub struct ResolvedCrate {
    pub name: String,
    pub version: String,
    pub checksum: Option<String>,
    pub root: PathBuf,
    pub manifest_path: PathBuf,
}

pub struct RegistryClient {
    options: RegistryOptions,
    cache_dir: PathBuf,
    index: Arc<SparseIndex>,
    agent: Arc<ureq::Agent>,
    crate_cache: Arc<RwLock<HashMap<String, Arc<crates_index::Crate>>>>,
    http_client: Client,
    _cargo_home: CargoHomeGuard,
}

impl RegistryClient {
    pub fn new(options: RegistryOptions) -> Result<Self> {
        let cache_dir = resolve_cache_dir(options.cache_dir.clone())?;
        fs::create_dir_all(&cache_dir)?;
        let cargo_home = CargoHomeGuard::apply(&cache_dir)?;
        let index = SparseIndex::new_cargo_default()?;
        Ok(Self {
            options,
            cache_dir,
            index: Arc::new(index),
            agent: Arc::new(ureq::Agent::new()),
            crate_cache: Arc::new(RwLock::new(HashMap::new())),
            http_client: Client::new(),
            _cargo_home: cargo_home,
        })
    }

    pub fn resolve(&self, name: &str, version_req: Option<&str>) -> Result<ResolvedCrate> {
        let req_str = version_req.ok_or_else(|| eyre::eyre!("missing version for {name}"))?;
        let req = VersionReq::parse(req_str)
            .map_err(|err| eyre::eyre!("invalid version requirement '{req_str}': {err}"))?;

        let (version, checksum) = resolve_version_and_checksum(
            &self.options,
            &self.index,
            &self.agent,
            &self.crate_cache,
            name,
            &req,
        )?;

        let crate_root = self
            .cache_dir
            .join("registry")
            .join("crates")
            .join(name)
            .join(&version);
        let manifest_path = crate_root.join("Cargo.toml");

        if manifest_path.exists() {
            return Ok(ResolvedCrate {
                name: name.to_string(),
                version,
                checksum,
                root: crate_root,
                manifest_path,
            });
        }

        if self.options.offline {
            return Err(eyre::eyre!(
                "crate '{name}' {version} not available offline"
            ));
        }

        download_and_unpack(
            &self.agent,
            name,
            &version,
            checksum.as_deref(),
            &crate_root,
        )?;

        Ok(ResolvedCrate {
            name: name.to_string(),
            version,
            checksum,
            root: crate_root,
            manifest_path,
        })
    }

    pub async fn resolve_async(&self, name: &str, version_req: Option<&str>) -> Result<ResolvedCrate> {
        let req_str = version_req.ok_or_else(|| eyre::eyre!("missing version for {name}"))?;
        let req = VersionReq::parse(req_str)
            .map_err(|err| eyre::eyre!("invalid version requirement '{req_str}': {err}"))?;
        let options = self.options.clone();
        let name_owned = name.to_string();
        let req_owned = req.clone();
        let index = Arc::clone(&self.index);
        let agent = Arc::clone(&self.agent);
        let crate_cache = Arc::clone(&self.crate_cache);
        let (version, checksum) = tokio::task::spawn_blocking(move || {
            resolve_version_and_checksum(
                &options,
                &index,
                &agent,
                &crate_cache,
                &name_owned,
                &req_owned,
            )
        })
        .await
        .map_err(|err| eyre::eyre!("failed to resolve crate {name}: {err}"))??;

        let crate_root = self
            .cache_dir
            .join("registry")
            .join("crates")
            .join(name)
            .join(&version);
        let manifest_path = crate_root.join("Cargo.toml");

        if manifest_path.exists() {
            return Ok(ResolvedCrate {
                name: name.to_string(),
                version,
                checksum,
                root: crate_root,
                manifest_path,
            });
        }

        if self.options.offline {
            return Err(eyre::eyre!(
                "crate '{name}' {version} not available offline"
            ));
        }

        download_and_unpack_async(
            &self.http_client,
            name,
            &version,
            checksum.as_deref(),
            &crate_root,
        )
        .await?;

        Ok(ResolvedCrate {
            name: name.to_string(),
            version,
            checksum,
            root: crate_root,
            manifest_path,
        })
    }

    pub fn resolve_with_reqs(&self, name: &str, reqs: &[VersionReq]) -> Result<ResolvedCrate> {
        if reqs.is_empty() {
            return Err(eyre::eyre!("missing version requirements for {name}"));
        }
        let (version, checksum) = resolve_version_and_checksum_with_reqs(
            &self.options,
            &self.index,
            &self.agent,
            &self.crate_cache,
            name,
            reqs,
        )?;
        self.resolve_locked(name, &version, checksum.as_deref())
    }

    pub async fn resolve_with_reqs_async(
        &self,
        name: &str,
        reqs: &[VersionReq],
    ) -> Result<ResolvedCrate> {
        if reqs.is_empty() {
            return Err(eyre::eyre!("missing version requirements for {name}"));
        }
        let options = self.options.clone();
        let name_owned = name.to_string();
        let reqs_owned = reqs.to_vec();
        let index = Arc::clone(&self.index);
        let agent = Arc::clone(&self.agent);
        let crate_cache = Arc::clone(&self.crate_cache);
        let (version, checksum) = tokio::task::spawn_blocking(move || {
            resolve_version_and_checksum_with_reqs(
                &options,
                &index,
                &agent,
                &crate_cache,
                &name_owned,
                &reqs_owned,
            )
        })
        .await
        .map_err(|err| eyre::eyre!("failed to resolve crate {name}: {err}"))??;
        self.resolve_locked_async(name, &version, checksum.as_deref()).await
    }

    pub async fn resolve_locked_async(
        &self,
        name: &str,
        version: &str,
        checksum: Option<&str>,
    ) -> Result<ResolvedCrate> {
        let crate_root = self
            .cache_dir
            .join("registry")
            .join("crates")
            .join(name)
            .join(version);
        let manifest_path = crate_root.join("Cargo.toml");

        if manifest_path.exists() {
            return Ok(ResolvedCrate {
                name: name.to_string(),
                version: version.to_string(),
                checksum: checksum.map(|value| value.to_string()),
                root: crate_root,
                manifest_path,
            });
        }

        if self.options.offline {
            return Err(eyre::eyre!(
                "crate '{name}' {version} not available offline"
            ));
        }

        download_and_unpack_async(
            &self.http_client,
            name,
            version,
            checksum,
            &crate_root,
        )
        .await?;

        Ok(ResolvedCrate {
            name: name.to_string(),
            version: version.to_string(),
            checksum: checksum.map(|value| value.to_string()),
            root: crate_root,
            manifest_path,
        })
    }

    pub fn resolve_locked(
        &self,
        name: &str,
        version: &str,
        checksum: Option<&str>,
    ) -> Result<ResolvedCrate> {
        let crate_root = self
            .cache_dir
            .join("registry")
            .join("crates")
            .join(name)
            .join(version);
        let manifest_path = crate_root.join("Cargo.toml");

        if manifest_path.exists() {
            return Ok(ResolvedCrate {
                name: name.to_string(),
                version: version.to_string(),
                checksum: checksum.map(|value| value.to_string()),
                root: crate_root,
                manifest_path,
            });
        }

        if self.options.offline {
            return Err(eyre::eyre!(
                "crate '{name}' {version} not available offline"
            ));
        }

        download_and_unpack(&self.agent, name, version, checksum, &crate_root)?;

        Ok(ResolvedCrate {
            name: name.to_string(),
            version: version.to_string(),
            checksum: checksum.map(|value| value.to_string()),
            root: crate_root,
            manifest_path,
        })
    }
}

fn select_version(krate: &crates_index::Crate, req: &VersionReq) -> Result<(String, Option<String>)> {
    let mut candidates: Vec<(Version, Option<String>)> = krate
        .versions()
        .iter()
        .filter(|v| !v.is_yanked())
        .filter_map(|v| {
            let parsed = Version::parse(&v.version()).ok()?;
            if req.matches(&parsed) {
                Some((parsed, Some(hex::encode(v.checksum()))))
            } else {
                None
            }
        })
        .collect();

    candidates.sort_by(|a, b| b.0.cmp(&a.0));
    let Some((version, checksum)) = candidates.first() else {
        return Err(eyre::eyre!(
            "no versions matching {} for crate {}",
            req,
            krate.name()
        ));
    };

    Ok((version.to_string(), checksum.clone()))
}

fn select_version_with_reqs(
    krate: &crates_index::Crate,
    reqs: &[VersionReq],
) -> Result<(String, Option<String>)> {
    let mut candidates: Vec<(Version, Option<String>)> = krate
        .versions()
        .iter()
        .filter(|v| !v.is_yanked())
        .filter_map(|v| {
            let parsed = Version::parse(&v.version()).ok()?;
            if reqs.iter().all(|req| req.matches(&parsed)) {
                Some((parsed, Some(hex::encode(v.checksum()))))
            } else {
                None
            }
        })
        .collect();

    candidates.sort_by(|a, b| b.0.cmp(&a.0));
    let Some((version, checksum)) = candidates.first() else {
        return Err(eyre::eyre!(
            "no versions matching requirements for crate {}",
            krate.name()
        ));
    };

    Ok((version.to_string(), checksum.clone()))
}

fn resolve_version_and_checksum_with_reqs(
    options: &RegistryOptions,
    index: &SparseIndex,
    agent: &ureq::Agent,
    crate_cache: &Arc<RwLock<HashMap<String, Arc<crates_index::Crate>>>>,
    name: &str,
    reqs: &[VersionReq],
) -> Result<(String, Option<String>)> {
    if !options.refresh_index {
        if let Some(krate) = crate_cache.read().get(name).cloned() {
            return select_version_with_reqs(krate.as_ref(), reqs);
        }
    }

    let krate = if options.offline {
        index
            .crate_from_cache(name)
            .map_err(|err| eyre::eyre!("crate '{name}' not available offline: {err}"))?
    } else {
        fetch_sparse_crate(agent, index, name)?
    };
    let krate = Arc::new(krate);
    crate_cache
        .write()
        .insert(name.to_string(), Arc::clone(&krate));
    select_version_with_reqs(krate.as_ref(), reqs)
}

fn resolve_version_and_checksum(
    options: &RegistryOptions,
    index: &SparseIndex,
    agent: &ureq::Agent,
    crate_cache: &Arc<RwLock<HashMap<String, Arc<crates_index::Crate>>>>,
    name: &str,
    req: &VersionReq,
) -> Result<(String, Option<String>)> {
    if !options.refresh_index {
        if let Some(krate) = crate_cache.read().get(name).cloned() {
            return select_version(krate.as_ref(), req);
        }
    }

    let krate = if options.offline {
        index
            .crate_from_cache(name)
            .map_err(|err| eyre::eyre!("crate '{name}' not available offline: {err}"))?
    } else {
        fetch_sparse_crate(agent, index, name)?
    };
    let krate = Arc::new(krate);
    crate_cache
        .write()
        .insert(name.to_string(), Arc::clone(&krate));
    select_version(krate.as_ref(), req)
}

fn fetch_sparse_crate(
    agent: &ureq::Agent,
    index: &SparseIndex,
    name: &str,
) -> Result<crates_index::Crate> {
    let request = index.make_cache_request(name)?;
    let mut ureq_request =
        agent.request(request.method().as_str(), &request.uri().to_string());
    for (name, value) in request.headers() {
        ureq_request = ureq_request.set(name.as_str(), value.to_str()?);
    }
    let response = ureq_request.call()?;
    let status = http::StatusCode::from_u16(response.status())?;
    let mut builder = http::Response::builder()
        .status(status)
        .version(http::Version::HTTP_11);
    for header_name in response.headers_names() {
        if let Some(value) = response.header(&header_name) {
            builder = builder.header(header_name.as_str(), value);
        }
    }
    let mut body = Vec::new();
    response.into_reader().read_to_end(&mut body)?;
    let response = builder.body(body)?;
    index
        .parse_cache_response(name, response, true)?
        .or_else(|| index.crate_from_cache(name).ok())
        .ok_or_else(|| eyre::eyre!("crate '{name}' not found"))
}

fn download_and_unpack(
    agent: &ureq::Agent,
    name: &str,
    version: &str,
    checksum: Option<&str>,
    crate_root: &Path,
) -> Result<()> {
    let url = format!(
        "https://crates.io/api/v1/crates/{}/{}/download",
        name, version
    );
    let download_dir = crate_root
        .parent()
        .and_then(|p| p.parent())
        .ok_or_else(|| eyre::eyre!("invalid cache path for {}", crate_root.display()))?
        .join("downloads");
    fs::create_dir_all(&download_dir)?;

    let archive_path = download_dir.join(format!("{name}-{version}.crate"));
    let response = agent.get(&url).call().map_err(|err| {
        eyre::eyre!("failed to download crate {name} {version} from {url}: {err}")
    })?;
    let mut reader = response.into_reader();
    let mut out = File::create(&archive_path)?;
    let mut hasher = Sha256::new();
    let mut buffer = [0u8; 8192];
    loop {
        let bytes = reader.read(&mut buffer)?;
        if bytes == 0 {
            break;
        }
        hasher.update(&buffer[..bytes]);
        out.write_all(&buffer[..bytes])?;
    }

    if let Some(expected) = checksum {
        let digest = hex::encode(hasher.finalize());
        if digest != expected {
            return Err(eyre::eyre!(
                "checksum mismatch for {} {}: expected {}, got {}",
                name,
                version,
                expected,
                digest
            ));
        }
    }

    fs::create_dir_all(crate_root)?;
    let file = File::open(&archive_path)?;
    unpack_crate_archive(file, crate_root)?;

    Ok(())
}

async fn download_and_unpack_async(
    client: &Client,
    name: &str,
    version: &str,
    checksum: Option<&str>,
    crate_root: &Path,
) -> Result<()> {
    let url = format!(
        "https://crates.io/api/v1/crates/{}/{}/download",
        name, version
    );
    let download_dir = crate_root
        .parent()
        .and_then(|p| p.parent())
        .ok_or_else(|| eyre::eyre!("invalid cache path for {}", crate_root.display()))?
        .join("downloads");
    std::fs::create_dir_all(&download_dir)?;

    let archive_path = download_dir.join(format!("{name}-{version}.crate"));
    let response = client
        .get(&url)
        .send()
        .await
        .map_err(|err| eyre::eyre!("failed to download crate {name} {version}: {err}"))?;
    if !response.status().is_success() {
        return Err(eyre::eyre!(
            "failed to download crate {name} {version}: http {}",
            response.status()
        ));
    }

    let mut file = tokio::fs::File::create(&archive_path).await?;
    let mut hasher = Sha256::new();
    let mut stream = response.bytes_stream();
    while let Some(chunk) = stream.next().await {
        let chunk = chunk?;
        hasher.update(&chunk);
        file.write_all(&chunk).await?;
    }
    file.flush().await?;

    if let Some(expected) = checksum {
        let digest = hex::encode(hasher.finalize());
        if digest != expected {
            return Err(eyre::eyre!(
                "checksum mismatch for {} {}: expected {}, got {}",
                name,
                version,
                expected,
                digest
            ));
        }
    }

    let crate_root = crate_root.to_path_buf();
    tokio::task::spawn_blocking(move || -> Result<()> {
        std::fs::create_dir_all(&crate_root)?;
        let file = File::open(&archive_path)?;
        unpack_crate_archive(file, &crate_root)?;
        Ok(())
    })
    .await
    .map_err(|err| eyre::eyre!("failed to unpack crate {name} {version}: {err}"))??;

    Ok(())
}

fn unpack_crate_archive<R: Read>(reader: R, dest: &Path) -> Result<()> {
    let gz = GzDecoder::new(reader);
    let mut archive = tar::Archive::new(gz);
    for entry in archive.entries()? {
        let mut entry = entry?;
        let path = entry.path()?.to_path_buf();
        let relative = strip_archive_prefix(&path)?;
        if relative.as_os_str().is_empty() {
            continue;
        }
        let dest_path = dest.join(relative);
        if entry.header().entry_type().is_dir() {
            fs::create_dir_all(&dest_path)?;
        } else {
            if let Some(parent) = dest_path.parent() {
                fs::create_dir_all(parent)?;
            }
            entry.unpack(&dest_path)?;
        }
    }
    Ok(())
}

fn strip_archive_prefix(path: &Path) -> Result<PathBuf> {
    let mut components = path.components();
    let _ = components.next();
    let mut clean = PathBuf::new();
    for component in components {
        match component {
            Component::Normal(part) => clean.push(part),
            _ => {
                return Err(eyre::eyre!(
                    "invalid archive path component: {:?}",
                    component
                ));
            }
        }
    }
    Ok(clean)
}

struct CargoHomeGuard {
    previous: Option<OsString>,
    applied: bool,
}

impl CargoHomeGuard {
    fn apply(cache_dir: &Path) -> Result<Self> {
        fs::create_dir_all(cache_dir)?;
        let previous = std::env::var_os("CARGO_HOME");
        unsafe {
            std::env::set_var("CARGO_HOME", cache_dir);
        }
        Ok(Self {
            previous,
            applied: true,
        })
    }
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

fn resolve_cache_dir(cache_dir: Option<PathBuf>) -> Result<PathBuf> {
    if let Some(cache_dir) = cache_dir {
        return Ok(cache_dir);
    }
    if let Some(cache_dir) = std::env::var_os("MAGNET_CACHE_DIR") {
        return Ok(PathBuf::from(cache_dir));
    }
    let home = std::env::var_os("HOME")
        .or_else(|| std::env::var_os("USERPROFILE"))
        .ok_or_else(|| eyre::eyre!("could not resolve home directory"))?;
    Ok(PathBuf::from(home).join(".cache").join("magnet"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use flate2::write::GzEncoder;
    use flate2::Compression;
    use tempfile::tempdir;

    #[test]
    fn unpack_crate_strips_top_level_dir() -> Result<()> {
        let temp = tempdir()?;
        let archive_path = temp.path().join("demo.crate");
        let crate_root = temp.path().join("out");
        let mut tarball = GzEncoder::new(File::create(&archive_path)?, Compression::default());
        let mut builder = tar::Builder::new(&mut tarball);
        let payload = b"pub fn demo() {}";
        let mut header = tar::Header::new_gnu();
        header.set_mode(0o644);
        header.set_mtime(0);
        header.set_size(payload.len() as u64);
        header.set_cksum();
        builder.append_data(&mut header, "demo-0.1.0/src/lib.rs", &payload[..])?;
        builder.finish()?;
        drop(builder);
        tarball.finish()?;

        let file = File::open(&archive_path)?;
        unpack_crate_archive(file, &crate_root)?;

        assert!(crate_root.join("src").join("lib.rs").exists());
        Ok(())
    }
}
