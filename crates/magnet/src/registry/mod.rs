use crates_index::Index;
use eyre::Result;
use flate2::read::GzDecoder;
use semver::{Version, VersionReq};
use sha2::{Digest, Sha256};
use std::ffi::OsString;
use std::fs::{self, File};
use std::io::{Read, Write};
use std::path::{Component, Path, PathBuf};

#[derive(Debug, Clone)]
pub struct RegistryOptions {
    pub offline: bool,
    pub cache_dir: Option<PathBuf>,
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
}

impl RegistryClient {
    pub fn new(options: RegistryOptions) -> Result<Self> {
        let cache_dir = resolve_cache_dir(options.cache_dir.clone())?;
        fs::create_dir_all(&cache_dir)?;
        Ok(Self { options, cache_dir })
    }

    pub fn resolve(&self, name: &str, version_req: Option<&str>) -> Result<ResolvedCrate> {
        let req_str = version_req.ok_or_else(|| eyre::eyre!("missing version for {name}"))?;
        let req = VersionReq::parse(req_str)
            .map_err(|err| eyre::eyre!("invalid version requirement '{req_str}': {err}"))?;

        let _guard = CargoHomeGuard::apply(&self.cache_dir)?;
        let mut index = Index::new_cargo_default()?;
        if self.options.offline {
            if !index.path().exists() {
                return Err(eyre::eyre!(
                    "registry index not available offline (missing {})",
                    index.path().display()
                ));
            }
        } else {
            index.update()?;
        }

        let krate = index
            .crate_(name)
            .ok_or_else(|| eyre::eyre!("crate '{name}' not found"))?;
        let (version, checksum) = select_version(&krate, &req)?;

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

        download_and_unpack(name, &version, checksum.as_deref(), &crate_root)?;

        Ok(ResolvedCrate {
            name: name.to_string(),
            version,
            checksum,
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

fn download_and_unpack(
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
    let response = ureq::get(&url).call().map_err(|err| {
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
