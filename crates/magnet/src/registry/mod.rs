#[cfg(feature = "net")]
mod net;

#[cfg(feature = "net")]
pub use net::*;

#[cfg(not(feature = "net"))]
mod net_disabled {
    use eyre::Result;
    use std::path::PathBuf;

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

    pub struct RegistryClient;

    impl RegistryClient {
        pub fn new(_options: RegistryOptions) -> Result<Self> {
            Err(eyre::eyre!(
                "Feature 'net' is disabled; registry client is unavailable."
            ))
        }

        pub fn resolve(&self, _name: &str, _version_req: Option<&str>) -> Result<ResolvedCrate> {
            Err(eyre::eyre!(
                "Feature 'net' is disabled; registry resolution is unavailable."
            ))
        }

        pub async fn resolve_async(
            &self,
            _name: &str,
            _version_req: Option<&str>,
        ) -> Result<ResolvedCrate> {
            Err(eyre::eyre!(
                "Feature 'net' is disabled; registry resolution is unavailable."
            ))
        }
    }
}

#[cfg(not(feature = "net"))]
pub use net_disabled::*;
