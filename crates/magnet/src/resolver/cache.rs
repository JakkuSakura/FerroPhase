use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct ResolverCachePolicy {
    pub cache_dir: Option<PathBuf>,
}

impl ResolverCachePolicy {
    pub fn new(cache_dir: Option<PathBuf>) -> Self {
        Self { cache_dir }
    }
}
