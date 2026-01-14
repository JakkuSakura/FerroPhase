use crate::models::DependencyModel;
use crate::registry::ResolvedCrate;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct RegistryDeps {
    pub dependencies: HashMap<String, DependencyModel>,
    pub dev_dependencies: HashMap<String, DependencyModel>,
    pub build_dependencies: HashMap<String, DependencyModel>,
}

#[derive(Debug, Clone)]
pub struct ResolvedRegistry {
    pub resolved: ResolvedCrate,
    pub deps: RegistryDeps,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct RegistryReqKey {
    pub name: String,
    pub version_req: String,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct RegistryManifestKey {
    pub name: String,
    pub version: String,
}
