use std::collections::HashMap;
use std::sync::Arc;

use crate::module::{ModuleDescriptor, ModuleId, ModuleLanguage, SymbolDescriptor};
use crate::package::graph::PackageGraph;

#[derive(Clone, Debug)]
pub struct ModuleImport {
    pub spec: String,
}

impl ModuleImport {
    pub fn new(spec: impl Into<String>) -> Self {
        Self { spec: spec.into() }
    }
}

#[derive(Clone, Debug)]
pub enum ResolvedSymbol {
    Module(ModuleId),
    Symbol(SymbolDescriptor),
}

pub type ExportMap = HashMap<String, SymbolDescriptor>;

#[derive(Debug, thiserror::Error)]
pub enum ResolverError {
    #[error("module not found: {0}")]
    ModuleNotFound(String),
    #[error("symbol not found: {0}")]
    SymbolNotFound(String),
    #[error("invalid import: {0}")]
    InvalidImport(String),
    #[error("{0}")]
    Other(String),
}

pub trait LanguageResolver: Send + Sync {
    fn resolve_module(
        &self,
        import: &ModuleImport,
        from: Option<&ModuleDescriptor>,
        graph: &PackageGraph,
    ) -> Result<ModuleId, ResolverError>;

    fn resolve_symbol(
        &self,
        module: &ModuleDescriptor,
        symbol: &str,
        graph: &PackageGraph,
    ) -> Result<ResolvedSymbol, ResolverError>;

    fn list_exports(
        &self,
        module: &ModuleDescriptor,
        graph: &PackageGraph,
    ) -> Result<ExportMap, ResolverError>;
}

#[derive(Default)]
pub struct ResolverRegistry {
    resolvers: HashMap<ModuleLanguage, Arc<dyn LanguageResolver>>,
}

impl ResolverRegistry {
    pub fn new() -> Self {
        Self {
            resolvers: HashMap::new(),
        }
    }

    pub fn register(
        &mut self,
        language: ModuleLanguage,
        resolver: Arc<dyn LanguageResolver>,
    ) {
        self.resolvers.insert(language, resolver);
    }

    pub fn resolver_for(&self, language: &ModuleLanguage) -> Option<&Arc<dyn LanguageResolver>> {
        self.resolvers.get(language)
    }
}
