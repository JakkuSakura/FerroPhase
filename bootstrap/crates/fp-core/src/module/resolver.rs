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

#[derive(Debug)]
pub enum ResolverError {
    ModuleNotFound(String),
    SymbolNotFound(String),
    InvalidImport(String),
    Other(String),
}

impl std::fmt::Display for ResolverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolverError::ModuleNotFound(name) => write!(f, "module not found: {}", name),
            ResolverError::SymbolNotFound(name) => write!(f, "symbol not found: {}", name),
            ResolverError::InvalidImport(name) => write!(f, "invalid import: {}", name),
            ResolverError::Other(message) => write!(f, "{}", message),
        }
    }
}

impl std::error::Error for ResolverError {}

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

impl std::fmt::Debug for ResolverRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let languages: Vec<&ModuleLanguage> = self.resolvers.keys().collect();
        f.debug_struct("ResolverRegistry")
            .field("languages", &languages)
            .finish()
    }
}

impl ResolverRegistry {
    pub fn new() -> Self {
        Self {
            resolvers: HashMap::new(),
        }
    }

    pub fn register(&mut self, language: ModuleLanguage, resolver: Arc<dyn LanguageResolver>) {
        self.resolvers.insert(language, resolver);
    }

    pub fn resolver_for(&self, language: &ModuleLanguage) -> Option<&Arc<dyn LanguageResolver>> {
        self.resolvers.get(language)
    }
}
