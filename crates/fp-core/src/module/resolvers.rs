use crate::module::resolver::{
    ExportMap, LanguageResolver, ModuleImport, ResolvedSymbol, ResolverError,
};
use crate::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use crate::package::graph::PackageGraph;

#[derive(Debug, Default, Clone)]
pub struct FerroResolver;

impl FerroResolver {
    fn parse_import(spec: &str) -> Result<FerroImportPath, ResolverError> {
        let trimmed = spec.trim();
        if trimmed.is_empty() {
            return Err(ResolverError::InvalidImport(
                "empty import spec".to_string(),
            ));
        }

        let mut has_root = false;
        let mut raw = trimmed;
        if raw.starts_with("::") {
            has_root = true;
            raw = raw.trim_start_matches("::");
        }

        let mut segments: Vec<&str> = raw.split("::").filter(|seg| !seg.is_empty()).collect();
        if segments.is_empty() {
            return Err(ResolverError::InvalidImport(format!(
                "invalid import spec '{}'",
                spec
            )));
        }

        let mut prefix = if has_root {
            FerroPrefix::Root
        } else {
            FerroPrefix::Plain
        };

        if !has_root {
            match segments[0] {
                "crate" => {
                    prefix = FerroPrefix::Crate;
                    segments.remove(0);
                }
                "self" => {
                    prefix = FerroPrefix::SelfMod;
                    segments.remove(0);
                }
                "super" => {
                    let mut depth = 0;
                    while segments.first() == Some(&"super") {
                        segments.remove(0);
                        depth += 1;
                    }
                    prefix = FerroPrefix::Super(depth);
                }
                _ => {}
            }
        }

        let segments = segments
            .into_iter()
            .map(|seg| seg.trim().to_string())
            .filter(|seg| !seg.is_empty())
            .collect::<Vec<_>>();

        Ok(FerroImportPath { prefix, segments })
    }

    fn resolve_module_path(
        import: &ModuleImport,
        from: Option<&ModuleDescriptor>,
        graph: &PackageGraph,
    ) -> Result<(ModuleId, ModuleDescriptor), ResolverError> {
        let path = Self::parse_import(&import.spec)?;
        let from = from.ok_or_else(|| {
            ResolverError::InvalidImport("relative imports require a source module".to_string())
        })?;

        let mut base_segments = Vec::new();
        let mut package_id = from.package.clone();

        match path.prefix {
            FerroPrefix::Root | FerroPrefix::Crate => {}
            FerroPrefix::SelfMod => {
                base_segments.extend(from.module_path.iter().cloned());
            }
            FerroPrefix::Super(depth) => {
                if depth > from.module_path.len() {
                    return Err(ResolverError::InvalidImport(format!(
                        "super:: import escapes module root: {}",
                        import.spec
                    )));
                }
                base_segments.extend(
                    from.module_path
                        .iter()
                        .take(from.module_path.len() - depth)
                        .cloned(),
                );
            }
            FerroPrefix::Plain => {
                if let Some((pkg_id, segments)) =
                    Self::dependency_package_override(from, graph, &path.segments)
                {
                    package_id = pkg_id;
                    base_segments = segments;
                } else {
                    base_segments.extend(from.module_path.iter().cloned());
                }
            }
        }

        let mut full_path = base_segments;
        full_path.extend(path.segments.iter().cloned());

        let module_id = Self::find_module_id(graph, &package_id, &full_path)
            .ok_or_else(|| ResolverError::ModuleNotFound(import.spec.clone()))?;
        let module = graph
            .module(&module_id)
            .cloned()
            .ok_or_else(|| ResolverError::ModuleNotFound(import.spec.clone()))?;
        if !matches!(
            module.language,
            ModuleLanguage::Ferro | ModuleLanguage::Rust
        ) {
            return Err(ResolverError::Other(format!(
                "module {} is not a Ferro/Rust module",
                module.id
            )));
        }
        Ok((module_id, module))
    }

    fn dependency_package_override(
        from: &ModuleDescriptor,
        graph: &PackageGraph,
        segments: &[String],
    ) -> Option<(crate::package::PackageId, Vec<String>)> {
        let Some(first) = segments.first() else {
            return None;
        };
        let package = graph.package(&from.package)?;
        let has_dependency = package
            .metadata
            .dependencies
            .iter()
            .any(|dep| dep.package == *first);
        if !has_dependency {
            return None;
        }
        let package_id = graph.package_by_name(first)?.clone();
        let remaining = segments.iter().skip(1).cloned().collect();
        Some((package_id, remaining))
    }

    fn find_module_id(
        graph: &PackageGraph,
        package_id: &crate::package::PackageId,
        module_path: &[String],
    ) -> Option<ModuleId> {
        let modules = graph.modules_for_package(package_id)?;
        modules
            .iter()
            .find(|module_id| {
                graph
                    .module(module_id)
                    .map(|module| module.module_path == module_path)
                    .unwrap_or(false)
            })
            .cloned()
    }
}

impl LanguageResolver for FerroResolver {
    fn resolve_module(
        &self,
        import: &ModuleImport,
        from: Option<&ModuleDescriptor>,
        graph: &PackageGraph,
    ) -> Result<ModuleId, ResolverError> {
        Self::resolve_module_path(import, from, graph).map(|(id, _)| id)
    }

    fn resolve_symbol(
        &self,
        module: &ModuleDescriptor,
        symbol: &str,
        _graph: &PackageGraph,
    ) -> Result<ResolvedSymbol, ResolverError> {
        let found = module.exports.iter().find(|item| item.name == symbol);
        match found {
            Some(desc) => Ok(ResolvedSymbol::Symbol(desc.clone())),
            None => Err(ResolverError::SymbolNotFound(symbol.to_string())),
        }
    }

    fn list_exports(
        &self,
        module: &ModuleDescriptor,
        _graph: &PackageGraph,
    ) -> Result<ExportMap, ResolverError> {
        Ok(module
            .exports
            .iter()
            .map(|item| (item.name.clone(), item.clone()))
            .collect())
    }
}

#[derive(Debug, Clone)]
pub struct TranspileResolver {
    language: ModuleLanguage,
}

impl TranspileResolver {
    pub fn new(language: ModuleLanguage) -> Self {
        Self { language }
    }

    fn parse_spec(&self, spec: &str) -> Result<Vec<String>, ResolverError> {
        let trimmed = spec.trim();
        if trimmed.is_empty() {
            return Err(ResolverError::InvalidImport(
                "empty import spec".to_string(),
            ));
        }
        let segments = if trimmed.contains("::") {
            trimmed.split("::").collect::<Vec<_>>()
        } else if trimmed.contains('/') {
            trimmed.split('/').collect::<Vec<_>>()
        } else if trimmed.contains('.') {
            trimmed.split('.').collect::<Vec<_>>()
        } else {
            vec![trimmed]
        };
        Ok(segments
            .into_iter()
            .map(|seg| seg.trim().to_string())
            .filter(|seg| !seg.is_empty())
            .collect())
    }

    fn resolve_module_descriptor(
        &self,
        import: &ModuleImport,
        from: Option<&ModuleDescriptor>,
        graph: &PackageGraph,
    ) -> Result<ModuleDescriptor, ResolverError> {
        let segments = self.parse_spec(&import.spec)?;
        let package_scope = from.map(|module| module.package.clone());

        let mut candidates = Vec::new();
        for module in graph.modules() {
            if module.language != self.language {
                continue;
            }
            if module.module_path == segments {
                if let Some(package_id) = &package_scope {
                    if &module.package == package_id {
                        return Ok(module.clone());
                    }
                }
                candidates.push(module.clone());
            }
        }

        if candidates.len() == 1 {
            return Ok(candidates.remove(0));
        }
        if candidates.is_empty() {
            return Err(ResolverError::ModuleNotFound(import.spec.clone()));
        }
        Err(ResolverError::Other(format!(
            "ambiguous module import '{}'",
            import.spec
        )))
    }
}

impl LanguageResolver for TranspileResolver {
    fn resolve_module(
        &self,
        import: &ModuleImport,
        from: Option<&ModuleDescriptor>,
        graph: &PackageGraph,
    ) -> Result<ModuleId, ResolverError> {
        self.resolve_module_descriptor(import, from, graph)
            .map(|module| module.id)
    }

    fn resolve_symbol(
        &self,
        module: &ModuleDescriptor,
        symbol: &str,
        _graph: &PackageGraph,
    ) -> Result<ResolvedSymbol, ResolverError> {
        let found = module.exports.iter().find(|item| item.name == symbol);
        match found {
            Some(desc) => Ok(ResolvedSymbol::Symbol(desc.clone())),
            None => Err(ResolverError::SymbolNotFound(symbol.to_string())),
        }
    }

    fn list_exports(
        &self,
        module: &ModuleDescriptor,
        _graph: &PackageGraph,
    ) -> Result<ExportMap, ResolverError> {
        Ok(module
            .exports
            .iter()
            .map(|item| (item.name.clone(), item.clone()))
            .collect())
    }
}

#[derive(Debug, Clone)]
struct FerroImportPath {
    prefix: FerroPrefix,
    segments: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FerroPrefix {
    Root,
    Crate,
    SelfMod,
    Super(usize),
    Plain,
}
