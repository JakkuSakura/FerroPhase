use std::sync::Arc;

use crate::module::path::{ParsedPath, PathPrefix, QualifiedPath};
use crate::module::resolver::ResolverRegistry;
use crate::module::{ModuleDescriptor, ModuleId};
use crate::package::graph::PackageGraph;
use crate::package::PackageId;

#[derive(Debug, Clone)]
pub struct ModuleResolutionContext {
    pub graph: Arc<PackageGraph>,
    pub resolvers: Arc<ResolverRegistry>,
    pub current_module: Option<ModuleId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct QualifiedSymbol {
    pub package: PackageId,
    pub path: QualifiedPath,
}

impl QualifiedSymbol {
    pub fn new(package: PackageId, path: QualifiedPath) -> Self {
        Self { package, path }
    }

    pub fn to_key(&self) -> String {
        format!("{}::{}", self.package, self.path.to_key())
    }
}

pub fn resolve_symbol_path(
    parsed: &ParsedPath,
    ctx: &ModuleResolutionContext,
    scope_contains: impl Fn(&str) -> bool,
    item_exists: impl Fn(&QualifiedSymbol) -> bool,
) -> Option<QualifiedSymbol> {
    let module_id = ctx.current_module.as_ref()?;
    let module = ctx.graph.module(module_id)?;
    let (package_id, base_segments, raw_segments) =
        resolve_base_segments(parsed, module, &ctx.graph)?;

    if raw_segments.is_empty() {
        return None;
    }

    if matches!(parsed.prefix, PathPrefix::Plain) && raw_segments.len() == 1 {
        let first = raw_segments[0].as_str();
        if scope_contains(first) {
            return Some(QualifiedSymbol::new(
                module.package.clone(),
                QualifiedPath::new(vec![first.to_string()]),
            ));
        }
    }

    let mut segments = base_segments;
    segments.extend(raw_segments);
    let candidate = QualifiedPath::new(segments);
    let qualified = QualifiedSymbol::new(package_id.clone(), candidate.clone());
    if item_exists(&qualified) || module_exists(&ctx.graph, &package_id, &candidate) {
        return Some(qualified);
    }

    None
}

fn resolve_base_segments(
    parsed: &ParsedPath,
    module: &ModuleDescriptor,
    graph: &PackageGraph,
) -> Option<(PackageId, Vec<String>, Vec<String>)> {
    if parsed.segments.is_empty() {
        return None;
    }

    match parsed.prefix {
        PathPrefix::Root | PathPrefix::Crate => {
            Some((module.package.clone(), Vec::new(), parsed.segments.clone()))
        }
        PathPrefix::SelfMod => Some((
            module.package.clone(),
            module.module_path.clone(),
            parsed.segments.clone(),
        )),
        PathPrefix::Super(depth) => {
            if depth > module.module_path.len() {
                return None;
            }
            let base = module
                .module_path
                .iter()
                .take(module.module_path.len() - depth)
                .cloned()
                .collect::<Vec<_>>();
            Some((module.package.clone(), base, parsed.segments.clone()))
        }
        PathPrefix::Plain => {
            if let Some((package_id, remaining)) =
                dependency_package_override(module, graph, &parsed.segments)
            {
                if remaining.is_empty() {
                    return None;
                }
                return Some((package_id, Vec::new(), remaining));
            }
            Some((
                module.package.clone(),
                module.module_path.clone(),
                parsed.segments.clone(),
            ))
        }
    }
}

fn dependency_package_override(
    from: &ModuleDescriptor,
    graph: &PackageGraph,
    segments: &[String],
) -> Option<(PackageId, Vec<String>)> {
    let first = segments.first()?;
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

fn module_exists(graph: &PackageGraph, package_id: &PackageId, path: &QualifiedPath) -> bool {
    let Some(modules) = graph.modules_for_package(package_id) else {
        return false;
    };
    modules.iter().any(|module_id| {
        graph
            .module(module_id)
            .map(|module| module.module_path == path.segments)
            .unwrap_or(false)
    })
}
