//! Path-value machinery for a syntactic AST path (`ast::Path`): parsing a
//! textual path spec (`ParsedPath`), and the fully-resolved absolute form
//! used as a lookup key (`QualifiedPath`). Actual resolution against a real
//! module tree (`parse_path`/`resolve_item_path`, as they used to be named
//! here) lives on `fp-backend`'s `AstToHirLowerer` instead — its only real
//! caller, which needs its own state (module path, module tree, symbol
//! tables, workspace) throughout, not a free function reached into via
//! several closures. Lives under `ast` rather than a shared crate-root
//! module — like every other IR here (`hir::Path`/`hir::DefPath`,
//! `mir::ident::Path`), path values are owned by the stage that defines
//! them, not centralized across stages.

use std::collections::HashSet;

use crate::package::PackageId;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedPath {
    pub prefix: PathPrefix,
    pub segments: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct QualifiedPath {
    pub package_id: PackageId,
    pub segments: Vec<String>,
}

impl QualifiedPath {
    pub fn new(package_id: PackageId, segments: Vec<String>) -> Self {
        Self {
            package_id,
            segments,
        }
    }

    pub fn with_package_id(package_id: PackageId, segments: Vec<String>) -> Self {
        Self::new(package_id, segments)
    }

    pub fn from_slice(segments: &[String]) -> Self {
        Self {
            package_id: PackageId::default(),
            segments: segments.to_vec(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn head(&self) -> Option<&str> {
        self.segments.first().map(|seg| seg.as_str())
    }

    pub fn tail(&self) -> Option<&str> {
        self.segments.last().map(|seg| seg.as_str())
    }

    pub fn push(&mut self, segment: String) {
        self.segments.push(segment);
    }

    pub fn pop(&mut self) -> Option<String> {
        self.segments.pop()
    }

    pub fn with_segment(&self, segment: String) -> Self {
        let mut segments = self.segments.clone();
        segments.push(segment);
        Self {
            package_id: self.package_id.clone(),
            segments,
        }
    }

    pub fn join(&self, extra: &[String]) -> Self {
        let mut segments = self.segments.clone();
        segments.extend(extra.iter().cloned());
        Self {
            package_id: self.package_id.clone(),
            segments,
        }
    }

    pub fn parent_n(&self, depth: usize) -> Option<Self> {
        if depth > self.segments.len() {
            return None;
        }
        let keep = self.segments.len().saturating_sub(depth);
        Some(Self {
            package_id: self.package_id.clone(),
            segments: self.segments[..keep].to_vec(),
        })
    }

    pub fn to_key(&self) -> String {
        segments_to_key(&self.segments)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum PathPrefix {
    Root,
    Crate,
    SelfMod,
    Super(usize),
    Plain,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathError {
    EmptyPath,
    InvalidPath(String),
}

pub fn resolve_path(
    parsed: &ParsedPath,
    module_path: &QualifiedPath,
    root_modules: &HashSet<String>,
    extern_prelude: &HashSet<String>,
    module_defs: &HashSet<QualifiedPath>,
) -> Option<QualifiedPath> {
    if parsed.segments.is_empty() {
        return None;
    }

    match parsed.prefix {
        PathPrefix::Root | PathPrefix::Crate => Some(QualifiedPath::new(
            module_path.package_id.clone(),
            parsed.segments.clone(),
        )),
        PathPrefix::SelfMod => Some(module_path.join(&parsed.segments)),
        PathPrefix::Super(depth) => module_path
            .parent_n(depth)
            .map(|parent| parent.join(&parsed.segments)),
        PathPrefix::Plain => {
            let first = parsed.segments.first()?;
            let base = if module_path.head() == Some("bin") {
                QualifiedPath::new(module_path.package_id.clone(), Vec::new())
            } else {
                module_path.clone()
            };
            if !base.is_empty() {
                let local = base.with_segment(first.clone());
                if module_defs.contains(&local) {
                    return Some(base.join(&parsed.segments));
                }
            } else {
                let local = QualifiedPath::new(module_path.package_id.clone(), vec![first.clone()]);
                if module_defs.contains(&local) {
                    return Some(QualifiedPath::new(
                        module_path.package_id.clone(),
                        parsed.segments.clone(),
                    ));
                }
            }
            if root_modules.contains(first) || extern_prelude.contains(first) {
                return Some(QualifiedPath::new(
                    module_path.package_id.clone(),
                    parsed.segments.clone(),
                ));
            }
            None
        }
    }
}

pub fn segments_to_key(segments: &[String]) -> String {
    segments.join("::")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolve_plain_prefers_local_module() {
        let parsed = ParsedPath {
            prefix: PathPrefix::Plain,
            segments: vec!["meta".to_string(), "TypeBuilder".to_string()],
        };
        let mut module_defs = HashSet::new();
        module_defs.insert(QualifiedPath::new(vec![
            "std".to_string(),
            "meta".to_string(),
        ]));
        let resolved = resolve_path(
            &parsed,
            &QualifiedPath::new(vec!["std".to_string()]),
            &HashSet::new(),
            &HashSet::new(),
            &module_defs,
        )
        .unwrap();
        assert_eq!(
            resolved,
            QualifiedPath::new(vec![
                "std".to_string(),
                "meta".to_string(),
                "TypeBuilder".to_string()
            ])
        );
    }

    #[test]
    fn resolve_plain_from_bin_uses_crate_root() {
        let parsed = ParsedPath {
            prefix: PathPrefix::Plain,
            segments: vec!["fptest".to_string(), "config".to_string()],
        };
        let mut module_defs = HashSet::new();
        module_defs.insert(QualifiedPath::new(vec!["fptest".to_string()]));
        let resolved = resolve_path(
            &parsed,
            &QualifiedPath::new(vec!["bin".to_string(), "fptest".to_string()]),
            &HashSet::new(),
            &HashSet::new(),
            &module_defs,
        )
        .unwrap();
        assert_eq!(
            resolved,
            QualifiedPath::new(vec!["fptest".to_string(), "config".to_string()])
        );
    }
}
