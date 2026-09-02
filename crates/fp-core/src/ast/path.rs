//! Path-value machinery for a syntactic AST path (`ast::Path`): parsing a
//! textual AST paths (`ast::Path`), and the fully-resolved absolute form
//! used as a lookup key (`InPackagePath`). Actual resolution against a real
//! module tree (`parse_path`/`resolve_item_path`, as they used to be named
//! here) lives on `fp-backend`'s `AstToHirLowerer` instead — its only real
//! caller, which needs its own state (module path, module tree, symbol
//! tables, workspace) throughout, not a free function reached into via
//! several closures. Lives under `ast` rather than a shared crate-root
//! module — like every other IR here (`hir::Path`,
//! `mir::ident::Path`), path values are owned by the stage that defines
//! them, not centralized across stages.

use std::collections::HashSet;

use serde::{Deserialize, Serialize};

use crate::ast::ident::Path;

impl Path {
    pub fn head(&self) -> Option<&str> {
        self.segments.first().map(|segment| segment.as_str())
    }

    pub fn resolve_from(&self, location: &InPackagePath) -> Option<InPackagePath> {
        if self.is_empty() {
            return None;
        }
        match self.prefix {
            PathPrefix::Root | PathPrefix::Crate => Some(InPackagePath::new(
                self.segments
                    .iter()
                    .map(|s| s.as_str().to_owned())
                    .collect(),
            )),
            PathPrefix::SelfMod => Some(
                location.join(
                    &self
                        .segments
                        .iter()
                        .map(|s| s.as_str().to_owned())
                        .collect::<Vec<_>>(),
                ),
            ),
            PathPrefix::Super(depth) => location.parent_n(depth).map(|parent| {
                parent.join(
                    &self
                        .segments
                        .iter()
                        .map(|s| s.as_str().to_owned())
                        .collect::<Vec<_>>(),
                )
            }),
            PathPrefix::Plain => Some(
                location.join(
                    &self
                        .segments
                        .iter()
                        .map(|s| s.as_str().to_owned())
                        .collect::<Vec<_>>(),
                ),
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct InPackagePath {
    pub segments: Vec<String>,
}

impl InPackagePath {
    pub fn new(segments: Vec<String>) -> Self {
        Self { segments }
    }

    pub fn from_slice(segments: &[String]) -> Self {
        Self {
            segments: segments.to_vec(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn len(&self) -> usize {
        self.segments.len()
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
        Self { segments }
    }

    pub fn join(&self, extra: &[String]) -> Self {
        let mut segments = self.segments.clone();
        segments.extend(extra.iter().cloned());
        Self { segments }
    }

    pub fn parent_n(&self, depth: usize) -> Option<Self> {
        if depth > self.segments.len() {
            return None;
        }
        let keep = self.segments.len().saturating_sub(depth);
        Some(Self {
            segments: self.segments[..keep].to_vec(),
        })
    }

    pub fn to_key(&self) -> String {
        segments_to_key(&self.segments)
    }

    pub fn to_ast_path(&self) -> crate::ast::Path {
        crate::ast::Path::plain(
            self.segments
                .iter()
                .map(|segment| crate::ast::Ident::new(segment.clone()))
                .collect(),
        )
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
    parsed: &Path,
    module_path: &InPackagePath,
    root_modules: &HashSet<String>,
    extern_prelude: &HashSet<String>,
    module_defs: &HashSet<InPackagePath>,
) -> Option<InPackagePath> {
    if parsed.is_empty() {
        return None;
    }

    match parsed.prefix {
        PathPrefix::Root | PathPrefix::Crate => parsed.resolve_from(module_path),
        PathPrefix::SelfMod => parsed.resolve_from(module_path),
        PathPrefix::Super(_) => parsed.resolve_from(module_path),
        PathPrefix::Plain => {
            let first = parsed.head()?;
            let base = if module_path.head() == Some("bin") {
                InPackagePath::new(Vec::new())
            } else {
                module_path.clone()
            };
            if !base.is_empty() {
                let local = base.with_segment(first.to_owned());
                if module_defs.contains(&local) {
                    return parsed.resolve_from(module_path);
                }
            } else {
                let local = InPackagePath::new(vec![first.to_owned()]);
                if module_defs.contains(&local) {
                    return parsed.resolve_from(module_path);
                }
            }
            if root_modules.contains(first) || extern_prelude.contains(first) {
                return parsed.resolve_from(module_path);
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
        let parsed = Path::new(PathPrefix::Plain, vec!["meta".into(), "TypeBuilder".into()]);
        let mut module_defs = HashSet::new();
        module_defs.insert(InPackagePath::new(vec![
            "std".to_string(),
            "meta".to_string(),
        ]));
        let resolved = resolve_path(
            &parsed,
            &InPackagePath::new(vec!["std".to_string()]),
            &HashSet::new(),
            &HashSet::new(),
            &module_defs,
        )
        .unwrap();
        assert_eq!(
            resolved,
            InPackagePath::new(vec![
                "std".to_string(),
                "meta".to_string(),
                "TypeBuilder".to_string()
            ])
        );
    }

    #[test]
    fn resolve_plain_from_bin_uses_crate_root() {
        let parsed = Path::new(PathPrefix::Plain, vec!["fptest".into(), "config".into()]);
        let mut module_defs = HashSet::new();
        module_defs.insert(InPackagePath::new(vec!["fptest".to_string()]));
        let resolved = resolve_path(
            &parsed,
            &InPackagePath::new(vec!["bin".to_string(), "fptest".to_string()]),
            &HashSet::new(),
            &HashSet::new(),
            &module_defs,
        )
        .unwrap();
        assert_eq!(
            resolved,
            InPackagePath::new(vec!["fptest".to_string(), "config".to_string()])
        );
    }
}
