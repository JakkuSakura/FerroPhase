use std::collections::HashSet;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedPath {
    pub prefix: PathPrefix,
    pub segments: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct QualifiedPath {
    pub segments: Vec<String>,
}

impl QualifiedPath {
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

pub fn parse_path(spec: &str) -> Result<ParsedPath, PathError> {
    let trimmed = spec.trim();
    if trimmed.is_empty() {
        return Err(PathError::EmptyPath);
    }

    let mut raw = trimmed;
    let mut prefix = PathPrefix::Plain;
    if raw.starts_with("::") {
        prefix = PathPrefix::Root;
        raw = raw.trim_start_matches("::");
    }

    let mut segments: Vec<String> = raw
        .split("::")
        .filter(|seg| !seg.is_empty())
        .map(|seg| seg.trim().to_string())
        .filter(|seg| !seg.is_empty())
        .collect();

    if segments.is_empty() {
        return Err(PathError::InvalidPath(spec.to_string()));
    }

    if matches!(prefix, PathPrefix::Plain) {
        match segments[0].as_str() {
            "crate" => {
                prefix = PathPrefix::Crate;
                segments.remove(0);
            }
            "self" => {
                prefix = PathPrefix::SelfMod;
                segments.remove(0);
            }
            "super" => {
                let mut depth = 0;
                while segments.first().map(|seg| seg.as_str()) == Some("super") {
                    segments.remove(0);
                    depth += 1;
                }
                prefix = PathPrefix::Super(depth);
            }
            _ => {}
        }
    }

    Ok(ParsedPath { prefix, segments })
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
        PathPrefix::Root | PathPrefix::Crate => Some(QualifiedPath::new(parsed.segments.clone())),
        PathPrefix::SelfMod => {
            Some(module_path.join(&parsed.segments))
        }
        PathPrefix::Super(depth) => {
            module_path
                .parent_n(depth)
                .map(|parent| parent.join(&parsed.segments))
        }
        PathPrefix::Plain => {
            let first = parsed.segments.first()?;
            if !module_path.is_empty() {
                let local = module_path.with_segment(first.clone());
                if module_defs.contains(&local) {
                    return Some(module_path.join(&parsed.segments));
                }
            }
            if root_modules.contains(first) || extern_prelude.contains(first) {
                return Some(QualifiedPath::new(parsed.segments.clone()));
            }
            None
        }
    }
}

pub fn resolve_item_path<F>(
    parsed: &ParsedPath,
    module_path: &QualifiedPath,
    root_modules: &HashSet<String>,
    extern_prelude: &HashSet<String>,
    module_defs: &HashSet<QualifiedPath>,
    item_exists: F,
    scope_contains: impl Fn(&str) -> bool,
) -> Option<QualifiedPath>
where
    F: Fn(&QualifiedPath) -> bool,
{
    if parsed.segments.is_empty() {
        return None;
    }

    match parsed.prefix {
        PathPrefix::Root | PathPrefix::Crate => Some(QualifiedPath::new(parsed.segments.clone())),
        PathPrefix::SelfMod => {
            Some(module_path.join(&parsed.segments))
        }
        PathPrefix::Super(depth) => {
            module_path
                .parent_n(depth)
                .map(|parent| parent.join(&parsed.segments))
        }
        PathPrefix::Plain => {
            let first = parsed.segments.first()?;
            if parsed.segments.len() == 1 {
                if scope_contains(first) {
                    return Some(QualifiedPath::new(vec![first.clone()]));
                }
                if !module_path.is_empty() {
                    let local = module_path.with_segment(first.clone());
                    if item_exists(&local) || module_defs.contains(&local) {
                        return Some(local);
                    }
                } else {
                    let local = QualifiedPath::new(parsed.segments.clone());
                    if item_exists(&local) {
                        return Some(local);
                    }
                }
                if root_modules.contains(first) || extern_prelude.contains(first) {
                    return Some(QualifiedPath::new(parsed.segments.clone()));
                }
                return None;
            }

            if !module_path.is_empty() {
                let local = module_path.join(&parsed.segments);
                if item_exists(&local) {
                    return Some(local);
                }
                let module_candidate = module_path.with_segment(first.clone());
                if module_defs.contains(&module_candidate) {
                    return Some(local);
                }
            } else {
                let local = QualifiedPath::new(parsed.segments.clone());
                if item_exists(&local) {
                    return Some(local);
                }
            }

            if root_modules.contains(first) || extern_prelude.contains(first) {
                return Some(QualifiedPath::new(parsed.segments.clone()));
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
    fn resolve_item_prefers_local_scope() {
        let parsed = ParsedPath {
            prefix: PathPrefix::Plain,
            segments: vec!["TypeBuilder".to_string()],
        };
        let module_path = QualifiedPath::new(vec!["std".to_string(), "meta".to_string()]);
        let expected = QualifiedPath::new(vec![
            "std".to_string(),
            "meta".to_string(),
            "TypeBuilder".to_string(),
        ]);
        let resolved = resolve_item_path(
            &parsed,
            &module_path,
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            |segments| segments == &expected,
            |_| false,
        )
        .unwrap();
        assert_eq!(resolved, expected);
    }

    #[test]
    fn resolve_item_prefers_scope_over_module() {
        let parsed = ParsedPath {
            prefix: PathPrefix::Plain,
            segments: vec!["TypeBuilder".to_string()],
        };
        let module_path = QualifiedPath::new(vec!["std".to_string(), "meta".to_string()]);
        let resolved = resolve_item_path(
            &parsed,
            &module_path,
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            |_segments| true,
            |name| name == "TypeBuilder",
        )
        .unwrap();
        assert_eq!(resolved, QualifiedPath::new(vec!["TypeBuilder".to_string()]));
    }

    #[test]
    fn resolve_item_plain_module_blocks_extern() {
        let parsed = ParsedPath {
            prefix: PathPrefix::Plain,
            segments: vec!["net".to_string(), "tcp".to_string()],
        };
        let mut module_defs = HashSet::new();
        module_defs.insert(QualifiedPath::new(vec![
            "std".to_string(),
            "net".to_string(),
        ]));
        let resolved = resolve_item_path(
            &parsed,
            &QualifiedPath::new(vec!["std".to_string()]),
            &HashSet::new(),
            &["net".to_string()].into_iter().collect(),
            &module_defs,
            |_segments| false,
            |_| false,
        )
        .unwrap();
        assert_eq!(
            resolved,
            QualifiedPath::new(vec![
                "std".to_string(),
                "net".to_string(),
                "tcp".to_string()
            ])
        );
    }
}
