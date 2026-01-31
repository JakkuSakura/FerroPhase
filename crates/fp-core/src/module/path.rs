use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedPath {
    pub prefix: PathPrefix,
    pub segments: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

pub fn parse_segments(segments: &[String]) -> Result<ParsedPath, PathError> {
    if segments.is_empty() {
        return Err(PathError::EmptyPath);
    }

    let mut idx = 0;
    let first = segments[0].as_str();
    let mut prefix = PathPrefix::Plain;

    if first == "__root__" {
        prefix = PathPrefix::Root;
        idx = 1;
    } else if first == "crate" {
        prefix = PathPrefix::Crate;
        idx = 1;
    } else if first == "self" {
        prefix = PathPrefix::SelfMod;
        idx = 1;
    } else if first == "super" {
        let mut depth = 0;
        while idx < segments.len() && segments[idx] == "super" {
            depth += 1;
            idx += 1;
        }
        prefix = PathPrefix::Super(depth);
    }

    let rest = segments[idx..].to_vec();
    Ok(ParsedPath {
        prefix,
        segments: rest,
    })
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
    module_path: &[String],
    root_modules: &HashSet<String>,
    extern_prelude: &HashSet<String>,
    module_defs: &HashSet<Vec<String>>,
) -> Option<Vec<String>> {
    if parsed.segments.is_empty() {
        return None;
    }

    match parsed.prefix {
        PathPrefix::Root | PathPrefix::Crate => Some(parsed.segments.clone()),
        PathPrefix::SelfMod => {
            let mut full = module_path.to_vec();
            full.extend(parsed.segments.iter().cloned());
            Some(full)
        }
        PathPrefix::Super(depth) => {
            if depth > module_path.len() {
                return None;
            }
            let mut full = module_path[..module_path.len() - depth].to_vec();
            full.extend(parsed.segments.iter().cloned());
            Some(full)
        }
        PathPrefix::Plain => {
            let first = parsed.segments.first()?;
            if !module_path.is_empty() {
                let mut local = module_path.to_vec();
                local.push(first.clone());
                if module_defs.contains(&local) {
                    let mut full = module_path.to_vec();
                    full.extend(parsed.segments.iter().cloned());
                    return Some(full);
                }
            }
            if root_modules.contains(first) || extern_prelude.contains(first) {
                return Some(parsed.segments.clone());
            }
            None
        }
    }
}

pub fn resolve_item_path<F>(
    parsed: &ParsedPath,
    module_path: &[String],
    root_modules: &HashSet<String>,
    extern_prelude: &HashSet<String>,
    module_defs: &HashSet<Vec<String>>,
    item_exists: F,
    scope_contains: impl Fn(&str) -> bool,
) -> Option<Vec<String>>
where
    F: Fn(&[String]) -> bool,
{
    if parsed.segments.is_empty() {
        return None;
    }

    match parsed.prefix {
        PathPrefix::Root | PathPrefix::Crate => Some(parsed.segments.clone()),
        PathPrefix::SelfMod => {
            let mut full = module_path.to_vec();
            full.extend(parsed.segments.iter().cloned());
            Some(full)
        }
        PathPrefix::Super(depth) => {
            if depth > module_path.len() {
                return None;
            }
            let mut full = module_path[..module_path.len() - depth].to_vec();
            full.extend(parsed.segments.iter().cloned());
            Some(full)
        }
        PathPrefix::Plain => {
            let first = parsed.segments.first()?;
            if parsed.segments.len() == 1 {
                if scope_contains(first) {
                    return Some(vec![first.clone()]);
                }
                if !module_path.is_empty() {
                    let mut local = module_path.to_vec();
                    local.push(first.clone());
                    if item_exists(&local) || module_defs.contains(&local) {
                        return Some(local);
                    }
                } else if item_exists(&parsed.segments) {
                    return Some(parsed.segments.clone());
                }
                if root_modules.contains(first) || extern_prelude.contains(first) {
                    return Some(parsed.segments.clone());
                }
                return None;
            }

            if !module_path.is_empty() {
                let mut local = module_path.to_vec();
                local.extend(parsed.segments.iter().cloned());
                if item_exists(&local) {
                    return Some(local);
                }
                let mut module_candidate = module_path.to_vec();
                module_candidate.push(first.clone());
                if module_defs.contains(&module_candidate) {
                    return Some(local);
                }
            } else if item_exists(&parsed.segments) {
                return Some(parsed.segments.clone());
            }

            if root_modules.contains(first) || extern_prelude.contains(first) {
                return Some(parsed.segments.clone());
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
    fn parse_segments_prefixes() {
        let parsed = parse_segments(&["self".into(), "foo".into()]).unwrap();
        assert_eq!(parsed.prefix, PathPrefix::SelfMod);
        assert_eq!(parsed.segments, vec!["foo".to_string()]);

        let parsed = parse_segments(&["super".into(), "super".into(), "bar".into()]).unwrap();
        assert_eq!(parsed.prefix, PathPrefix::Super(2));
        assert_eq!(parsed.segments, vec!["bar".to_string()]);

        let parsed = parse_segments(&["__root__".into(), "std".into()]).unwrap();
        assert_eq!(parsed.prefix, PathPrefix::Root);
        assert_eq!(parsed.segments, vec!["std".to_string()]);
    }

    #[test]
    fn resolve_plain_prefers_local_module() {
        let parsed = ParsedPath {
            prefix: PathPrefix::Plain,
            segments: vec!["meta".to_string(), "TypeBuilder".to_string()],
        };
        let mut module_defs = HashSet::new();
        module_defs.insert(vec!["std".to_string(), "meta".to_string()]);
        let resolved = resolve_path(
            &parsed,
            &vec!["std".to_string()],
            &HashSet::new(),
            &HashSet::new(),
            &module_defs,
        )
        .unwrap();
        assert_eq!(
            resolved,
            vec!["std".to_string(), "meta".to_string(), "TypeBuilder".to_string()]
        );
    }

    #[test]
    fn resolve_item_prefers_local_scope() {
        let parsed = ParsedPath {
            prefix: PathPrefix::Plain,
            segments: vec!["TypeBuilder".to_string()],
        };
        let module_path = vec!["std".to_string(), "meta".to_string()];
        let expected = vec![
            "std".to_string(),
            "meta".to_string(),
            "TypeBuilder".to_string(),
        ];
        let resolved = resolve_item_path(
            &parsed,
            &module_path,
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            |segments| segments == expected.as_slice(),
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
        let module_path = vec!["std".to_string(), "meta".to_string()];
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
        assert_eq!(resolved, vec!["TypeBuilder".to_string()]);
    }

    #[test]
    fn resolve_item_plain_module_blocks_extern() {
        let parsed = ParsedPath {
            prefix: PathPrefix::Plain,
            segments: vec!["net".to_string(), "tcp".to_string()],
        };
        let mut module_defs = HashSet::new();
        module_defs.insert(vec!["std".to_string(), "net".to_string()]);
        let resolved = resolve_item_path(
            &parsed,
            &vec!["std".to_string()],
            &HashSet::new(),
            &["net".to_string()].into_iter().collect(),
            &module_defs,
            |_segments| false,
            |_| false,
        )
        .unwrap();
        assert_eq!(
            resolved,
            vec!["std".to_string(), "net".to_string(), "tcp".to_string()]
        );
    }
}
