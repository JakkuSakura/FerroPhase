use crate::frontend::collect_import_references;
use std::collections::{HashSet, VecDeque};
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct ResolvedModule {
    pub path: PathBuf,
    pub source: String,
}

#[derive(Debug, Default, Clone)]
pub struct ResolveOutcome {
    pub modules: Vec<ResolvedModule>,
    pub warnings: Vec<String>,
}

pub fn resolve_imports(root_path: &Path, root_source: &str) -> ResolveOutcome {
    let mut outcome = ResolveOutcome::default();
    let mut visited = HashSet::new();
    visited.insert(normalize_path(root_path));
    let mut pending = VecDeque::new();

    enqueue_imports(
        root_path,
        root_source,
        &mut visited,
        &mut pending,
        &mut outcome.warnings,
    );

    while let Some(path) = pending.pop_front() {
        match fs::read_to_string(&path) {
            Ok(source) => {
                enqueue_imports(
                    &path,
                    &source,
                    &mut visited,
                    &mut pending,
                    &mut outcome.warnings,
                );
                outcome.modules.push(ResolvedModule { path, source });
            }
            Err(err) => outcome.warnings.push(format!(
                "Warning: failed to read resolved import {} ({err})",
                path.display()
            )),
        }
    }

    outcome
}

pub fn is_typescript_like_source(path: &Path) -> bool {
    match path.extension().and_then(|ext| ext.to_str()) {
        Some(ext) => {
            let ext = ext.to_ascii_lowercase();
            matches!(
                ext.as_str(),
                "ts" | "tsx" | "mts" | "cts" | "js" | "jsx" | "mjs"
            ) && !path
                .file_name()
                .and_then(|name| name.to_str())
                .map(|name| {
                    name.ends_with(".d.ts") || name.ends_with(".d.mts") || name.ends_with(".d.cts")
                })
                .unwrap_or(false)
        }
        None => false,
    }
}

fn enqueue_imports(
    current_path: &Path,
    source: &str,
    visited: &mut HashSet<PathBuf>,
    pending: &mut VecDeque<PathBuf>,
    warnings: &mut Vec<String>,
) {
    let references = collect_import_references(source, Some(current_path));
    let base_dir = current_path.parent().unwrap_or_else(|| Path::new("."));

    for reference in references {
        if reference.is_type_only {
            continue;
        }
        if !is_local_spec(&reference.spec) {
            continue;
        }

        let spec = strip_query_fragment(&reference.spec);
        let target = base_dir.join(spec.trim_end_matches('/'));
        let mut handled = false;

        for candidate in candidate_import_paths(&target) {
            if !candidate.exists() {
                continue;
            }
            let normalized = normalize_path(&candidate);
            if !visited.insert(normalized.clone()) {
                handled = true;
                break;
            }
            if is_typescript_like_source(&normalized) {
                pending.push_back(normalized);
            } else if is_skipped_extension(&normalized) {
                warnings.push(format!(
                    "Warning: unsupported import '{}' from {} (skipping)",
                    reference.spec,
                    current_path.display()
                ));
            } else {
                warnings.push(format!(
                    "Warning: unable to parse import '{}' from {} (unsupported extension)",
                    reference.spec,
                    current_path.display()
                ));
            }
            handled = true;
            break;
        }

        if !handled {
            warnings.push(format!(
                "Warning: unable to resolve import '{}' from {}",
                reference.spec,
                current_path.display()
            ));
        }
    }
}

fn candidate_import_paths(base: &Path) -> Vec<PathBuf> {
    const FILE_EXTS: &[&str] = &["ts", "tsx", "mts", "cts", "js", "jsx", "mjs"];
    const INDEX_FILES: &[&str] = &[
        "index.ts",
        "index.tsx",
        "index.mts",
        "index.cts",
        "index.js",
        "index.jsx",
        "index.mjs",
    ];

    let mut candidates = Vec::new();
    candidates.push(base.to_path_buf());

    if let Some(ext) = base.extension().and_then(|ext| ext.to_str()) {
        match ext.to_ascii_lowercase().as_str() {
            "js" | "jsx" | "mjs" => {
                candidates.push(set_extension(base, "ts"));
                candidates.push(set_extension(base, "tsx"));
            }
            "d.ts" | "d.mts" | "d.cts" => {
                candidates.push(set_extension(base, "ts"));
            }
            _ => {}
        }
    }

    if base.extension().is_none() {
        for ext in FILE_EXTS {
            candidates.push(base.with_extension(ext));
        }
        for index in INDEX_FILES {
            candidates.push(base.join(index));
        }
    }

    candidates
}

fn set_extension(path: &Path, ext: &str) -> PathBuf {
    let mut cloned = path.to_path_buf();
    cloned.set_extension(ext);
    cloned
}

fn strip_query_fragment(spec: &str) -> &str {
    spec.split(|ch| ch == '?' || ch == '#')
        .next()
        .unwrap_or(spec)
}

fn is_local_spec(spec: &str) -> bool {
    spec.starts_with("./") || spec.starts_with("../") || spec.starts_with('/')
}

fn is_skipped_extension(path: &Path) -> bool {
    matches!(path.extension().and_then(|ext| ext.to_str()).map(|s| s.to_ascii_lowercase()), Some(ref ext) if ext == "cjs")
}

fn normalize_path(path: &Path) -> PathBuf {
    match path.canonicalize() {
        Ok(path) => path,
        Err(_) => path.to_path_buf(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn resolves_js_to_ts_if_available() {
        let tmp = tempdir().unwrap();
        let root = tmp.path();
        fs::create_dir_all(root.join("src")).unwrap();
        fs::write(root.join("src/lib.ts"), "export const value = 1;\n").unwrap();
        fs::write(root.join("entry.ts"), "import value from './src/lib.js';").unwrap();

        let outcome = resolve_imports(
            root.join("entry.ts").as_path(),
            "import value from './src/lib.js';",
        );
        assert!(outcome.warnings.is_empty());
        assert_eq!(outcome.modules.len(), 1);
        assert!(outcome.modules[0].path.ends_with("src/lib.ts"));
    }

    #[test]
    fn skips_cjs_with_warning() {
        let tmp = tempdir().unwrap();
        let root = tmp.path();
        fs::write(root.join("entry.ts"), "import pkg from './shim.cjs';").unwrap();
        fs::write(root.join("shim.cjs"), "module.exports = {};").unwrap();

        let outcome = resolve_imports(
            root.join("entry.ts").as_path(),
            "import pkg from './shim.cjs';",
        );
        assert!(outcome.modules.is_empty());
        assert_eq!(outcome.warnings.len(), 1);
        assert!(outcome.warnings[0].contains("unsupported"));
    }
}
