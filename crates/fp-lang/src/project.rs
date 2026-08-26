use std::path::{Path, PathBuf};

/// Find nearest Cargo.toml or Magnet.toml by walking up
pub fn find_manifest(path: &Path) -> Option<PathBuf> {
    let mut current = if path.is_dir() {
        path.to_path_buf()
    } else {
        path.parent()?.to_path_buf()
    };
    loop {
        if current.join("Cargo.toml").exists() || current.join("Magnet.toml").exists() {
            return Some(current);
        }
        if !current.pop() {
            return None;
        }
    }
}

/// Parse workspace members from Cargo.toml or Magnet.toml
pub fn list_members(root: &Path) -> Vec<(String, PathBuf)> {
    for name in &["Magnet.toml", "Cargo.toml"] {
        let path = root.join(name);
        if let Ok(content) = std::fs::read_to_string(&path) {
            if let Some(members) = parse_toml_members(&content, root) {
                return members;
            }
            if let Some(name) = parse_toml_package_name(&content) {
                return vec![(name, root.to_path_buf())];
            }
        }
    }
    // Single crate at root, no manifest at all: lib.rs / main.rs
    vec![(
        root.file_name()
            .unwrap_or_default()
            .to_string_lossy()
            .to_string(),
        root.to_path_buf(),
    )]
}

/// Parse workspace members from `Cargo.toml` only, ignoring any sibling
/// `Magnet.toml` entirely. For a real Rust/Cargo project, `Cargo.toml` is the
/// authoritative workspace manifest — a repo can carry a stale/out-of-sync
/// `Magnet.toml` left over from unrelated FerroPhase tooling (it has its own,
/// independent `[workspace] members` list, meant for `.fp`/Magnet packages,
/// not Cargo crates), and `list_members`'s Magnet-first precedence would
/// silently use that instead of the real Cargo workspace for a Rust source
/// language. Used by [`crate::project::find_manifest`]-driven Rust-specific
/// discovery (see `fp_rust::RustPackageProvider`) instead of `list_members`.
pub fn list_cargo_members(root: &Path) -> Vec<(String, PathBuf)> {
    if let Ok(content) = std::fs::read_to_string(root.join("Cargo.toml")) {
        if let Some(members) = parse_toml_members(&content, root) {
            return members;
        }
        if let Some(name) = parse_toml_package_name(&content) {
            return vec![(name, root.to_path_buf())];
        }
    }
    vec![(
        root.file_name()
            .unwrap_or_default()
            .to_string_lossy()
            .to_string(),
        root.to_path_buf(),
    )]
}

/// A single (non-workspace) manifest's own `[package].name` — the name a
/// standalone Cargo/Magnet crate declares for itself, as opposed to
/// `parse_toml_members`'s `[workspace].members` list for multi-crate repos.
fn parse_toml_package_name(content: &str) -> Option<String> {
    let manifest: toml::Value = content.parse().ok()?;
    manifest
        .get("package")
        .and_then(|package| package.get("name"))
        .and_then(|name| name.as_str())
        .map(str::to_string)
}

fn parse_toml_members(content: &str, root: &Path) -> Option<Vec<(String, PathBuf)>> {
    let manifest: toml::Value = content.parse().ok()?;
    let members = manifest
        .get("workspace")
        .and_then(|workspace| workspace.get("members"))
        .and_then(|members| members.as_array())?;

    let mut result = Vec::new();
    for member in members {
        let Some(member) = member.as_str() else {
            continue;
        };
        if member.contains('*') {
            let base = member.split('*').next().unwrap_or("");
            let parent = root.join(base);
            if let Ok(entries) = std::fs::read_dir(&parent) {
                for entry in entries.flatten() {
                    if entry.file_type().map_or(false, |t| t.is_dir()) {
                        let p = entry.path();
                        result.push((
                            p.file_name()
                                .unwrap_or_default()
                                .to_string_lossy()
                                .to_string(),
                            p,
                        ));
                    }
                }
            }
        } else if !member.is_empty() {
            let p = root.join(member);
            result.push((
                p.file_name()
                    .unwrap_or_default()
                    .to_string_lossy()
                    .to_string(),
                p,
            ));
        }
    }
    if result.is_empty() {
        None
    } else {
        Some(result)
    }
}

/// List all .rs/.fp source files under a crate directory
pub fn list_sources(dir: &Path) -> Vec<(String, PathBuf)> {
    let src = dir.join("src");
    if !src.exists() || !src.is_dir() {
        return Vec::new();
    }
    let mut files = Vec::new();
    walk_dir(&src, &src, &mut files);
    files
}

fn walk_dir(base: &Path, current: &Path, files: &mut Vec<(String, PathBuf)>) {
    if let Ok(entries) = std::fs::read_dir(current) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                walk_dir(base, &path, files);
            } else if path.extension().map_or(false, |e| e == "rs" || e == "fp") {
                let rel = path.strip_prefix(base).unwrap_or(&path);
                files.push((rel.display().to_string(), path));
            }
        }
    }
}
