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
        }
    }
    // Single crate at root: lib.rs / main.rs
    vec![(root.file_name().unwrap_or_default().to_string_lossy().to_string(), root.to_path_buf())]
}

fn parse_toml_members(content: &str, root: &Path) -> Option<Vec<(String, PathBuf)>> {
    // Simple line-based parsing for [workspace] members
    let mut in_workspace = false;
    let mut result = Vec::new();
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed == "[workspace]" {
            in_workspace = true;
        } else if trimmed.starts_with('[') {
            in_workspace = false;
        } else if in_workspace && trimmed.starts_with("members") {
            let value = trimmed.splitn(2, '=').nth(1).unwrap_or("");
            let cleaned = value.trim().trim_start_matches('[').trim_end_matches(']');
            for member in cleaned.split(',') {
                let member = member.trim().trim_matches('"').trim();
                if member.contains('*') {
                    let base = member.split('*').next().unwrap_or("");
                    let parent = root.join(base);
                    if let Ok(entries) = std::fs::read_dir(&parent) {
                        for entry in entries.flatten() {
                            if entry.file_type().map_or(false, |t| t.is_dir()) {
                                let p = entry.path();
                                let _rel = p.strip_prefix(root).unwrap_or(&p);
                                result.push((
                                    p.file_name().unwrap_or_default().to_string_lossy().to_string(),
                                    p,
                                ));
                            }
                        }
                    }
                } else if !member.is_empty() {
                    let p = root.join(member);
                    result.push((
                        p.file_name().unwrap_or_default().to_string_lossy().to_string(),
                        p,
                    ));
                }
            }
        }
    }
    if result.is_empty() { None } else { Some(result) }
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
