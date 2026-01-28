use fp_core::ast::{Value, ValueList, ValueMap};
use fp_core::formats::toml;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

pub type Result<T> = fp_core::Result<T>;

pub fn env_flag_enabled(name: &str) -> bool {
    env::var(name)
        .map(|value| matches!(value.as_str(), "1" | "true" | "yes" | "on"))
        .unwrap_or(false)
}

pub fn find_furthest_manifest(start: &Path) -> Result<(PathBuf, PathBuf)> {
    let mut current = start.canonicalize().unwrap_or_else(|_| start.to_path_buf());
    let mut last_found = None;
    loop {
        let magnet = current.join("Magnet.toml");
        let cargo = current.join("Cargo.toml");
        if magnet.exists() {
            last_found = Some((current.clone(), magnet));
        } else if cargo.exists() {
            last_found = Some((current.clone(), cargo));
        }
        if !current.pop() {
            break;
        }
    }
    last_found.ok_or_else(|| "no Magnet.toml or Cargo.toml found".to_string().into())
}

pub fn read_toml_file(path: &Path) -> Result<Value> {
    let contents = fs::read_to_string(path)?;
    toml::parse_value(&contents)
}

pub fn write_toml_file(path: &Path, value: &Value) -> Result<()> {
    let payload = toml::to_string_pretty(value)?;
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(path, payload)?;
    Ok(())
}

pub fn get_table<'a>(value: &'a Value, key: &str) -> Option<&'a ValueMap> {
    match value {
        Value::Map(map) => map.entries.iter().find_map(|entry| match &entry.key {
            Value::String(s) if s.value == key => match &entry.value {
                Value::Map(map) => Some(map),
                _ => None,
            },
            _ => None,
        }),
        _ => None,
    }
}

pub fn get_table_value<'a>(value: &'a Value, key: &str) -> Option<&'a Value> {
    match value {
        Value::Map(map) => map.entries.iter().find_map(|entry| match &entry.key {
            Value::String(s) if s.value == key => Some(&entry.value),
            _ => None,
        }),
        _ => None,
    }
}

pub fn get_string(value: &Value) -> Option<String> {
    match value {
        Value::String(s) => Some(s.value.clone()),
        _ => None,
    }
}

pub fn get_bool(value: &Value) -> Option<bool> {
    match value {
        Value::Bool(b) => Some(b.value),
        _ => None,
    }
}

pub fn get_string_list(value: &Value) -> Vec<String> {
    match value {
        Value::List(list) => list
            .values
            .iter()
            .filter_map(get_string)
            .collect::<Vec<_>>(),
        _ => Vec::new(),
    }
}

pub fn map_get<'a>(map: &'a ValueMap, key: &str) -> Option<&'a Value> {
    map.entries.iter().find_map(|entry| match &entry.key {
        Value::String(s) if s.value == key => Some(&entry.value),
        _ => None,
    })
}

pub fn map_get_string(map: &ValueMap, key: &str) -> Option<String> {
    map_get(map, key).and_then(get_string)
}

pub fn map_get_bool(map: &ValueMap, key: &str) -> Option<bool> {
    map_get(map, key).and_then(get_bool)
}

pub fn map_get_string_list(map: &ValueMap, key: &str) -> Vec<String> {
    map_get(map, key)
        .map(get_string_list)
        .unwrap_or_default()
}

pub fn glob_relative(root: &Path, pattern: &str) -> Result<Vec<PathBuf>> {
    let mut out = Vec::new();
    let parts = split_pattern(pattern);
    expand_glob(root, &parts, &mut out)?;
    Ok(out)
}

fn split_pattern(pattern: &str) -> Vec<String> {
    pattern
        .split(|c| c == '/' || c == '\\')
        .filter(|segment| !segment.is_empty())
        .map(|segment| segment.to_string())
        .collect()
}

fn expand_glob(root: &Path, parts: &[String], out: &mut Vec<PathBuf>) -> Result<()> {
    if parts.is_empty() {
        out.push(root.to_path_buf());
        return Ok(());
    }
    let (head, tail) = parts.split_first().unwrap();
    if head.contains('*') {
        let entries = match fs::read_dir(root) {
            Ok(entries) => entries,
            Err(_) => return Ok(()),
        };
        for entry in entries {
            let entry = entry?;
            let name = entry.file_name();
            let name = name.to_string_lossy();
            if match_segment(&name, head) {
                expand_glob(&entry.path(), tail, out)?;
            }
        }
    } else {
        expand_glob(&root.join(head), tail, out)?;
    }
    Ok(())
}

fn match_segment(name: &str, pattern: &str) -> bool {
    if pattern == "*" {
        return true;
    }
    let mut remainder = name;
    let mut first = true;
    for part in pattern.split('*') {
        if part.is_empty() {
            continue;
        }
        if let Some(idx) = remainder.find(part) {
            if first && !pattern.starts_with('*') && idx != 0 {
                return false;
            }
            remainder = &remainder[idx + part.len()..];
        } else {
            return false;
        }
        first = false;
    }
    if !pattern.ends_with('*') && !remainder.is_empty() {
        return false;
    }
    true
}

pub fn collect_sources(root: &Path, extensions: &[&str]) -> Result<Vec<PathBuf>> {
    let mut out = Vec::new();
    if root.exists() {
        collect_sources_inner(root, extensions, &mut out)?;
    }
    Ok(out)
}

fn collect_sources_inner(root: &Path, extensions: &[&str], out: &mut Vec<PathBuf>) -> Result<()> {
    for entry in fs::read_dir(root)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            collect_sources_inner(&path, extensions, out)?;
        } else if let Some(ext) = path.extension().and_then(|ext| ext.to_str()) {
            if extensions.iter().any(|needle| needle.eq_ignore_ascii_case(ext)) {
                out.push(path);
            }
        }
    }
    Ok(())
}

pub fn build_kv_list(map: &ValueMap) -> Vec<String> {
    let mut out = Vec::new();
    for entry in &map.entries {
        let key = match &entry.key {
            Value::String(s) => s.value.clone(),
            _ => continue,
        };
        let value = match &entry.value {
            Value::String(s) => s.value.clone(),
            Value::Int(i) => i.value.to_string(),
            Value::Decimal(d) => d.value.to_string(),
            Value::Bool(b) => b.value.to_string(),
            Value::List(list) => list
                .values
                .iter()
                .filter_map(get_string)
                .collect::<Vec<_>>()
                .join(","),
            _ => continue,
        };
        out.push(format!("{key}={value}"));
    }
    out
}

pub fn to_string_list(values: &[String]) -> Value {
    Value::List(ValueList::new(
        values
            .iter()
            .cloned()
            .map(Value::string)
            .collect::<Vec<_>>(),
    ))
}
