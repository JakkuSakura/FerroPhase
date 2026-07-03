use std::env;
use std::fs;
use std::path::{Path, PathBuf};

fn main() {
    if let Err(err) = run() {
        eprintln!("build error: {err}");
        std::process::exit(1);
    }
}

fn run() -> Result<(), String> {
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").map_err(|e| e.to_string())?);
    let std_root = manifest_dir.join("src/std");
    let out_dir = PathBuf::from(env::var("OUT_DIR").map_err(|e| e.to_string())?);
    let output = out_dir.join("embedded_shell_std.rs");

    let mut files = Vec::new();
    collect_fp_files(&std_root, &std_root, &mut files)?;
    files.sort_by(|lhs, rhs| lhs.0.cmp(&rhs.0));

    let mut generated = String::from("pub const PATHS: &[&str] = &[\n");
    for (relative, _) in &files {
        generated.push_str(&format!("    {relative:?},\n"));
    }
    generated.push_str("];\n\n");
    generated.push_str("pub fn get(path: &str) -> Option<&'static str> {\n    match path {\n");
    for (relative, absolute) in files {
        generated.push_str(&format!(
            "        {relative:?} => Some(include_str!({absolute:?})),\n"
        ));
    }
    generated.push_str("        _ => None,\n    }\n}\n");

    fs::write(output, generated).map_err(|err| format!("write embedded shell std: {err}"))
}

fn collect_fp_files(
    root: &Path,
    dir: &Path,
    files: &mut Vec<(String, String)>,
) -> Result<(), String> {
    println!("cargo:rerun-if-changed={}", dir.display());

    let mut entries = fs::read_dir(dir)
        .map_err(|err| format!("failed to read {}: {err}", dir.display()))?
        .collect::<Result<Vec<_>, _>>()
        .map_err(|err| format!("failed to list {}: {err}", dir.display()))?;
    entries.sort_by_key(|entry| entry.path());

    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            collect_fp_files(root, &path, files)?;
            continue;
        }
        if path.extension().and_then(|ext| ext.to_str()) != Some("fp") {
            continue;
        }

        let relative = path
            .strip_prefix(root)
            .map_err(|err| format!("relative shell std path: {err}"))?
            .to_string_lossy()
            .replace('\\', "/");
        files.push((relative, path.to_string_lossy().into_owned()));
    }
    Ok(())
}
