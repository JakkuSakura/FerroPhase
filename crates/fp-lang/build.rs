use std::env;
use std::fs;
use std::path::{Path, PathBuf};

fn main() {
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").expect("manifest dir"));
    let std_root = manifest_dir.join("src/std");
    let out_dir = PathBuf::from(env::var("OUT_DIR").expect("out dir"));
    let output = out_dir.join("embedded_std.rs");

    let mut files = Vec::new();
    collect_fp_files(&std_root, &std_root, &mut files);
    files.sort_by(|lhs, rhs| lhs.0.cmp(&rhs.0));

    let mut generated =
        String::from("pub fn get(path: &str) -> Option<&'static str> {\n    match path {\n");
    for (relative, absolute) in files {
        generated.push_str(&format!(
            "        {relative:?} => Some(include_str!({absolute:?})),\n"
        ));
    }
    generated.push_str("        _ => None,\n    }\n}\n");

    fs::write(output, generated).expect("write embedded std");
}

fn collect_fp_files(root: &Path, dir: &Path, files: &mut Vec<(String, String)>) {
    println!("cargo:rerun-if-changed={}", dir.display());

    let mut entries = fs::read_dir(dir)
        .unwrap_or_else(|err| panic!("failed to read {}: {err}", dir.display()))
        .collect::<Result<Vec<_>, _>>()
        .unwrap_or_else(|err| panic!("failed to list {}: {err}", dir.display()));
    entries.sort_by_key(|entry| entry.path());

    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            collect_fp_files(root, &path, files);
            continue;
        }
        if path.extension().and_then(|ext| ext.to_str()) != Some("fp") {
            continue;
        }

        let relative = path
            .strip_prefix(root)
            .expect("relative std path")
            .to_string_lossy()
            .replace('\\', "/");
        files.push((relative, path.to_string_lossy().into_owned()));
    }
}
