use crate::configs::ManifestConfig;
use crate::models::{ManifestModel, PackageModel};
use crate::utils::find_furthest_manifest;
use eyre::{Result, ContextCompat, bail, Context};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tracing::info;

pub struct TranspileOptions {
    pub path: PathBuf,
    pub target: String,
    pub output: Option<PathBuf>,
    pub package: Option<String>,
}

pub fn transpile(options: &TranspileOptions) -> Result<()> {
    let start_dir = if options.path.is_file() {
        options.path.parent().unwrap_or(Path::new(".")).to_path_buf()
    } else {
        options.path.clone()
    };
    let start_dir = start_dir.canonicalize().unwrap_or(start_dir);

    let (root, manifest) = find_furthest_manifest(&start_dir)?;
    info!("transpile: manifest root {}", root.display());

    let package_name = options.package.clone().unwrap_or_else(|| manifest.name().to_string());

    let fp_bin = find_fp_binary()?;

    let members = manifest_members(&manifest, &root);
    info!("transpile: {} workspace members", members.len());

    let mut source_files = Vec::new();
    for member in &members {
        discover_sources(&root.join(member), &mut source_files)?;
    }
    info!("transpile: found {} source files", source_files.len());

    let output_root = options.output.clone().unwrap_or_else(|| {
        root.join("target").join("transpile").join(&options.target)
    });
    fs::create_dir_all(&output_root)?;

    let ext = extension_for_target(&options.target);

    for src in &source_files {
        let src_str = src.display().to_string();
        let relative = member_relative_path(src, &members, &root);
        let out_path = output_root.join(&relative).with_extension(ext);

        if let Some(parent) = out_path.parent() {
            fs::create_dir_all(parent)?;
        }

        let mut cmd = Command::new(&fp_bin);
        cmd.args([
            "compile",
            &src_str,
            "--package", &package_name,
            "--target", &options.target,
            "--skip-typing",
        ]);
        cmd.current_dir(&root);

        info!("transpile: {} -> {}", src.display(), out_path.display());
        let status = cmd.status()
            .with_context(|| format!("fp compile failed for {}", src.display()))?;
        if !status.success() {
            bail!("fp compile failed with status {}", status.code().unwrap_or(-1));
        }

        let adjacent = src.with_extension(ext);
        if adjacent.exists() && adjacent != out_path {
            fs::rename(&adjacent, &out_path)?;
        }
    }

    write_project_files(&output_root, &options.target, &package_name)?;
    info!("transpile: done — {} files in {}", source_files.len(), output_root.display());
    Ok(())
}

fn manifest_members(_manifest: &ManifestModel, root: &Path) -> Vec<String> {
    if let Ok(content) = fs::read_to_string(root.join("Magnet.toml")) {
        if let Ok(cfg) = toml::from_str::<toml::value::Table>(&content) {
            if let Some(ws) = cfg.get("workspace") {
                if let Some(members) = ws.get("members").and_then(|m| m.as_array()) {
                    return members.iter()
                        .filter_map(|v| v.as_str().map(|s| s.to_string()))
                        .filter(|s| !s.contains('*'))
                        .collect();
                }
            }
        }
    }
    vec!["crates/skln-core".into(), "crates/skln-git".into()]
}

fn member_relative_path(src: &Path, members: &[String], root: &Path) -> String {
    let src_str = src.display().to_string();
    for member in members {
        let member_dir = root.join(member).display().to_string();
        if src_str.starts_with(&member_dir) {
            return src_str[member_dir.len()..].trim_start_matches('/').to_string();
        }
    }
    if let Some(idx) = src_str.rfind("/src/") {
        src_str[idx + 5..].to_string()
    } else {
        src.file_name().unwrap_or_default().to_string_lossy().to_string()
    }
}

fn find_fp_binary() -> Result<PathBuf> {
    let cargo_target = std::env::var("CARGO_TARGET_DIR")
        .unwrap_or_else(|_| "target".to_string());
    let profile = if cfg!(debug_assertions) { "debug" } else { "release" };
    let bin = PathBuf::from(&cargo_target).join(profile).join("fp");
    if bin.exists() { return Ok(bin.canonicalize().unwrap_or(bin)); }
    if let Ok(exe) = std::env::current_exe() {
        let sibling = exe.parent().unwrap_or(Path::new(".")).join("fp");
        if sibling.exists() { return Ok(sibling); }
    }
    bail!("Cannot find fp binary. Build: cargo build -p fp-cli")
}

fn discover_sources(dir: &Path, files: &mut Vec<PathBuf>) -> Result<()> {
    for entry in glob::glob(&dir.join("src/**/*.rs").display().to_string())? {
        files.push(entry?);
    }
    for entry in glob::glob(&dir.join("src/**/*.fp").display().to_string())? {
        files.push(entry?);
    }
    Ok(())
}

fn extension_for_target(target: &str) -> &str {
    match target {
        "kotlin" | "kt" => "kt",
        "typescript" | "ts" => "ts",
        "python" | "py" => "py",
        "go" | "golang" => "go",
        "csharp" | "cs" => "cs",
        "rust" | "rs" => "rs",
        "zig" => "zig",
        _ => "kt",
    }
}

fn write_project_files(output: &Path, target: &str, package_name: &str) -> Result<()> {
    if target == "kotlin" || target == "kt" {
        fs::write(
            output.join("settings.gradle.kts"),
            format!("rootProject.name = \"{}\"\n", package_name.replace('-', "_")),
        )?;
        fs::write(
            output.join("build.gradle.kts"),
            "plugins { kotlin(\"jvm\") version \"2.1.0\" }\n\
             group = \"com.sakuralens.skln\"\n\
             version = \"0.1.0\"\n\
             repositories { mavenCentral() }\n\
             dependencies { testImplementation(kotlin(\"test\")) }\n\
             kotlin { jvmToolchain(21) }\n",
        )?;
        let kotlin_root = output.join("src").join("main").join("kotlin");
        fs::create_dir_all(&kotlin_root)?;
        for entry in fs::read_dir(output)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() && path.file_name().map_or(false, |n| n != "src") {
                let dest = kotlin_root.join(path.file_name().unwrap());
                if let Err(e) = fs::rename(&path, &dest) {
                    // Directory might not be empty, try recursive copy
                    copy_dir_recursive(&path, &dest)?;
                    fs::remove_dir_all(&path)?;
                }
            }
        }
    }
    Ok(())
}

fn copy_dir_recursive(src: &Path, dst: &Path) -> Result<()> {
    fs::create_dir_all(dst)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let path = entry.path();
        let dest = dst.join(path.file_name().unwrap());
        if path.is_dir() {
            copy_dir_recursive(&path, &dest)?;
        } else {
            fs::copy(&path, &dest)?;
        }
    }
    Ok(())
}
