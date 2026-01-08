//! Command implementation for initializing Magnet.toml files

use eyre::{Context, Result, bail};
use std::fs;
use std::path::{Path, PathBuf};
use tracing::info;

/// Initialize a new Magnet.toml file at the specified path
pub fn init(path: &Path, from_cargo: bool) -> Result<()> {
    if from_cargo {
        return init_from_cargo(path);
    }

    init_template(path)
}

fn init_template(path: &Path) -> Result<()> {
    let root_dir = resolve_root_dir(path)?;
    ensure_empty_or_new(&root_dir)?;

    let project_name = root_dir
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("magnet-project")
        .to_string();

    fs::create_dir_all(&root_dir).context(format!(
        "Failed to create directory at {}",
        root_dir.display()
    ))?;

    let workspace_toml = r#"[workspace]
members = ["crates/*"]
resolver = "2"
"#;
    let workspace_path = root_dir.join("Magnet.toml");
    fs::write(&workspace_path, workspace_toml).context(format!(
        "Failed to write Magnet.toml to {}",
        workspace_path.display()
    ))?;

    let package_dir = root_dir.join("crates").join(&project_name);
    fs::create_dir_all(package_dir.join("src")).context(format!(
        "Failed to create package directory at {}",
        package_dir.display()
    ))?;

    let package_toml = format!(
        r#"[package]
name = "{name}"
version = "0.1.0"
edition = "2021"
"#,
        name = project_name
    );
    fs::write(package_dir.join("Magnet.toml"), package_toml).context(format!(
        "Failed to write package Magnet.toml for {}",
        project_name
    ))?;

    let lib_rs = format!(
        "pub fn hello() {{\n    println!(\"Hello from {}!\");\n}}\n",
        project_name
    );
    fs::write(package_dir.join("src").join("lib.rs"), lib_rs)
        .context("Failed to write template source file")?;

    info!(
        "Created template project at {}",
        root_dir.join("crates").join(project_name).display()
    );

    Ok(())
}

fn init_from_cargo(path: &Path) -> Result<()> {
    let (cargo_path, target_path) = resolve_cargo_paths(path)?;
    let content = fs::read_to_string(&cargo_path)
        .with_context(|| format!("Failed to read {}", cargo_path.display()))?;

    let value: toml::Value = toml::from_str(&content)
        .with_context(|| format!("Failed to parse {}", cargo_path.display()))?;

    let has_workspace = value.get("workspace").is_some();
    let has_package = value.get("package").is_some();

    if has_workspace && has_package {
        bail!("Cargo.toml contains both [workspace] and [package]; split it before importing");
    }

    if !has_workspace && !has_package {
        bail!("Cargo.toml has no [workspace] or [package] section");
    }

    fs::write(&target_path, content).context(format!(
        "Failed to write Magnet.toml to {}",
        target_path.display()
    ))?;

    info!("Created Magnet.toml at {}", target_path.display());

    Ok(())
}

fn resolve_root_dir(path: &Path) -> Result<PathBuf> {
    if path.is_file() {
        Ok(path.parent().unwrap_or(Path::new(".")).to_path_buf())
    } else {
        Ok(path.to_path_buf())
    }
}

fn resolve_cargo_paths(path: &Path) -> Result<(PathBuf, PathBuf)> {
    if path.is_dir() {
        let cargo_path = path.join("Cargo.toml");
        if !cargo_path.exists() {
            bail!("Cargo.toml not found in {}", path.display());
        }
        let target_path = path.join("Magnet.toml");
        return Ok((cargo_path, target_path));
    }

    let file_name = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("");
    if file_name == "Cargo.toml" {
        let target_path = path.parent().unwrap_or(Path::new(".")).join("Magnet.toml");
        return Ok((path.to_path_buf(), target_path));
    }

    if file_name == "Magnet.toml" {
        let cargo_path = path.parent().unwrap_or(Path::new(".")).join("Cargo.toml");
        if !cargo_path.exists() {
            bail!("Cargo.toml not found next to {}", path.display());
        }
        return Ok((cargo_path, path.to_path_buf()));
    }

    bail!(
        "Expected a directory, Cargo.toml, or Magnet.toml path; got {}",
        path.display()
    );
}

fn ensure_empty_or_new(path: &Path) -> Result<()> {
    if !path.exists() {
        return Ok(());
    }
    if path
        .read_dir()
        .map_err(|e| eyre::eyre!("Failed to read {}: {}", path.display(), e))?
        .next()
        .is_some()
    {
        bail!("Directory '{}' is not empty", path.display());
    }
    Ok(())
}
