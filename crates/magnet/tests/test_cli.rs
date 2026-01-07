use std::fs;
use std::process::Command;
use eyre::Result;
use tempfile::tempdir;

#[test]
fn test_magnet_cli_commands() -> Result<()> {
    // Create a temporary directory for our test
    let temp_dir = tempdir()?;
    let magnet_toml_path = temp_dir.path().join("Magnet.toml");
    
    // Get the path to the magnet binary
    let manifest_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = manifest_dir
        .parent()
        .and_then(|p| p.parent())
        .unwrap_or(&manifest_dir)
        .to_path_buf();
    let magnet_bin = match std::env::var("CARGO_BIN_EXE_magnet") {
        Ok(path) => path,
        Err(_) => {
            let bin_name = if cfg!(windows) {
                "magnet.exe"
            } else {
                "magnet"
            };
            workspace_root
                .join("target")
                .join("debug")
                .join(bin_name)
                .to_string_lossy()
                .to_string()
        }
    };
    if !std::path::Path::new(&magnet_bin).exists() {
        let status = Command::new("cargo")
            .current_dir(&workspace_root)
            .args(["build", "-p", "magnet", "--bin", "magnet"])
            .status()?;
        assert!(status.success(), "cargo build failed for magnet");
    }
    
    // Test the init command
    let output = Command::new(&magnet_bin)
        .args(["init", temp_dir.path().to_str().unwrap()])
        .output()?;
    
    assert!(output.status.success(), 
        "magnet init failed with: {}", String::from_utf8_lossy(&output.stderr));
    assert!(magnet_toml_path.exists(), "Magnet.toml was not created");
    
    // Read the generated Magnet.toml and check its contents
    let content = fs::read_to_string(&magnet_toml_path)?;
    assert!(content.contains("[workspace]"), "Magnet.toml missing [workspace] section");
    assert!(
        content.contains("members"),
        "Magnet.toml missing workspace members"
    );

    let project_name = temp_dir
        .path()
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("magnet-project");
    let package_manifest = temp_dir
        .path()
        .join("crates")
        .join(project_name)
        .join("Magnet.toml");
    assert!(
        package_manifest.exists(),
        "Package Magnet.toml was not created"
    );
    
    // Create a crates directory and some test crates
    let crates_dir = temp_dir.path().join("crates");
    fs::create_dir_all(&crates_dir)?;
    
    let crate1_dir = crates_dir.join("crate1");
    let crate2_dir = crates_dir.join("crate2");
    fs::create_dir_all(&crate1_dir)?;
    fs::create_dir_all(&crate2_dir)?;
    
    // Create Cargo.toml files for test crates
    fs::write(
        crate1_dir.join("Cargo.toml"),
        r#"[package]
name = "crate1"
version = "0.1.0"
edition = "2024"

[dependencies]
"#
    )?;
    
    fs::write(
        crate2_dir.join("Cargo.toml"),
        r#"[package]
name = "crate2"
version = "0.1.0"
edition = "2024"

[dependencies]
crate1 = { version = "0.1.0" }
"#
    )?;
    
    // Update Magnet.toml to include our test crates
    fs::write(
        &magnet_toml_path,
        r#"[workspace]
members = ["crates/*"]
exclude = []
resolver = "2"

[dependencies]
serde = "1.0"
"#
    )?;
    
    // Test the tree command
    let output = Command::new(&magnet_bin)
        .args(["tree", temp_dir.path().to_str().unwrap()])
        .current_dir(temp_dir.path())
        .output()?;
    
    assert!(output.status.success(), 
        "magnet tree failed with: {}", String::from_utf8_lossy(&output.stderr));
    
    let output_str = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(output_str.contains("crate1"), "Tree output should include crate1");
    assert!(output_str.contains("crate2"), "Tree output should include crate2");
    
    // Test the generate command
    let output = Command::new(&magnet_bin)
        .args(["generate", temp_dir.path().to_str().unwrap()])
        .current_dir(temp_dir.path())
        .output()?;
    
    assert!(output.status.success(), 
        "magnet generate failed with: {}", String::from_utf8_lossy(&output.stderr));
    
    // Check that Cargo.toml was generated in the workspace root
    let cargo_toml_path = temp_dir.path().join("Cargo.toml");
    assert!(cargo_toml_path.exists(), "Cargo.toml was not created");
    
    // Read the generated Cargo.toml and check its contents
    let content = fs::read_to_string(&cargo_toml_path)?;
    assert!(content.contains("[workspace]"), "Cargo.toml missing [workspace] section");
    assert!(content.contains("members"), "Cargo.toml missing workspace members");
    // Dependency wiring is validated in generator tests; here we only check basics.
    
    // Test the check command
    let output = Command::new(&magnet_bin)
        .args(["check", temp_dir.path().to_str().unwrap()])
        .current_dir(temp_dir.path())
        .output()?;
    
    assert!(output.status.success(), 
        "magnet check failed with: {}", String::from_utf8_lossy(&output.stderr));
    
    Ok(())
}
