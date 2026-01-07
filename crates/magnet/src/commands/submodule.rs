//! Command implementation for managing Git submodules

use eyre::{Context, Result, eyre};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use tracing::{debug, error, info};

/// Git submodule init command - initializes and updates submodules
pub fn init(path: &Path) -> Result<()> {
    info!("Initializing submodules at {}", path.display());
    update_submodules(path, false)
}

/// Git submodule update command - updates submodules
pub fn update(path: &Path, remote: bool) -> Result<()> {
    info!(
        "Updating submodules{} at {}",
        if remote { " with remote changes" } else { "" },
        path.display()
    );
    update_submodules(path, remote)
}

/// Git submodule deinit command - removes a submodule
pub fn deinit(path: &Path, submodule_path: &Path) -> Result<()> {
    info!("Deinitializing submodule at {}", submodule_path.display());

    // Execute git submodule deinit
    execute_git_command(
        path,
        &[
            "submodule",
            "deinit",
            "-f",
            &submodule_path.display().to_string(),
        ],
    )?;

    // Execute git rm --cached
    execute_git_command(
        path,
        &[
            "rm",
            "--cached",
            "-rf",
            &submodule_path.display().to_string(),
        ],
    )?;

    // Remove directory
    let full_path = path.join(submodule_path);
    if full_path.exists() {
        std::fs::remove_dir_all(&full_path).context(format!(
            "Failed to remove directory at {}",
            full_path.display()
        ))?;
    }

    // Remove .git/modules directory
    let git_modules_path = path.join(".git/modules").join(submodule_path);
    if git_modules_path.exists() {
        std::fs::remove_dir_all(&git_modules_path).context(format!(
            "Failed to remove .git/modules directory at {}",
            git_modules_path.display()
        ))?;
    }

    info!("Submodule deinitialized at {}", submodule_path.display());
    info!("Note: You may need to manually edit .gitmodules to remove the entry for this submodule");

    Ok(())
}

/// Git submodule list command - lists all submodules
pub fn list(path: &Path) -> Result<()> {
    info!("Listing submodules at {}", path.display());

    let submodules = get_submodules(path)?;

    if submodules.is_empty() {
        info!("No submodules found.");
    } else {
        info!("Submodules:");
        for submodule in &submodules {
            info!("  {}", submodule.display());
        }
    }

    Ok(())
}

/// Git submodule switch command - switches submodules to a specific revision
pub fn switch(path: &Path, revision: &str) -> Result<()> {
    info!("Switching submodules to revision: {}", revision);

    // Deinit all submodules
    execute_git_command(path, &["submodule", "deinit", "--all", "-f"])?;

    // Switch to the specified revision
    execute_git_command(path, &["switch", "-d", "-f", revision])?;

    // Clean the directory
    execute_git_command(path, &["clean", "-f", "-d", "."])?;

    // Update submodules
    update_submodules(path, false)?;

    info!("Submodules switched to revision: {}", revision);
    Ok(())
}

// Helper function to recursively update submodules
fn update_submodules(path: &Path, remote: bool) -> Result<()> {
    let submodules = get_submodules(path)?;

    let mut success_modules = Vec::new();
    let mut error_modules = Vec::new();

    for submodule in &submodules {
        debug!("Updating submodule: {}", submodule.display());

        let mut args = vec![
            "submodule".to_string(),
            "update".to_string(),
            "--init".to_string(),
        ];
        if remote {
            args.push("--remote".to_string());
        }
        args.push(submodule.display().to_string());
        let args: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
        match execute_git_command(path, &args) {
            Ok(_) => {
                info!("Submodule updated: {}", submodule.display());
                success_modules.push(submodule);

                // Recursively update submodules inside this one
                let full_submodule_path = path.join(submodule);
                if let Err(e) = update_submodules(&full_submodule_path, remote) {
                    debug!(
                        "Error updating nested submodules in {}: {}",
                        submodule.display(),
                        e
                    );
                }
            }
            Err(e) => {
                error!("Error updating submodule {}: {}", submodule.display(), e);
                error_modules.push(submodule);
            }
        }
    }

    info!(
        "Submodule update complete ({}/{} successful)",
        success_modules.len(),
        submodules.len()
    );

    Ok(())
}

// Helper function to get list of submodules
fn get_submodules(path: &Path) -> Result<Vec<PathBuf>> {
    let output = Command::new("git")
        .current_dir(path)
        .args(&["config", "--file", ".gitmodules", "--get-regexp", "path"])
        .output()
        .context("Failed to execute git config to get submodules")?;

    if !output.status.success() {
        let error = String::from_utf8_lossy(&output.stderr);
        if error.contains("No such file or directory") {
            // No .gitmodules file, so no submodules
            return Ok(Vec::new());
        }
        return Err(eyre!("Failed to get submodules: {}", error));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let submodules = stdout
        .lines()
        .filter_map(|line| {
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() >= 2 {
                Some(PathBuf::from(parts[1]))
            } else {
                None
            }
        })
        .collect();

    debug!("Found submodules: {:?}", submodules);
    Ok(submodules)
}

// Helper function to execute a git command
fn execute_git_command(path: &Path, args: &[&str]) -> Result<()> {
    debug!("Executing git command: git {}", args.join(" "));

    let status = Command::new("git")
        .current_dir(path)
        .args(args)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status()
        .context(format!(
            "Failed to execute git command: git {}",
            args.join(" ")
        ))?;

    if !status.success() {
        return Err(eyre!("Git command failed with exit code: {}", status));
    }

    Ok(())
}
