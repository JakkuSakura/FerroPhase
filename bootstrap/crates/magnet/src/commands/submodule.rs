use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

pub fn run(args: &[String]) -> crate::Result<()> {
    if args.is_empty() {
        return Err("missing submodule action".to_string().into());
    }
    let action = args[0].as_str();
    let mut path: Option<PathBuf> = None;
    let mut remote: Option<String> = None;
    let mut rev: Option<String> = None;
    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--remote" => {
                let Some(value) = args.get(i + 1) else {
                    return Err("missing --remote".to_string().into());
                };
                remote = Some(value.clone());
                i += 2;
            }
            "--rev" => {
                let Some(value) = args.get(i + 1) else {
                    return Err("missing --rev".to_string().into());
                };
                rev = Some(value.clone());
                i += 2;
            }
            value => {
                if path.is_none() {
                    path = Some(PathBuf::from(value));
                    i += 1;
                } else {
                    return Err(format!("unexpected argument: {value}").into());
                }
            }
        }
    }
    let root = path.unwrap_or_else(|| PathBuf::from("."));

    match action {
        "list" => git_command(&root, &["submodule", "status"]),
        "init" => git_command(&root, &["submodule", "update", "--init", "--recursive"]),
        "update" => git_command(&root, &["submodule", "update", "--recursive"]),
        "deinit" => {
            let Some(target) = remote else {
                return Err("--remote <path> required for deinit".to_string().into());
            };
            git_command(&root, &["submodule", "deinit", "-f", &target])
        }
        "switch" => {
            let Some(target) = remote else {
                return Err("--remote <path> required for switch".to_string().into());
            };
            let Some(rev) = rev else {
                return Err("--rev <rev> required for switch".to_string().into());
            };
            git_command(&root, &["submodule", "update", "--remote", "--", &target])?;
            let target_path = root.join(target);
            git_command(&target_path, &["checkout", &rev])
        }
        _ => Err(format!("unknown submodule action '{action}'").into()),
    }
}

fn git_command(root: &Path, args: &[&str]) -> crate::Result<()> {
    let mut cmd = Command::new("git");
    cmd.args(args);
    cmd.current_dir(root);
    cmd.stdin(Stdio::inherit());
    cmd.stdout(Stdio::inherit());
    cmd.stderr(Stdio::inherit());
    let status = cmd.status()?;
    if !status.success() {
        return Err("git command failed".to_string().into());
    }
    Ok(())
}
