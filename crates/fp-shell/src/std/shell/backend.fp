#[cfg(target_lang = "bash")]
#[command = "bash -lc"]
extern "bash" fn bash(cmd: str);
#[cfg(target_lang = "pwsh")]
#[command = "Invoke-Expression"]
extern "pwsh" fn invoke_expression(cmd: str);

#[cfg(target_lang = "bash")]
#[command = "ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=$SSH_CONTROL_PATH"]
extern "bash" fn ssh(target: str, cmd: str);
#[cfg(target_lang = "pwsh")]
#[command = "ssh"]
extern "pwsh" fn ssh(target: str, cmd: str);

#[cfg(target_lang = "bash")]
#[command = "ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=$SSH_CONTROL_PATH -p"]
extern "bash" fn ssh_port(port: str, target: str, cmd: str);
#[cfg(target_lang = "pwsh")]
#[command = "ssh -p"]
extern "pwsh" fn ssh_port(port: str, target: str, cmd: str);

#[cfg(target_lang = "bash")]
#[command = "docker exec"]
extern "bash" fn docker_exec(container: str, shell: str, flag: str, cmd: str);
#[cfg(target_lang = "pwsh")]
#[command = "docker exec"]
extern "pwsh" fn docker_exec(container: str, shell: str, flag: str, cmd: str);

#[cfg(target_lang = "bash")]
#[command = "docker exec --user"]
extern "bash" fn docker_exec_user(user: str, container: str, shell: str, flag: str, cmd: str);
#[cfg(target_lang = "pwsh")]
#[command = "docker exec --user"]
extern "pwsh" fn docker_exec_user(user: str, container: str, shell: str, flag: str, cmd: str);

#[cfg(target_lang = "bash")]
#[command = "pwsh"]
extern "bash" fn winrm_run(host: str, cmd: str);
#[cfg(target_lang = "pwsh")]
#[command = "New-PSSession Invoke-Command Remove-PSSession"]
extern "pwsh" fn winrm_run(host: str, cmd: str);

#[cfg(target_lang = "bash")]
#[command = "chroot"]
extern "bash" fn chroot_exec(directory: str, shell: str, flag: str, cmd: str);
#[cfg(target_lang = "pwsh")]
#[command = "chroot"]
extern "pwsh" fn chroot_exec(directory: str, shell: str, flag: str, cmd: str);

#[cfg(target_lang = "bash")]
#[command = "cp --"]
extern "bash" fn cp(src: str, dest: str);
#[cfg(target_lang = "pwsh")]
#[command = "Copy-Item -Force"]
extern "pwsh" fn copy_item(src: str, dest: str);

#[cfg(target_lang = "bash")]
#[command = "scp -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=$SSH_CONTROL_PATH"]
extern "bash" fn scp(src: str, dest: str);
#[cfg(target_lang = "pwsh")]
#[command = "scp"]
extern "pwsh" fn scp(src: str, dest: str);

#[cfg(target_lang = "bash")]
#[command = "scp -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=$SSH_CONTROL_PATH -P"]
extern "bash" fn scp_port(port: str, src: str, dest: str);
#[cfg(target_lang = "pwsh")]
#[command = "scp -P"]
extern "pwsh" fn scp_port(port: str, src: str, dest: str);

#[cfg(target_lang = "bash")]
#[command = "docker cp"]
extern "bash" fn docker_cp(src: str, dest: str);
#[cfg(target_lang = "pwsh")]
#[command = "docker cp"]
extern "pwsh" fn docker_cp(src: str, dest: str);

#[cfg(target_lang = "bash")]
#[command = "pwsh"]
extern "bash" fn winrm_copy(host: str, src: str, dest: str);
#[cfg(target_lang = "pwsh")]
#[command = "New-PSSession Invoke-Command Copy-Item Remove-PSSession"]
extern "pwsh" fn winrm_copy(host: str, src: str, dest: str);

#[cfg(target_lang = "bash")]
extern "bash" fn runtime_temp_path() -> str;
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn runtime_temp_path() -> str;

#[cfg(target_lang = "bash")]
#[command = "envsubst"]
extern "bash" fn render_template(src: str, dest: str, vars: str);
#[cfg(target_lang = "pwsh")]
#[command = "Get-Content Set-Content"]
extern "pwsh" fn render_template(src: str, dest: str, vars: str);

#[cfg(target_lang = "bash")]
#[command = "rm -f"]
extern "bash" fn remove_file(path: str);
#[cfg(target_lang = "pwsh")]
#[command = "Remove-Item -Force -ErrorAction SilentlyContinue"]
extern "pwsh" fn remove_file(path: str);

#[cfg(target_lang = "bash")]
#[command = "rsync -e \"ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=$SSH_CONTROL_PATH\""]
extern "bash" fn rsync_cli(flags: str, src: str, dest: str);
#[cfg(target_lang = "pwsh")]
#[command = "rsync"]
extern "pwsh" fn rsync_cli(flags: str, src: str, dest: str);

#[cfg(target_lang = "bash")]
#[command = "rsync -e"]
extern "bash" fn rsync_cli_shell(shell: str, flags: str, src: str, dest: str);
#[cfg(target_lang = "pwsh")]
#[command = "rsync -e"]
extern "pwsh" fn rsync_cli_shell(shell: str, flags: str, src: str, dest: str);

#[cfg(target_lang = "bash")]
extern "bash" fn runtime_fail(message: str);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn runtime_fail(message: str);

#[cfg(target_lang = "bash")]
extern "bash" fn runtime_set_changed(changed: bool);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn runtime_set_changed(changed: bool);

#[cfg(target_lang = "bash")]
extern "bash" fn runtime_last_changed() -> bool;
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn runtime_last_changed() -> bool;

#[cfg(target_lang = "bash")]
extern "bash" fn runtime_record_change(op: str, target: str, summary: str, changed: bool);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn runtime_record_change(op: str, target: str, summary: str, changed: bool);

#[cfg(target_lang = "bash")]
extern "bash" fn runtime_change_summary() -> str;
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn runtime_change_summary() -> str;

#[cfg(target_lang = "bash")]
extern "bash" fn runtime_clear_change_summary();
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn runtime_clear_change_summary();

#[cfg(target_lang = "bash")]
const fn run_local_host(cmd: str) {
    bash(cmd)
}

#[cfg(target_lang = "pwsh")]
const fn run_local_host(cmd: str) {
    invoke_expression(cmd)
}

#[cfg(target_lang = "bash")]
const fn copy_local_host(src: str, dest: str) {
    cp(src, dest)
}

#[cfg(target_lang = "pwsh")]
const fn copy_local_host(src: str, dest: str) {
    copy_item(src, dest)
}

const fn run_host(host: str, cmd: str) {
    let transport = match host {
        "localhost" => "local",
        _ => host.transport,
    };
    match transport {
        "local" => run_local_host(cmd),
        "ssh" => run_ssh_host(host, cmd),
        "docker" => run_docker_host(host, cmd),
        "kubectl" => run_kubectl_host(host, cmd),
        "winrm" => winrm_run(host, cmd),
        "chroot" => run_chroot_host(host, cmd),
        _ => runtime_fail(f"unsupported transport: {transport}"),
    }
}

const fn copy_host(host: str, src: str, dest: str) {
    let transport = match host {
        "localhost" => "local",
        _ => host.transport,
    };
    match transport {
        "local" => copy_local_host(src, dest),
        "ssh" => copy_ssh_host(host, src, dest),
        "docker" => copy_docker_host(host, src, dest),
        "kubectl" => copy_kubectl_host(host, src, dest),
        "winrm" => winrm_copy(host, src, dest),
        "chroot" => copy_chroot_host(host, src, dest),
        _ => runtime_fail(f"unsupported transport for copy: {transport}"),
    }
}

const fn template_host(host: str, src: str, dest: str, vars: str) {
    let tmp = runtime_temp_path();
    render_template(src, tmp, vars);
    copy_host(host, tmp, dest);
    remove_file(tmp);
}

const fn rsync_host(host: str, flags: str, src: str, dest: str) {
    let transport = match host {
        "localhost" => "local",
        _ => host.transport,
    };
    match transport {
        "local" => rsync_cli(flags, src, dest),
        "chroot" => rsync_chroot_host(host, flags, src, dest),
        _ => rsync_remote_host(host, flags, src, dest),
    }
}

const fn ssh_target(host: str) -> str {
    let user = host.user;
    let address = host.address;
    if user != "" {
        f"{user}@{address}"
    } else {
        address
    }
}

const fn run_ssh_host(host: str, cmd: str) {
    let target = ssh_target(host);
    let port = host.port;
    if port != "" {
        ssh_port(port, target, cmd);
    } else {
        ssh(target, cmd);
    }
}

const fn copy_ssh_host(host: str, src: str, dest: str) {
    let target = ssh_target(host);
    let remote = f"{target}:{dest}";
    let port = host.port;
    if port != "" {
        scp_port(port, src, remote);
    } else {
        scp(src, remote);
    }
}

const fn run_docker_host(host: str, cmd: str) {
    let container = host.container;
    let user = host.user;
    if user != "" {
        docker_exec_user(user, container, "sh", "-lc", cmd);
    } else {
        docker_exec(container, "sh", "-lc", cmd);
    }
}

const fn run_kubectl_host(host: str, cmd: str) {
    let context = host.context;
    let namespace = host.namespace;
    let container = host.container;
    let pod = host.pod;
    let context_part = if context != "" { f" --context {context}" } else { "" };
    let ns_part = if namespace != "" { f" -n {namespace}" } else { "" };
    let container_part = if container != "" { f" -c {container}" } else { "" };
    let kubectl_cmd = f"kubectl{context_part}{ns_part} exec{container_part} {pod} -- sh -lc {cmd}";
    run_local_host(kubectl_cmd);
}

const fn run_chroot_host(host: str, cmd: str) {
    chroot_exec(host.chroot_directory, "sh", "-lc", cmd);
}

const fn copy_docker_host(host: str, src: str, dest: str) {
    let container = host.container;
    docker_cp(src, f"{container}:{dest}");
}

const fn copy_kubectl_host(host: str, src: str, dest: str) {
    let context = host.context;
    let namespace = host.namespace;
    let remote = f"{host.pod}:{dest}";
    let context_part = if context != "" { f" --context {context}" } else { "" };
    let ns_part = if namespace != "" { f" -n {namespace}" } else { "" };
    let kubectl_cp_cmd = f"kubectl{context_part}{ns_part} cp {src} {remote}";
    run_local_host(kubectl_cp_cmd);
}

const fn chroot_path(host: str, path: str) -> str {
    f"{host.chroot_directory}{path}"
}

const fn copy_chroot_host(host: str, src: str, dest: str) {
    copy_local_host(src, chroot_path(host, dest));
}

const fn rsync_remote_target(host: str) -> str {
    let address = host.address;
    if address == "" {
        runtime_fail(f"host is not rsync-reachable: missing address for {host}");
        ""
    } else {
        let user = host.user;
        if user != "" {
            f"{user}@{address}"
        } else {
            address
        }
    }
}

const fn rsync_remote_host(host: str, flags: str, src: str, dest: str) {
    let remote = f"{rsync_remote_target(host)}:{dest}";
    let port = host.port;
    if port != "" {
        rsync_cli_shell(f"ssh -p {port}", flags, src, remote);
    } else {
        rsync_cli(flags, src, remote);
    }
}

const fn rsync_chroot_host(host: str, flags: str, src: str, dest: str) {
    let target = chroot_path(host, dest);
    rsync_cli(flags, src, target);
}

const fn command_with_options(command: str, cwd: str, sudo: bool) -> str {
    if cwd != "" {
        if sudo {
            f"sudo cd {cwd} && {command}"
        } else {
            f"cd {cwd} && {command}"
        }
    } else {
        if sudo {
            f"sudo {command}"
        } else {
            command
        }
    }
}

const fn process_ok(command: str) -> bool {
    std::shell::process::ok(command)
}

const fn rsync_flag_string(archive: bool, compress: bool, delete: bool, checksum: bool) -> str {
    if archive {
        if compress {
            rsync_flag_string_suffix("-az", delete, checksum)
        } else {
            rsync_flag_string_suffix("-a", delete, checksum)
        }
    } else {
        if compress {
            rsync_flag_string_suffix("-z", delete, checksum)
        } else {
            rsync_flag_string_suffix("", delete, checksum)
        }
    }
}

const fn rsync_flag_string_suffix(base: str, delete: bool, checksum: bool) -> str {
    if delete {
        if checksum {
            if base != "" {
                f"{base} --delete --checksum"
            } else {
                "--delete --checksum"
            }
        } else {
            if base != "" {
                f"{base} --delete"
            } else {
                "--delete"
            }
        }
    } else {
        if checksum {
            if base != "" {
                f"{base} --checksum"
            } else {
                "--checksum"
            }
        } else {
            base
        }
    }
}

const fn should_apply(only_if: str, unless: str, creates: str, removes: str) -> bool {
    if only_if != "" {
        if !process_ok(only_if) {
            return false;
        }
    }
    if unless != "" {
        if process_ok(unless) {
            return false;
        }
    }
    if creates != "" {
        if !process_ok(f"test ! -e {creates}") {
            return false;
        }
    }
    if removes != "" {
        if !process_ok(f"test -e {removes}") {
            return false;
        }
    }
    true
}

pub const fn shell_run(host: str, command: str, only_if: str, unless: str, creates: str, removes: str) {
    if !should_apply(only_if, unless, creates, removes) {
        return;
    }
    run_host(host, command);
}

pub const fn shell_run_local(host: str, command: str, only_if: str, unless: str, creates: str, removes: str) {
    if !should_apply(only_if, unless, creates, removes) {
        return;
    }
    run_local_host(command);
}

pub const fn shell_run_ssh(host: str, command: str, only_if: str, unless: str, creates: str, removes: str) {
    if !should_apply(only_if, unless, creates, removes) {
        return;
    }
    run_ssh_host(host, command);
}

pub const fn shell_run_docker(host: str, command: str, only_if: str, unless: str, creates: str, removes: str) {
    if !should_apply(only_if, unless, creates, removes) {
        return;
    }
    run_docker_host(host, command);
}

pub const fn shell_run_kubectl(host: str, command: str, only_if: str, unless: str, creates: str, removes: str) {
    if !should_apply(only_if, unless, creates, removes) {
        return;
    }
    run_kubectl_host(host, command);
}

pub const fn shell_run_winrm(host: str, command: str, only_if: str, unless: str, creates: str, removes: str) {
    if !should_apply(only_if, unless, creates, removes) {
        return;
    }
    winrm_run(host, command);
}

pub const fn shell_run_chroot(host: str, command: str, only_if: str, unless: str, creates: str, removes: str) {
    if !should_apply(only_if, unless, creates, removes) {
        return;
    }
    run_chroot_host(host, command);
}

const fn shell_copy(host: str, src: str, dest: str, only_if: str, unless: str, creates: str, removes: str) {
    if !should_apply(only_if, unless, creates, removes) {
        return;
    }
    copy_host(host, src, dest);
}

const fn shell_copy_local(host: str, src: str, dest: str, only_if: str, unless: str, creates: str, removes: str) {
    if !should_apply(only_if, unless, creates, removes) {
        return;
    }
    copy_local_host(src, dest);
}

const fn shell_copy_ssh(host: str, src: str, dest: str, only_if: str, unless: str, creates: str, removes: str) {
    if !should_apply(only_if, unless, creates, removes) {
        return;
    }
    copy_ssh_host(host, src, dest);
}

const fn shell_copy_docker(host: str, src: str, dest: str, only_if: str, unless: str, creates: str, removes: str) {
    if !should_apply(only_if, unless, creates, removes) {
        return;
    }
    copy_docker_host(host, src, dest);
}

const fn shell_copy_kubectl(host: str, src: str, dest: str, only_if: str, unless: str, creates: str, removes: str) {
    if !should_apply(only_if, unless, creates, removes) {
        return;
    }
    copy_kubectl_host(host, src, dest);
}

const fn shell_copy_winrm(host: str, src: str, dest: str, only_if: str, unless: str, creates: str, removes: str) {
    if !should_apply(only_if, unless, creates, removes) {
        return;
    }
    winrm_copy(host, src, dest);
}

const fn shell_copy_chroot(host: str, src: str, dest: str, only_if: str, unless: str, creates: str, removes: str) {
    if !should_apply(only_if, unless, creates, removes) {
        return;
    }
    copy_chroot_host(host, src, dest);
}

const fn template_run(host: str, src: str, dest: str, vars: str, only_if: str, unless: str, creates: str, removes: str) {
    if !should_apply(only_if, unless, creates, removes) {
        return;
    }
    template_host(host, src, dest, vars);
}

const fn rsync_run(host: str, flags: str, src: str, dest: str, only_if: str, unless: str, creates: str, removes: str) {
    if !should_apply(only_if, unless, creates, removes) {
        return;
    }
    rsync_host(host, flags, src, dest);
}
