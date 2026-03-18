#[cfg(target_lang = "bash")]
extern "bash" fn runtime_host_transport(host: str) -> str;
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn runtime_host_transport(host: str) -> str;

#[cfg(target_lang = "bash")]
#[command = "bash"]
extern "bash" fn bash(cmd: str);
#[cfg(target_lang = "pwsh")]
#[command = "Invoke-Expression"]
extern "pwsh" fn invoke_expression(cmd: str);

#[cfg(target_lang = "bash")]
#[command = "ssh"]
extern "bash" fn ssh(host: str, cmd: str);
#[cfg(target_lang = "pwsh")]
#[command = "ssh"]
extern "pwsh" fn ssh(host: str, cmd: str);

#[cfg(target_lang = "bash")]
#[command = "docker exec"]
extern "bash" fn docker_exec(host: str, cmd: str);
#[cfg(target_lang = "pwsh")]
#[command = "docker exec"]
extern "pwsh" fn docker_exec(host: str, cmd: str);

#[cfg(target_lang = "bash")]
#[command = "kubectl exec"]
extern "bash" fn kubectl_exec(host: str, cmd: str);
#[cfg(target_lang = "pwsh")]
#[command = "kubectl exec"]
extern "pwsh" fn kubectl_exec(host: str, cmd: str);

#[cfg(target_lang = "bash")]
#[command = "pwsh"]
extern "bash" fn winrm_run(host: str, cmd: str);
#[cfg(target_lang = "pwsh")]
#[command = "New-PSSession Invoke-Command Remove-PSSession"]
extern "pwsh" fn winrm_run(host: str, cmd: str);

#[cfg(target_lang = "bash")]
#[command = "cp"]
extern "bash" fn cp(src: str, dest: str);
#[cfg(target_lang = "pwsh")]
#[command = "Copy-Item"]
extern "pwsh" fn copy_item(src: str, dest: str);

#[cfg(target_lang = "bash")]
#[command = "scp"]
extern "bash" fn scp(host: str, src: str, dest: str);
#[cfg(target_lang = "pwsh")]
#[command = "scp"]
extern "pwsh" fn scp(host: str, src: str, dest: str);

#[cfg(target_lang = "bash")]
#[command = "docker cp"]
extern "bash" fn docker_cp(host: str, src: str, dest: str);
#[cfg(target_lang = "pwsh")]
#[command = "docker cp"]
extern "pwsh" fn docker_cp(host: str, src: str, dest: str);

#[cfg(target_lang = "bash")]
#[command = "kubectl cp"]
extern "bash" fn kubectl_cp(host: str, src: str, dest: str);
#[cfg(target_lang = "pwsh")]
#[command = "kubectl cp"]
extern "pwsh" fn kubectl_cp(host: str, src: str, dest: str);

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
#[command = "rm"]
extern "bash" fn remove_file(path: str);
#[cfg(target_lang = "pwsh")]
#[command = "Remove-Item"]
extern "pwsh" fn remove_file(path: str);

#[cfg(target_lang = "bash")]
#[command = "rsync ssh"]
extern "bash" fn rsync_ssh(host: str, flags: str, src: str, dest: str);
#[cfg(target_lang = "pwsh")]
#[command = "rsync"]
extern "pwsh" fn rsync_ssh(host: str, flags: str, src: str, dest: str);

#[cfg(target_lang = "bash")]
extern "bash" fn runtime_fail(message: str);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn runtime_fail(message: str);

#[cfg(target_lang = "bash")]
extern "bash" fn runtime_set_changed(changed: bool);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn runtime_set_changed(changed: bool);

const fn host_transport(host: str) -> str {
    runtime_host_transport(host)
}

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
    let transport = host_transport(host);
    match transport {
        "local" => run_local_host(cmd),
        "ssh" => ssh(host, cmd),
        "docker" => docker_exec(host, cmd),
        "kubectl" => kubectl_exec(host, cmd),
        "winrm" => winrm_run(host, cmd),
        _ => runtime_fail(f"unsupported transport: {transport}"),
    }
}

const fn copy_host(host: str, src: str, dest: str) {
    let transport = host_transport(host);
    match transport {
        "local" => copy_local_host(src, dest),
        "ssh" => scp(host, src, dest),
        "docker" => docker_cp(host, src, dest),
        "kubectl" => kubectl_cp(host, src, dest),
        "winrm" => winrm_copy(host, src, dest),
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
    let transport = host_transport(host);
    match transport {
        "ssh" => rsync_ssh(host, flags, src, dest),
        _ => runtime_fail(f"rsync is only supported for ssh in shell target, got: {transport}"),
    }
}

const fn shell_run(host: str, command: str, only_if: str, unless: str, creates: str, removes: str) {
    runtime_set_changed(false);
    if only_if != "" {
        if std::server::shell(only_if) {
            shell_run_after_only_if(host, command, unless, creates, removes);
        }
    } else {
        shell_run_after_only_if(host, command, unless, creates, removes);
    }
}

const fn shell_run_after_only_if(host: str, command: str, unless: str, creates: str, removes: str) {
    if unless != "" {
        if !std::server::shell(unless) {
            shell_run_after_unless(host, command, creates, removes);
        }
    } else {
        shell_run_after_unless(host, command, creates, removes);
    }
}

const fn shell_run_after_unless(host: str, command: str, creates: str, removes: str) {
    if creates != "" {
        if std::server::shell(f"test ! -e {creates}") {
            shell_run_after_creates(host, command, removes);
        }
    } else {
        shell_run_after_creates(host, command, removes);
    }
}

const fn shell_run_after_creates(host: str, command: str, removes: str) {
    if removes != "" {
        if std::server::shell(f"test -e {removes}") {
            run_host(host, command);
            runtime_set_changed(true);
        }
    } else {
        run_host(host, command);
        runtime_set_changed(true);
    }
}

const fn shell_copy(host: str, src: str, dest: str, only_if: str, unless: str, creates: str, removes: str) {
    runtime_set_changed(false);
    if only_if != "" {
        if std::server::shell(only_if) {
            shell_copy_after_only_if(host, src, dest, unless, creates, removes);
        }
    } else {
        shell_copy_after_only_if(host, src, dest, unless, creates, removes);
    }
}

const fn shell_copy_after_only_if(host: str, src: str, dest: str, unless: str, creates: str, removes: str) {
    if unless != "" {
        if !std::server::shell(unless) {
            shell_copy_after_unless(host, src, dest, creates, removes);
        }
    } else {
        shell_copy_after_unless(host, src, dest, creates, removes);
    }
}

const fn shell_copy_after_unless(host: str, src: str, dest: str, creates: str, removes: str) {
    if creates != "" {
        if std::server::shell(f"test ! -e {creates}") {
            shell_copy_after_creates(host, src, dest, removes);
        }
    } else {
        shell_copy_after_creates(host, src, dest, removes);
    }
}

const fn shell_copy_after_creates(host: str, src: str, dest: str, removes: str) {
    if removes != "" {
        if std::server::shell(f"test -e {removes}") {
            copy_host(host, src, dest);
            runtime_set_changed(true);
        }
    } else {
        copy_host(host, src, dest);
        runtime_set_changed(true);
    }
}

const fn shell_template(host: str, src: str, dest: str, vars: str, only_if: str, unless: str, creates: str, removes: str) {
    runtime_set_changed(false);
    if only_if != "" {
        if std::server::shell(only_if) {
            shell_template_after_only_if(host, src, dest, vars, unless, creates, removes);
        }
    } else {
        shell_template_after_only_if(host, src, dest, vars, unless, creates, removes);
    }
}

const fn shell_template_after_only_if(host: str, src: str, dest: str, vars: str, unless: str, creates: str, removes: str) {
    if unless != "" {
        if !std::server::shell(unless) {
            shell_template_after_unless(host, src, dest, vars, creates, removes);
        }
    } else {
        shell_template_after_unless(host, src, dest, vars, creates, removes);
    }
}

const fn shell_template_after_unless(host: str, src: str, dest: str, vars: str, creates: str, removes: str) {
    if creates != "" {
        if std::server::shell(f"test ! -e {creates}") {
            shell_template_after_creates(host, src, dest, vars, removes);
        }
    } else {
        shell_template_after_creates(host, src, dest, vars, removes);
    }
}

const fn shell_template_after_creates(host: str, src: str, dest: str, vars: str, removes: str) {
    if removes != "" {
        if std::server::shell(f"test -e {removes}") {
            template_host(host, src, dest, vars);
            runtime_set_changed(true);
        }
    } else {
        template_host(host, src, dest, vars);
        runtime_set_changed(true);
    }
}

const fn shell_rsync(host: str, flags: str, src: str, dest: str, only_if: str, unless: str, creates: str, removes: str) {
    runtime_set_changed(false);
    if only_if != "" {
        if std::server::shell(only_if) {
            shell_rsync_after_only_if(host, flags, src, dest, unless, creates, removes);
        }
    } else {
        shell_rsync_after_only_if(host, flags, src, dest, unless, creates, removes);
    }
}

const fn shell_rsync_after_only_if(host: str, flags: str, src: str, dest: str, unless: str, creates: str, removes: str) {
    if unless != "" {
        if !std::server::shell(unless) {
            shell_rsync_after_unless(host, flags, src, dest, creates, removes);
        }
    } else {
        shell_rsync_after_unless(host, flags, src, dest, creates, removes);
    }
}

const fn shell_rsync_after_unless(host: str, flags: str, src: str, dest: str, creates: str, removes: str) {
    if creates != "" {
        if std::server::shell(f"test ! -e {creates}") {
            shell_rsync_after_creates(host, flags, src, dest, removes);
        }
    } else {
        shell_rsync_after_creates(host, flags, src, dest, removes);
    }
}

const fn shell_rsync_after_creates(host: str, flags: str, src: str, dest: str, removes: str) {
    if removes != "" {
        if std::server::shell(f"test -e {removes}") {
            rsync_host(host, flags, src, dest);
            runtime_set_changed(true);
        }
    } else {
        rsync_host(host, flags, src, dest);
        runtime_set_changed(true);
    }
}
