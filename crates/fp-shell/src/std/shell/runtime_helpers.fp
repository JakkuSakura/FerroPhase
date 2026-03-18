const fn run_host(host: str, cmd: str) {
    let transport = host_transport(host);
    match transport {
        "local" => shell_run_local(cmd),
        "ssh" => shell_run_ssh(host, cmd),
        "docker" => shell_run_docker(host, cmd),
        "kubectl" => shell_run_kubectl(host, cmd),
        "winrm" => shell_run_winrm(host, cmd),
        _ => shell_fail(f"unsupported transport: {transport}"),
    }
}

const fn copy_host(host: str, src: str, dest: str) {
    let transport = host_transport(host);
    match transport {
        "local" => shell_copy_local(src, dest),
        "ssh" => shell_copy_ssh(host, src, dest),
        "docker" => shell_copy_docker(host, src, dest),
        "kubectl" => shell_copy_kubectl(host, src, dest),
        "winrm" => shell_copy_winrm(host, src, dest),
        _ => shell_fail(f"unsupported transport for copy: {transport}"),
    }
}

const fn template_host(host: str, src: str, dest: str, vars: str) {
    let tmp = shell_temp_path();
    shell_render_template(src, tmp, vars);
    copy_host(host, tmp, dest);
    shell_remove_file(tmp);
}

const fn rsync_host(host: str, flags: str, src: str, dest: str) {
    let transport = host_transport(host);
    match transport {
        "ssh" => shell_rsync_ssh(host, flags, src, dest),
        _ => shell_fail(f"rsync is only supported for ssh in shell target, got: {transport}"),
    }
}
