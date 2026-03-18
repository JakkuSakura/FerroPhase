const fn run_host(host: str, cmd: str) {
    let transport = std::shell::host_transport(host);
    match transport {
        "local" => std::shell::backend_run_local(cmd),
        "ssh" => std::shell::backend_run_ssh(host, cmd),
        "docker" => std::shell::backend_run_docker(host, cmd),
        "kubectl" => std::shell::backend_run_kubectl(host, cmd),
        "winrm" => std::shell::backend_run_winrm(host, cmd),
        _ => std::shell::backend_unsupported_transport(transport),
    }
}

const fn copy_host(host: str, src: str, dest: str) {
    let transport = std::shell::host_transport(host);
    match transport {
        "local" => std::shell::backend_copy_local(src, dest),
        "ssh" => std::shell::backend_copy_ssh(host, src, dest),
        "docker" => std::shell::backend_copy_docker(host, src, dest),
        "kubectl" => std::shell::backend_copy_kubectl(host, src, dest),
        "winrm" => std::shell::backend_copy_winrm(host, src, dest),
        _ => std::shell::backend_unsupported_copy_transport(transport),
    }
}

const fn template_host(host: str, src: str, dest: str, vars: str) {
    let tmp = std::shell::backend_temp_path();
    std::shell::backend_render_template(src, tmp, vars);
    copy_host(host, tmp, dest);
    std::shell::backend_remove_file(tmp);
}

const fn rsync_host(host: str, flags: str, src: str, dest: str) {
    let transport = std::shell::host_transport(host);
    match transport {
        "ssh" => std::shell::backend_rsync_ssh(host, flags, src, dest),
        _ => std::shell::backend_unsupported_rsync_transport(transport),
    }
}
