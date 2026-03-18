#[cfg(target_lang = "bash")]
extern "bash" fn shell_host_transport(host: str) -> str;
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn shell_host_transport(host: str) -> str;

#[cfg(target_lang = "bash")]
extern "bash" fn shell_run_local(cmd: str);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn shell_run_local(cmd: str);

#[cfg(target_lang = "bash")]
extern "bash" fn shell_run_ssh(host: str, cmd: str);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn shell_run_ssh(host: str, cmd: str);

#[cfg(target_lang = "bash")]
extern "bash" fn shell_run_docker(host: str, cmd: str);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn shell_run_docker(host: str, cmd: str);

#[cfg(target_lang = "bash")]
extern "bash" fn shell_run_kubectl(host: str, cmd: str);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn shell_run_kubectl(host: str, cmd: str);

#[cfg(target_lang = "bash")]
extern "bash" fn shell_run_winrm(host: str, cmd: str);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn shell_run_winrm(host: str, cmd: str);

#[cfg(target_lang = "bash")]
extern "bash" fn shell_copy_local(src: str, dest: str);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn shell_copy_local(src: str, dest: str);

#[cfg(target_lang = "bash")]
extern "bash" fn shell_copy_ssh(host: str, src: str, dest: str);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn shell_copy_ssh(host: str, src: str, dest: str);

#[cfg(target_lang = "bash")]
extern "bash" fn shell_copy_docker(host: str, src: str, dest: str);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn shell_copy_docker(host: str, src: str, dest: str);

#[cfg(target_lang = "bash")]
extern "bash" fn shell_copy_kubectl(host: str, src: str, dest: str);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn shell_copy_kubectl(host: str, src: str, dest: str);

#[cfg(target_lang = "bash")]
extern "bash" fn shell_copy_winrm(host: str, src: str, dest: str);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn shell_copy_winrm(host: str, src: str, dest: str);

#[cfg(target_lang = "bash")]
extern "bash" fn shell_temp_path() -> str;
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn shell_temp_path() -> str;

#[cfg(target_lang = "bash")]
extern "bash" fn shell_render_template(src: str, dest: str, vars: str);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn shell_render_template(src: str, dest: str, vars: str);

#[cfg(target_lang = "bash")]
extern "bash" fn shell_remove_file(path: str);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn shell_remove_file(path: str);

#[cfg(target_lang = "bash")]
extern "bash" fn shell_rsync_ssh(host: str, flags: str, src: str, dest: str);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn shell_rsync_ssh(host: str, flags: str, src: str, dest: str);

#[cfg(target_lang = "bash")]
extern "bash" fn shell_fail(message: str);
#[cfg(target_lang = "pwsh")]
extern "pwsh" fn shell_fail(message: str);

const fn host_transport(host: str) -> str {
    shell_host_transport(host)
}
