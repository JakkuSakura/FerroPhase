pub const fn shell(
    command: str,
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
    sudo: bool = false,
    cwd: str = "",
) -> bool {
    let command = command_with_options(command, cwd, sudo);
    shell_run(hosts, command, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn shell_local(
    command: str,
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
    sudo: bool = false,
    cwd: str = "",
) -> bool {
    let command = command_with_options(command, cwd, sudo);
    shell_run_local(hosts, command, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn shell_ssh(
    command: str,
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
    sudo: bool = false,
    cwd: str = "",
) -> bool {
    let command = command_with_options(command, cwd, sudo);
    shell_run_ssh(hosts, command, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn shell_docker(
    command: str,
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
    sudo: bool = false,
    cwd: str = "",
) -> bool {
    let command = command_with_options(command, cwd, sudo);
    shell_run_docker(hosts, command, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn shell_kubectl(
    command: str,
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
    sudo: bool = false,
    cwd: str = "",
) -> bool {
    let command = command_with_options(command, cwd, sudo);
    shell_run_kubectl(hosts, command, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn shell_winrm(
    command: str,
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
    sudo: bool = false,
    cwd: str = "",
) -> bool {
    let command = command_with_options(command, cwd, sudo);
    shell_run_winrm(hosts, command, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn shell_chroot(
    command: str,
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
    sudo: bool = false,
    cwd: str = "",
) -> bool {
    let command = command_with_options(command, cwd, sudo);
    shell_run_chroot(hosts, command, only_if, unless, creates, removes);
    runtime_last_changed()
}
