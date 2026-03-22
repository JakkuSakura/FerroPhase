pub const fn system_info() -> str {
    std::shell::process::output("podman system info --format=json")
}

pub const fn containers() -> str {
    std::shell::process::output("podman ps --format=json --all")
}
