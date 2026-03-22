pub const fn containers() -> str {
    std::shell::process::output("lxc list --format json --fast")
}
