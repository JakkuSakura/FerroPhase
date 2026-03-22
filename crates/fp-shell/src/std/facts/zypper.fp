pub const fn repositories() -> str {
    std::shell::process::output("cat /etc/zypp/repos.d/*.repo 2>/dev/null")
}
