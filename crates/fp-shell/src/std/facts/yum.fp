pub const fn repositories() -> str {
    std::shell::process::output("cat /etc/yum.conf /etc/yum.repos.d/*.repo 2>/dev/null")
}
