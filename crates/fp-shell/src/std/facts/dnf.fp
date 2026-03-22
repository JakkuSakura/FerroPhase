pub const fn repositories() -> str {
    std::shell::process::output(
        "cat /etc/dnf.conf /etc/dnf.repos.d/*.repo /etc/yum.repos.d/*.repo 2>/dev/null"
    )
}
