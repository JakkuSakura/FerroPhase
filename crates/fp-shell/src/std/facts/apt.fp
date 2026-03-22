pub const fn sources() -> str {
    std::shell::process::output(
        "cat /etc/apt/sources.list /etc/apt/sources.list.d/*.list 2>/dev/null"
    )
}

pub const fn keys() -> str {
    std::shell::process::output("apt-key list --with-colons 2>/dev/null || true")
}

pub const fn simulate(command: str) -> str {
    std::shell::process::output(
        f"LC_ALL=C DEBIAN_FRONTEND=noninteractive apt-get -y -o Dpkg::Options::='--force-confdef' -o Dpkg::Options::='--force-confold' {command} --dry-run"
    )
}
