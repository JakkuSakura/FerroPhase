pub const fn config() -> str {
    std::shell::process::output("cat /etc/opkg.conf")
}

pub const fn feeds() -> str {
    std::shell::process::output(
        "cat /etc/opkg/distfeeds.conf 2>/dev/null; printf '%s\\n' 'CUSTOM'; cat /etc/opkg/customfeeds.conf 2>/dev/null"
    )
}

pub const fn architectures() -> str {
    std::shell::process::output("/bin/opkg print-architecture")
}

pub const fn packages() -> str {
    std::shell::process::output("/bin/opkg list-installed")
}

pub const fn upgradeable_packages() -> str {
    std::shell::process::output("/bin/opkg list-upgradable || true")
}
