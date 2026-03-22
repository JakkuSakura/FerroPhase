pub const fn arch() -> str {
    std::shell::process::output("dpkg --print-architecture")
}

pub const fn packages() -> str {
    std::shell::process::output("dpkg -l")
}

pub const fn package(package: str) -> str {
    std::shell::process::output(
        f"! test -e {package} && (dpkg -s {package} 2>/dev/null || true) || dpkg -I {package}"
    )
}
