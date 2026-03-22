pub const fn boolean(boolean: str) -> str {
    std::shell::process::output(f"getsebool {boolean}")
}

pub const fn file_context(path: str) -> str {
    std::shell::process::output(f"stat -c %C {path} || exit 0")
}

pub const fn file_context_mapping(target: str) -> str {
    std::shell::process::output(
        f"set -o pipefail && semanage fcontext -n -l | (grep '^{target}' || true)"
    )
}

pub const fn ports() -> str {
    std::shell::process::output("semanage port -ln")
}

pub const fn port(protocol: str, port: str) -> str {
    std::shell::process::output(
        f"(sepolicy network -p {port} 2>/dev/null || true) | grep {protocol}"
    )
}
