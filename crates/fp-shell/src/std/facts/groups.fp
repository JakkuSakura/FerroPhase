#[cfg(target_lang = "bash")]
pub const fn list() -> str {
    std::shell::process::output(
        "getent group 2>/dev/null | cut -d: -f1 || cat /etc/group | cut -d: -f1",
    )
}

#[cfg(target_lang = "pwsh")]
pub const fn list() -> str {
    std::shell::process::output("Get-LocalGroup | Select -ExpandProperty Name")
}

#[cfg(target_lang = "bash")]
pub const fn exists(name: str) -> bool {
    std::shell::process::ok(f"getent group {name} >/dev/null 2>&1 || grep -q '^{name}:' /etc/group")
}

#[cfg(target_lang = "pwsh")]
pub const fn exists(name: str) -> bool {
    std::shell::process::ok(f"Get-LocalGroup -Name \"{name}\"")
}

#[cfg(target_lang = "bash")]
pub const fn info(name: str) -> str {
    std::shell::process::output(
        f"getent group {name} 2>/dev/null || grep '^{name}:' /etc/group",
    )
}

#[cfg(target_lang = "pwsh")]
pub const fn info(name: str) -> str {
    std::shell::process::output(f"Get-LocalGroup -Name \"{name}\" | Format-List -Property *")
}
