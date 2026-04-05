#[cfg(target_lang = "bash")]
pub const fn list() -> str {
    std::shell::process::output(
        "getent passwd 2>/dev/null | cut -d: -f1 || cat /etc/passwd | cut -d: -f1 || dscl . -list /Users",
    )
}

#[cfg(target_lang = "pwsh")]
pub const fn list() -> str {
    std::shell::process::output("Get-LocalUser | Select -ExpandProperty Name")
}

#[cfg(target_lang = "bash")]
pub const fn exists(name: str) -> bool {
    std::shell::process::ok(f"id -u {name} >/dev/null 2>&1")
}

#[cfg(target_lang = "pwsh")]
pub const fn exists(name: str) -> bool {
    std::shell::process::ok(f"Get-LocalUser -Name \"{name}\"")
}

#[cfg(target_lang = "bash")]
pub const fn info(name: str) -> str {
    std::shell::process::output(
        f"getent passwd {name} 2>/dev/null || cat /etc/passwd | grep '^{name}:' || dscl . -read /Users/{name}",
    )
}

#[cfg(target_lang = "pwsh")]
pub const fn info(name: str) -> str {
    std::shell::process::output(f"Get-LocalUser -Name \"{name}\" | Format-List -Property *")
}
