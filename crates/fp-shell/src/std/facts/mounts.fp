#[cfg(target_lang = "bash")]
pub const fn list() -> str {
    std::shell::process::output(
        "findmnt -rn -o TARGET,SOURCE,FSTYPE,OPTIONS 2>/dev/null || mount",
    )
}

#[cfg(target_lang = "pwsh")]
pub const fn list() -> str {
    std::shell::process::output(
        "Get-PSDrive -PSProvider FileSystem | Format-Table -AutoSize Name,Root,Used,Free",
    )
}

#[cfg(target_lang = "bash")]
pub const fn find(path: str) -> str {
    std::shell::process::output(f"findmnt -rn -o TARGET,SOURCE,FSTYPE,OPTIONS --target {path} 2>/dev/null || mount | grep ' on {path} '")
}

#[cfg(target_lang = "pwsh")]
pub const fn find(path: str) -> str {
    std::shell::process::output(
        f"Get-PSDrive -PSProvider FileSystem | Where-Object {{$_.Root -eq \"{path}\"}} | Format-List -Property *",
    )
}
