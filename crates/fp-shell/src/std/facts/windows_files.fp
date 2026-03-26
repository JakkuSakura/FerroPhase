#[cfg(target_lang = "pwsh")]
pub const fn file(path: str) -> str {
    std::shell::process::output(
        f"if (Test-Path -PathType Leaf -LiteralPath \"{path}\") {{ Get-ItemProperty -Path \"{path}\" }}",
    )
}

#[cfg(target_lang = "pwsh")]
pub const fn directory(path: str) -> str {
    std::shell::process::output(
        f"if (Test-Path -PathType Container -LiteralPath \"{path}\") {{ Get-ItemProperty -Path \"{path}\" }}",
    )
}

#[cfg(target_lang = "pwsh")]
pub const fn link(path: str) -> str {
    std::shell::process::output(f"(Get-Item -LiteralPath \"{path}\").LinkType")
}

#[cfg(target_lang = "pwsh")]
pub const fn temp_dir() -> str {
    std::shell::process::output("[System.IO.Path]::GetTempPath()")
}
