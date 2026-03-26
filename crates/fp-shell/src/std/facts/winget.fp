#[cfg(target_lang = "pwsh")]
pub const fn packages() -> str {
    std::shell::process::output("Get-WingetPackage | Select -Property Id, InstalledVersion")
}
