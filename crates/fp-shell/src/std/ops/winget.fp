#[cfg(target_lang = "pwsh")]
pub const fn packages(
    packages: str,
    context hosts: str = "localhost",
    present: bool = true,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    if packages == "" {
        true
    } else {
        let command = match present {
            true => f"winget install --no-upgrade --silent --exact {packages}",
            false => f"winget uninstall --silent --exact {packages}",
        };
        std::ops::server::shell(
            command,
            hosts=hosts,
            sudo=false,
            only_if=only_if,
            unless=unless,
            creates=creates,
            removes=removes,
        )
    }
}
