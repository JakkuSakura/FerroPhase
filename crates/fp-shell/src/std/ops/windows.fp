#[cfg(target_lang = "pwsh")]
pub const fn service(
    service: str,
    context hosts: str = "localhost",
    running: bool = true,
    restart: bool = false,
    suspend: bool = false,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    let command = match (running, restart, suspend) {
        (_, _, true) => f"Suspend-Service -Name {service}",
        (false, _, false) => f"Stop-Service -Name {service}",
        (true, true, false) => f"Restart-Service -Name {service}",
        (true, false, false) => f"Start-Service -Name {service}",
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

#[cfg(target_lang = "pwsh")]
pub const fn reboot(
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    std::ops::server::shell(
        "Restart-Computer -Force",
        hosts=hosts,
        sudo=false,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    )
}
