pub const fn packages(
    packages: str,
    context hosts: str = "localhost",
    present: bool = true,
    latest: bool = false,
    sudo: bool = false,
) -> bool {
    if present {
        if latest {
            std::ops::server::shell(f"choco update -y {packages}", hosts=hosts, sudo=sudo)
        } else {
            std::ops::server::shell(f"choco install -y {packages}", hosts=hosts, sudo=sudo)
        }
    } else {
        std::ops::server::shell(f"choco uninstall -y -x {packages}", hosts=hosts, sudo=sudo)
    }
}

pub const fn install(context hosts: str = "localhost", sudo: bool = false) -> bool {
    std::ops::server::shell(
        "Set-ExecutionPolicy Bypass -Scope Process -Force ; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))",
        hosts=hosts,
        sudo=sudo,
    )
}
