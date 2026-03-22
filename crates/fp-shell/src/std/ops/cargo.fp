pub const fn packages(
    packages: str,
    context hosts: str = "localhost",
    present: bool = true,
    latest: bool = false,
    sudo: bool = false,
) -> bool {
    if present {
        if latest {
            std::ops::server::shell(f"cargo install {packages}", hosts=hosts, sudo=sudo)
        } else {
            std::ops::server::shell(f"cargo install {packages}", hosts=hosts, sudo=sudo)
        }
    } else {
        std::ops::server::shell(f"cargo uninstall {packages}", hosts=hosts, sudo=sudo)
    }
}
