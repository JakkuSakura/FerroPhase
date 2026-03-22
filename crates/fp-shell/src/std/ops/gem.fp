pub const fn packages(
    packages: str,
    context hosts: str = "localhost",
    present: bool = true,
    latest: bool,
    sudo: bool,
) -> bool {
    if present {
        if latest {
            std::ops::server::shell(f"gem update {packages}", hosts=hosts, sudo=sudo)
        } else {
            std::ops::server::shell(f"gem install {packages}", hosts=hosts, sudo=sudo)
        }
    } else {
        std::ops::server::shell(f"gem uninstall {packages}", hosts=hosts, sudo=sudo)
    }
}
