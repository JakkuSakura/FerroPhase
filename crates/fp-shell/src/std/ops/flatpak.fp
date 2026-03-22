pub const fn packages(
    packages: str,
    context hosts: str = "localhost",
    remote: str,
    present: bool = true,
    sudo: bool,
) -> bool {
    if present {
        if remote == "" {
            std::ops::server::shell(
                f"flatpak install --noninteractive {packages}",
                hosts=hosts,
                sudo=sudo,
            )
        } else {
            std::ops::server::shell(
                f"flatpak install --noninteractive {remote} {packages}",
                hosts=hosts,
                sudo=sudo,
            )
        }
    } else {
        std::ops::server::shell(
            f"flatpak uninstall --noninteractive {packages}",
            hosts=hosts,
            sudo=sudo,
        )
    }
}
