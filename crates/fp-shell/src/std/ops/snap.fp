pub const fn packages(
    packages: str,
    context hosts: str = "localhost",
    channel: str = "latest/stable",
    classic: bool,
    present: bool = true,
    latest: bool,
    sudo: bool,
) -> bool {
    if present {
        if latest {
            std::ops::server::shell(
                f"snap refresh {packages} --channel={channel}",
                hosts=hosts,
                sudo=sudo,
            )
        } else {
            if classic {
                std::ops::server::shell(
                    f"snap install --classic {packages} --channel={channel}",
                    hosts=hosts,
                    sudo=sudo,
                )
            } else {
                std::ops::server::shell(
                    f"snap install {packages} --channel={channel}",
                    hosts=hosts,
                    sudo=sudo,
                )
            }
        }
    } else {
        std::ops::server::shell(f"snap remove {packages}", hosts=hosts, sudo=sudo)
    }
}
