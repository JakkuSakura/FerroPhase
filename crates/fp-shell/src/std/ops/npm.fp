pub const fn packages(
    packages: str,
    context hosts: str = "localhost",
    present: bool = true,
    latest: bool = false,
    directory: str = "",
    sudo: bool = false,
) -> bool {
    if directory == "" {
        if present {
            if latest {
                std::ops::server::shell(f"npm update -g {packages}", hosts=hosts, sudo=sudo)
            } else {
                std::ops::server::shell(f"npm install -g {packages}", hosts=hosts, sudo=sudo)
            }
        } else {
            std::ops::server::shell(f"npm uninstall -g {packages}", hosts=hosts, sudo=sudo)
        }
    } else {
        if present {
            if latest {
                std::ops::server::shell(
                    f"cd {directory} && npm update {packages}",
                    hosts=hosts,
                    sudo=sudo,
                )
            } else {
                std::ops::server::shell(
                    f"cd {directory} && npm install {packages}",
                    hosts=hosts,
                    sudo=sudo,
                )
            }
        } else {
            std::ops::server::shell(
                f"cd {directory} && npm uninstall {packages}",
                hosts=hosts,
                sudo=sudo,
            )
        }
    }
}
