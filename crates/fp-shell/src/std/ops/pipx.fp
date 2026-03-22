pub const fn packages(
    packages: str,
    context hosts: str = "localhost",
    present: bool = true,
    latest: bool,
    extra_args: str,
    sudo: bool,
) -> bool {
    if present {
        if latest {
            if extra_args == "" {
                std::ops::server::shell(f"pipx upgrade {packages}", hosts=hosts, sudo=sudo)
            } else {
                std::ops::server::shell(
                    f"pipx upgrade {extra_args} {packages}",
                    hosts=hosts,
                    sudo=sudo,
                )
            }
        } else {
            if extra_args == "" {
                std::ops::server::shell(f"pipx install {packages}", hosts=hosts, sudo=sudo)
            } else {
                std::ops::server::shell(
                    f"pipx install {extra_args} {packages}",
                    hosts=hosts,
                    sudo=sudo,
                )
            }
        }
    } else {
        std::ops::server::shell(f"pipx uninstall {packages}", hosts=hosts, sudo=sudo)
    }
}

pub const fn upgrade_all(context hosts: str = "localhost", sudo: bool) -> bool {
    std::ops::server::shell("pipx upgrade-all", hosts=hosts, sudo=sudo)
}

pub const fn ensure_path(context hosts: str = "localhost", sudo: bool) -> bool {
    std::ops::server::shell("pipx ensurepath", hosts=hosts, sudo=sudo)
}
