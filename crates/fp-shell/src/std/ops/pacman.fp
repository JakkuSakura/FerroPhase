pub const fn update(context hosts: str = "localhost", sudo: bool = true) -> bool {
    std::ops::server::shell("pacman -Sy", hosts=hosts, sudo=sudo)
}

pub const fn upgrade(context hosts: str = "localhost", sudo: bool = true) -> bool {
    std::ops::server::shell("pacman --noconfirm -Su", hosts=hosts, sudo=sudo)
}

pub const fn packages(
    packages: str,
    context hosts: str = "localhost",
    present: bool = true,
    update: bool = false,
    upgrade: bool = false,
    sudo: bool = true,
) -> bool {
    if update {
        std::ops::pacman::update(hosts=hosts, sudo=sudo);
    }
    if upgrade {
        std::ops::pacman::upgrade(hosts=hosts, sudo=sudo);
    }
    if present {
        std::ops::server::shell(
            f"pacman --noconfirm -S {packages}",
            hosts=hosts,
            sudo=sudo,
        )
    } else {
        std::ops::server::shell(
            f"pacman --noconfirm -R {packages}",
            hosts=hosts,
            sudo=sudo,
        )
    }
}
