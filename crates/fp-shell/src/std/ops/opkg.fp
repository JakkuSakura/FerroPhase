pub const fn update(context hosts: str = "localhost", sudo: bool = true) -> bool {
    std::ops::server::shell("opkg update", hosts=hosts, sudo=sudo)
}

pub const fn packages(
    packages: str,
    context hosts: str = "localhost",
    present: bool = true,
    latest: bool,
    update: bool = true,
    sudo: bool = true,
) -> bool {
    if update {
        std::ops::opkg::update(hosts=hosts, sudo=sudo);
    }
    if present {
        if latest {
            std::ops::server::shell(f"opkg upgrade {packages}", hosts=hosts, sudo=sudo)
        } else {
            std::ops::server::shell(f"opkg install {packages}", hosts=hosts, sudo=sudo)
        }
    } else {
        std::ops::server::shell(f"opkg remove {packages}", hosts=hosts, sudo=sudo)
    }
}
