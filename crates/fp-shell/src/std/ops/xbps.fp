pub const fn upgrade(context hosts: str = "localhost", sudo: bool = true) -> bool {
    std::ops::server::shell("xbps-install -y -u", hosts=hosts, sudo=sudo)
}

pub const fn update(context hosts: str = "localhost", sudo: bool = true) -> bool {
    std::ops::server::shell("xbps-install -S", hosts=hosts, sudo=sudo)
}

pub const fn packages(packages: str, context hosts: str = "localhost", present: bool = true, update: bool, upgrade: bool, sudo: bool = true) -> bool {
    if update {
        std::ops::xbps::update(hosts=hosts, sudo=sudo);
    }
    if upgrade {
        std::ops::xbps::upgrade(hosts=hosts, sudo=sudo);
    }
    if present {
        std::ops::server::shell(f"xbps-install -y -u {packages}", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"xbps-remove -y {packages}", hosts=hosts, sudo=sudo)
    }
}
