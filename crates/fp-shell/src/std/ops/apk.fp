pub const fn update(context hosts: str = "localhost", sudo: bool = true) -> bool {
    std::ops::server::shell("apk update", hosts=hosts, sudo=sudo)
}

pub const fn upgrade(context hosts: str = "localhost", available: bool = false, sudo: bool = true) -> bool {
    if available {
        std::ops::server::shell("apk upgrade --available", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell("apk upgrade", hosts=hosts, sudo=sudo)
    }
}

pub const fn packages(
    packages: str,
    context hosts: str = "localhost",
    present: bool = true,
    latest: bool = false,
    update: bool = false,
    upgrade: bool = false,
    sudo: bool = true,
) -> bool {
    if update {
        std::ops::apk::update(hosts=hosts, sudo=sudo);
    }
    if upgrade {
        std::ops::apk::upgrade(hosts=hosts, sudo=sudo);
    }
    if present {
        if latest {
            std::ops::server::shell(f"apk upgrade {packages}", hosts=hosts, sudo=sudo)
        } else {
            std::ops::server::shell(f"apk add {packages}", hosts=hosts, sudo=sudo)
        }
    } else {
        std::ops::server::shell(f"apk del {packages}", hosts=hosts, sudo=sudo)
    }
}
