pub const fn update(context hosts: str = "localhost", sudo: bool = true) -> bool {
    std::ops::server::shell("pkgin -y update", hosts=hosts, sudo=sudo)
}

pub const fn upgrade(context hosts: str = "localhost", sudo: bool = true) -> bool {
    std::ops::server::shell("pkgin -y upgrade", hosts=hosts, sudo=sudo)
}

pub const fn packages(
    packages: str,
    context hosts: str = "localhost",
    present: bool = true,
    latest: bool,
    update: bool,
    upgrade: bool,
    sudo: bool = true,
) -> bool {
    if update {
        std::ops::pkgin::update(hosts=hosts, sudo=sudo);
    }
    if upgrade {
        std::ops::pkgin::upgrade(hosts=hosts, sudo=sudo);
    }
    if present {
        if latest {
            std::ops::server::shell(f"pkgin -y upgrade {packages}", hosts=hosts, sudo=sudo)
        } else {
            std::ops::server::shell(f"pkgin -y install {packages}", hosts=hosts, sudo=sudo)
        }
    } else {
        std::ops::server::shell(f"pkgin -y remove {packages}", hosts=hosts, sudo=sudo)
    }
}
