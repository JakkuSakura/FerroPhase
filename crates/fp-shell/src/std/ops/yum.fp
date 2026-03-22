pub const fn update(context hosts: str = "localhost", sudo: bool = true) -> bool {
    std::ops::server::shell("yum update -y", hosts=hosts, sudo=sudo)
}

pub const fn key(src: str, context hosts: str = "localhost", sudo: bool = true) -> bool {
    std::ops::server::shell(f"rpm --import {src}", hosts=hosts, sudo=sudo)
}

pub const fn packages(
    packages: str,
    context hosts: str = "localhost",
    present: bool = true,
    latest: bool = false,
    update: bool = false,
    clean: bool = false,
    sudo: bool = true,
) -> bool {
    if clean {
        std::ops::server::shell("yum clean all", hosts=hosts, sudo=sudo);
    }
    if update {
        std::ops::yum::update(hosts=hosts, sudo=sudo);
    }
    if present {
        if latest {
            std::ops::server::shell(f"yum update -y {packages}", hosts=hosts, sudo=sudo)
        } else {
            std::ops::server::shell(f"yum install -y {packages}", hosts=hosts, sudo=sudo)
        }
    } else {
        std::ops::server::shell(f"yum remove -y {packages}", hosts=hosts, sudo=sudo)
    }
}
