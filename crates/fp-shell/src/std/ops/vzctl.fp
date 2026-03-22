pub const fn start(ctid: str, context hosts: str = "localhost", force: bool, sudo: bool) -> bool {
    if force {
        std::ops::server::shell(f"vzctl start {ctid} --force", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"vzctl start {ctid}", hosts=hosts, sudo=sudo)
    }
}

pub const fn stop(ctid: str, context hosts: str = "localhost", sudo: bool) -> bool {
    std::ops::server::shell(f"vzctl stop {ctid}", hosts=hosts, sudo=sudo)
}

pub const fn restart(ctid: str, context hosts: str = "localhost", force: bool, sudo: bool) -> bool {
    std::ops::vzctl::stop(ctid, hosts=hosts, sudo=sudo);
    std::ops::vzctl::start(ctid, hosts=hosts, force=force, sudo=sudo);
    true
}

pub const fn mount(ctid: str, context hosts: str = "localhost", sudo: bool) -> bool {
    std::ops::server::shell(f"vzctl mount {ctid}", hosts=hosts, sudo=sudo)
}

pub const fn unmount(ctid: str, context hosts: str = "localhost", sudo: bool) -> bool {
    std::ops::server::shell(f"vzctl umount {ctid}", hosts=hosts, sudo=sudo)
}

pub const fn delete(ctid: str, context hosts: str = "localhost", sudo: bool) -> bool {
    std::ops::server::shell(f"vzctl delete {ctid}", hosts=hosts, sudo=sudo)
}

pub const fn create(ctid: str, context hosts: str = "localhost", template: str, sudo: bool) -> bool {
    if template == "" {
        std::ops::server::shell(f"vzctl create {ctid}", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"vzctl create {ctid} --ostemplate {template}", hosts=hosts, sudo=sudo)
    }
}

pub const fn set(ctid: str, context hosts: str = "localhost", save: bool, settings: str, sudo: bool) -> bool {
    if save {
        std::ops::server::shell(f"vzctl set {ctid} --save {settings}", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"vzctl set {ctid} {settings}", hosts=hosts, sudo=sudo)
    }
}
