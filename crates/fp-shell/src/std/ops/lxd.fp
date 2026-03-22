pub const fn container(id: str, context hosts: str = "localhost", present: bool = true, image: str, sudo: bool) -> bool {
    if present {
        std::ops::server::shell(f"lxc launch {image} {id} < /dev/null", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"lxc stop {id} || true; lxc delete {id}", hosts=hosts, sudo=sudo)
    }
}
