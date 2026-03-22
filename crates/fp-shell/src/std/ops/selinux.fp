pub const fn boolean(bool_name: str, value: str, context hosts: str = "localhost", persistent: bool, sudo: bool) -> bool {
    if persistent {
        std::ops::server::shell(f"setsebool -P {bool_name} {value}", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"setsebool {bool_name} {value}", hosts=hosts, sudo=sudo)
    }
}

pub const fn file_context(path: str, se_type: str, context hosts: str = "localhost", sudo: bool) -> bool {
    std::ops::server::shell(f"chcon -t {se_type} {path}", hosts=hosts, sudo=sudo)
}

pub const fn file_context_mapping(target: str, se_type: str, context hosts: str = "localhost", present: bool = true, sudo: bool) -> bool {
    if present {
        std::ops::server::shell(f"semanage fcontext -a -t {se_type} {target}", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"semanage fcontext -d {target}", hosts=hosts, sudo=sudo)
    }
}

pub const fn port(protocol: str, port_num: str, se_type: str, context hosts: str = "localhost", present: bool = true, sudo: bool) -> bool {
    if present {
        std::ops::server::shell(f"semanage port -a -t {se_type} -p {protocol} {port_num}", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"semanage port -d -p {protocol} {port_num}", hosts=hosts, sudo=sudo)
    }
}
