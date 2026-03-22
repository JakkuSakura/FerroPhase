pub const fn service(service: str, context hosts: str = "localhost", running: bool = true, restarted: bool, reloaded: bool, command: str, enabled: bool, sudo: bool) -> bool {
    if command != "" {
        std::ops::server::shell(f"initctl {command} {service}", hosts=hosts, sudo=sudo);
    } else {
        if restarted {
            std::ops::server::shell(f"initctl restart {service}", hosts=hosts, sudo=sudo);
        } else {
            if reloaded {
                std::ops::server::shell(f"initctl reload {service}", hosts=hosts, sudo=sudo);
            } else {
                if running {
                    std::ops::server::shell(f"initctl start {service}", hosts=hosts, sudo=sudo);
                } else {
                    std::ops::server::shell(f"initctl stop {service}", hosts=hosts, sudo=sudo);
                }
            }
        }
    }
    if enabled {
        std::ops::server::shell(f"rm -f /etc/init/{service}.override", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"printf '%s\\n' manual > /etc/init/{service}.override", hosts=hosts, sudo=sudo)
    }
}
