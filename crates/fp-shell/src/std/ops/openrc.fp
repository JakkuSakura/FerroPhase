pub const fn service(service: str, context hosts: str = "localhost", running: bool = true, restarted: bool, reloaded: bool, command: str, enabled: bool, runlevel: str, sudo: bool = true) -> bool {
    if command != "" {
        std::ops::server::shell(f"rc-service {service} {command}", hosts=hosts, sudo=sudo);
    } else {
        if restarted {
            std::ops::server::shell(f"rc-service {service} restart", hosts=hosts, sudo=sudo);
        } else {
            if reloaded {
                std::ops::server::shell(f"rc-service {service} reload", hosts=hosts, sudo=sudo);
            } else {
                if running {
                    std::ops::server::shell(f"rc-service {service} start", hosts=hosts, sudo=sudo);
                } else {
                    std::ops::server::shell(f"rc-service {service} stop", hosts=hosts, sudo=sudo);
                }
            }
        }
    }
    if enabled {
        if runlevel == "" {
            std::ops::server::shell(f"rc-update add {service} default", hosts=hosts, sudo=sudo)
        } else {
            std::ops::server::shell(f"rc-update add {service} {runlevel}", hosts=hosts, sudo=sudo)
        }
    } else {
        if runlevel == "" {
            std::ops::server::shell(f"rc-update del {service} default", hosts=hosts, sudo=sudo)
        } else {
            std::ops::server::shell(f"rc-update del {service} {runlevel}", hosts=hosts, sudo=sudo)
        }
    }
}
