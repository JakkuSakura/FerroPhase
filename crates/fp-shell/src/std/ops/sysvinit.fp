pub const fn service(service: str, context hosts: str = "localhost", running: bool = true, restarted: bool, reloaded: bool, enabled: bool, command: str, sudo: bool = true) -> bool {
    if command != "" {
        std::ops::server::shell(f"/etc/init.d/{service} {command}", hosts=hosts, sudo=sudo);
    } else {
        if restarted {
            std::ops::server::shell(f"/etc/init.d/{service} restart", hosts=hosts, sudo=sudo);
        } else {
            if reloaded {
                std::ops::server::shell(f"/etc/init.d/{service} reload", hosts=hosts, sudo=sudo);
            } else {
                if running {
                    std::ops::server::shell(f"/etc/init.d/{service} start", hosts=hosts, sudo=sudo);
                } else {
                    std::ops::server::shell(f"/etc/init.d/{service} stop", hosts=hosts, sudo=sudo);
                }
            }
        }
    }
    if enabled {
        std::ops::server::shell(f"update-rc.d {service} defaults || chkconfig {service} on || rc-update add {service} default", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"update-rc.d -f {service} remove || chkconfig {service} off || rc-update del {service} default", hosts=hosts, sudo=sudo)
    }
}

pub const fn enable(service: str, context hosts: str = "localhost", start_priority: str, stop_priority: str, start_levels: str, stop_levels: str, sudo: bool = true) -> bool {
    std::ops::server::shell(
        f"for level in {start_levels}; do ln -sfn /etc/init.d/{service} /etc/rc${{level}}.d/S{start_priority}{service}; done; for level in {stop_levels}; do ln -sfn /etc/init.d/{service} /etc/rc${{level}}.d/K{stop_priority}{service}; done",
        hosts=hosts,
        sudo=sudo,
    )
}
