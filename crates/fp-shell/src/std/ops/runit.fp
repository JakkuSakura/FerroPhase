pub const fn service(service: str, context hosts: str = "localhost", running: bool = true, restarted: bool, reloaded: bool, command: str, enabled: bool, managed: bool, svdir: str, sourcedir: str, sudo: bool) -> bool {
    std::ops::runit::manage(service, hosts=hosts, managed=managed, svdir=svdir, sourcedir=sourcedir, sudo=sudo);
    std::ops::runit::auto(service, hosts=hosts, auto=enabled, sourcedir=sourcedir, sudo=sudo);
    if managed {
        if command != "" {
            std::ops::server::shell(f"SVDIR={svdir} sv {command} {service}", hosts=hosts, sudo=sudo);
        } else {
            if restarted {
                std::ops::server::shell(f"SVDIR={svdir} sv restart {service}", hosts=hosts, sudo=sudo);
            } else {
                if reloaded {
                    std::ops::server::shell(f"SVDIR={svdir} sv reload {service}", hosts=hosts, sudo=sudo);
                } else {
                    if running {
                        std::ops::server::shell(f"SVDIR={svdir} sv start {service}", hosts=hosts, sudo=sudo);
                    } else {
                        std::ops::server::shell(f"SVDIR={svdir} sv stop {service}", hosts=hosts, sudo=sudo);
                    }
                }
            }
        }
    } else {
        true
    }
}

pub const fn manage(service: str, context hosts: str = "localhost", managed: bool = true, svdir: str, sourcedir: str, sudo: bool) -> bool {
    if managed {
        std::ops::server::shell(f"ln -sfn {sourcedir}/{service} {svdir}/{service}", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"rm -f {svdir}/{service}", hosts=hosts, sudo=sudo)
    }
}

pub const fn wait_runsv(service: str, context hosts: str = "localhost", svdir: str, timeout: str, sudo: bool) -> bool {
    std::ops::server::shell(
        f"export SVDIR={svdir}; for i in $(seq {timeout}); do sv status {service} > /dev/null && exit 0; sleep 1; done; exit 1",
        hosts=hosts,
        sudo=sudo,
    )
}

pub const fn auto(service: str, context hosts: str = "localhost", auto: bool = true, sourcedir: str, sudo: bool) -> bool {
    if auto {
        std::ops::server::shell(f"rm -f {sourcedir}/{service}/down", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"touch {sourcedir}/{service}/down", hosts=hosts, sudo=sudo)
    }
}
