pub const fn service(
    service: str,
    context hosts: str = "localhost",
    running: bool = true,
    restarted: bool,
    reloaded: bool,
    enabled: bool,
    daemon_reload: bool,
    sudo: bool,
    only_if: str,
    unless: str,
    creates: str,
    removes: str,
) -> bool {
    let service = normalize_service(service);
    if daemon_reload {
        std::ops::server::shell(
            "systemctl daemon-reload",
            hosts=hosts,
            sudo=sudo,
            only_if=only_if,
            unless=unless,
            creates=creates,
            removes=removes,
        );
    }
    if restarted {
        std::ops::server::shell(
            f"systemctl restart {service}",
            hosts=hosts,
            sudo=sudo,
            only_if=only_if,
            unless=unless,
            creates=creates,
            removes=removes,
        );
    } else {
        if reloaded {
            std::ops::server::shell(
                f"systemctl reload {service}",
                hosts=hosts,
                sudo=sudo,
                only_if=only_if,
                unless=unless,
                creates=creates,
                removes=removes,
            );
        } else {
            if running {
                std::ops::server::shell(
                    f"systemctl start {service}",
                    hosts=hosts,
                    sudo=sudo,
                    only_if=only_if,
                    unless=unless,
                    creates=creates,
                    removes=removes,
                );
            } else {
                std::ops::server::shell(
                    f"systemctl stop {service}",
                    hosts=hosts,
                    sudo=sudo,
                    only_if=only_if,
                    unless=unless,
                    creates=creates,
                    removes=removes,
                );
            }
        }
    }
    if enabled {
        std::ops::server::shell(
            f"systemctl enable {service}",
            hosts=hosts,
            sudo=sudo,
        );
    } else {
        std::ops::server::shell(
            f"systemctl disable {service}",
            hosts=hosts,
            sudo=sudo,
        );
    }
    true
}

pub const fn daemon_reload(
    context hosts: str = "localhost",
    sudo: bool = true,
    only_if: str,
    unless: str,
    creates: str,
    removes: str,
) -> bool {
    std::ops::server::shell(
        "systemctl daemon-reload",
        hosts=hosts,
        sudo=sudo,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    )
}

const fn normalize_service(service: str) -> str {
    if std::shell::process::ok(
        f"printf '%s' {service} | grep -Eq '\\.(service|socket|device|mount|automount|swap|target|path|timer|slice|scope)$'"
    ) {
        service
    } else {
        f"{service}.service"
    }
}
