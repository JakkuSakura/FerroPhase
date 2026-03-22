pub const fn service(
    service: str,
    context hosts: str = "localhost",
    running: bool = true,
    restarted: bool,
    reloaded: bool,
    command: str,
    enabled: bool,
    sudo: bool = true,
) -> bool {
    if command != "" {
        std::ops::server::shell(
            f"test -e /etc/rc.d/{service} && /etc/rc.d/{service} {command} || /usr/local/etc/rc.d/{service} {command}",
            hosts=hosts,
            sudo=sudo,
        );
    } else {
        if restarted {
            std::ops::server::shell(
                f"test -e /etc/rc.d/{service} && /etc/rc.d/{service} restart || /usr/local/etc/rc.d/{service} restart",
                hosts=hosts,
                sudo=sudo,
            );
        } else {
            if reloaded {
                std::ops::server::shell(
                    f"test -e /etc/rc.d/{service} && /etc/rc.d/{service} reload || /usr/local/etc/rc.d/{service} reload",
                    hosts=hosts,
                    sudo=sudo,
                );
            } else {
                if running {
                    std::ops::server::shell(
                        f"test -e /etc/rc.d/{service} && /etc/rc.d/{service} start || /usr/local/etc/rc.d/{service} start",
                        hosts=hosts,
                        sudo=sudo,
                    );
                } else {
                    std::ops::server::shell(
                        f"test -e /etc/rc.d/{service} && /etc/rc.d/{service} stop || /usr/local/etc/rc.d/{service} stop",
                        hosts=hosts,
                        sudo=sudo,
                    );
                }
            }
        }
    }
    if enabled {
        std::ops::server::shell(
            f"grep -q '^{service}_enable=' /etc/rc.conf.local 2>/dev/null && sed -i.bak 's|^{service}_enable=.*|{service}_enable=\"YES\"|' /etc/rc.conf.local || printf '%s\\n' '{service}_enable=\"YES\"' >> /etc/rc.conf.local",
            hosts=hosts,
            sudo=sudo,
        )
    } else {
        std::ops::server::shell(
            f"grep -v '^{service}_enable=' /etc/rc.conf.local > /tmp/fp-shell-rc.conf.local 2>/dev/null || true; mv /tmp/fp-shell-rc.conf.local /etc/rc.conf.local 2>/dev/null || true",
            hosts=hosts,
            sudo=sudo,
        )
    }
}
