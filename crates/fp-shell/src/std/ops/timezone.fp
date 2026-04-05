const fn record_change(op: str, target: str, summary: str) -> bool {
    let changed = runtime_last_changed();
    runtime_record_change(op, target, summary, changed);
    runtime_last_changed()
}

#[cfg(target_lang = "bash")]
pub const fn timezone(
    name: str,
    context hosts: str = "localhost",
    present: bool = true,
    sudo: bool = true,
) -> bool {
    let current = std::facts::server::command(
        "timedatectl show -p Timezone --value 2>/dev/null || cat /etc/timezone 2>/dev/null || readlink /etc/localtime | sed 's#.*/zoneinfo/##'",
    );
    let desired = match present {
        true => name,
        false => "UTC",
    };
    let mut summary = "no-op";

    if current != desired {
        let timedatectl = std::facts::server::which("timedatectl");
        if timedatectl != "" {
            std::ops::server::shell(
                f"timedatectl set-timezone {desired}",
                hosts=hosts,
                sudo=sudo,
            );
        } else {
            std::ops::server::shell(
                f"ln -sf /usr/share/zoneinfo/{desired} /etc/localtime",
                hosts=hosts,
                sudo=sudo,
            );
            if std::facts::files::exists("/etc/timezone") {
                std::ops::server::shell(
                    f"printf '%s\\n' \"{desired}\" > /etc/timezone",
                    hosts=hosts,
                    sudo=sudo,
                );
            }
        }
        summary = "updated";
    } else {
        runtime_set_changed(false);
    }

    record_change("timezone", desired, summary)
}

#[cfg(target_lang = "pwsh")]
pub const fn timezone(
    name: str,
    context hosts: str = "localhost",
    present: bool = true,
    sudo: bool = true,
) -> bool {
    let current = std::facts::server::command("Get-TimeZone | Select -ExpandProperty Id");
    let desired = match present {
        true => name,
        false => "UTC",
    };
    let mut summary = "no-op";

    if current != desired {
        std::ops::server::shell(
            f"Set-TimeZone -Id \"{desired}\"",
            hosts=hosts,
            sudo=sudo,
        );
        summary = "updated";
    } else {
        runtime_set_changed(false);
    }

    record_change("timezone", desired, summary)
}
