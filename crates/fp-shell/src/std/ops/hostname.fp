const fn record_change(op: str, target: str, summary: str) -> bool {
    let changed = runtime_last_changed();
    runtime_record_change(op, target, summary, changed);
    runtime_last_changed()
}

#[cfg(target_lang = "bash")]
pub const fn hostname(
    name: str,
    context hosts: str = "localhost",
    present: bool = true,
    sudo: bool = true,
) -> bool {
    let current = std::facts::server::hostname();
    let desired = match present {
        true => name,
        false => "localhost",
    };
    let mut summary = "no-op";

    if current != desired {
        let hostnamectl = std::facts::server::which("hostnamectl");
        let scutil = std::facts::server::which("scutil");
        if hostnamectl != "" {
            std::ops::server::shell(
                f"hostnamectl set-hostname {desired}",
                hosts=hosts,
                sudo=sudo,
            );
        } else if scutil != "" {
            std::ops::server::shell(
                f"scutil --set HostName {desired} && scutil --set ComputerName {desired} && scutil --set LocalHostName {desired}",
                hosts=hosts,
                sudo=sudo,
            );
        } else {
            std::ops::server::shell(f"hostname {desired}", hosts=hosts, sudo=sudo);
        }
        summary = "updated";
    } else {
        runtime_set_changed(false);
    }

    record_change("hostname", desired, summary)
}

#[cfg(target_lang = "pwsh")]
pub const fn hostname(
    name: str,
    context hosts: str = "localhost",
    present: bool = true,
    sudo: bool = true,
) -> bool {
    let current = std::facts::server::hostname();
    let desired = match present {
        true => name,
        false => "localhost",
    };
    let mut summary = "no-op";

    if current != desired {
        std::ops::server::shell(
            f"Rename-Computer -NewName \"{desired}\" -Force",
            hosts=hosts,
            sudo=sudo,
        );
        summary = "updated";
    } else {
        runtime_set_changed(false);
    }

    record_change("hostname", desired, summary)
}
