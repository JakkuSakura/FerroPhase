const fn record_change(op: str, target: str, summary: str) -> bool {
    let changed = runtime_last_changed();
    runtime_record_change(op, target, summary, changed);
    runtime_last_changed()
}

const fn parse_passwd_field(info: str, index: i64) -> str {
    let parts = info.split(":");
    if parts.len() > index {
        parts[index]
    } else {
        ""
    }
}

const fn groups_missing(current: str, desired: str) -> bool {
    if desired == "" {
        return false;
    }
    let list = desired.split(",");
    let mut idx = 0;
    while idx < list.len() {
        let name = list[idx];
        if name != "" && !current.contains(name) {
            return true;
        }
        idx = idx + 1;
    }
    false
}

#[cfg(target_lang = "bash")]
pub const fn user(
    name: str,
    context hosts: str = "localhost",
    present: bool = true,
    uid: str = "",
    gid: str = "",
    groups: str = "",
    shell: str = "",
    home: str = "",
    system: bool = false,
    remove_home: bool = false,
    sudo: bool = true,
) -> bool {
    let exists = std::facts::users::exists(name);
    let mut summary = "no-op";

    if present {
        if !exists {
            let mut opts = "";
            if system {
                opts = f"{opts} -r";
            }
            if uid != "" {
                opts = f"{opts} -u {uid}";
            }
            if gid != "" {
                opts = f"{opts} -g {gid}";
            }
            if groups != "" {
                opts = f"{opts} -G {groups}";
            }
            if shell != "" {
                opts = f"{opts} -s {shell}";
            }
            if home != "" {
                opts = f"{opts} -d {home} -m";
            }
            std::ops::server::shell(f"useradd{opts} {name}", hosts=hosts, sudo=sudo);
            summary = "created";
            return record_change("users", name, summary);
        }

        let info = std::facts::users::info(name);
        let current_uid = parse_passwd_field(info, 2);
        let current_gid = parse_passwd_field(info, 3);
        let current_home = parse_passwd_field(info, 5);
        let current_shell = parse_passwd_field(info, 6);
        let current_groups = std::facts::server::command(f"id -nG {name} 2>/dev/null || true");

        let mut needs_update = false;
        let mut opts = "";
        if uid != "" && uid != current_uid {
            opts = f"{opts} -u {uid}";
            needs_update = true;
        }
        if gid != "" && gid != current_gid {
            opts = f"{opts} -g {gid}";
            needs_update = true;
        }
        if home != "" && home != current_home {
            opts = f"{opts} -d {home} -m";
            needs_update = true;
        }
        if shell != "" && shell != current_shell {
            opts = f"{opts} -s {shell}";
            needs_update = true;
        }
        if groups != "" && groups_missing(current_groups, groups) {
            opts = f"{opts} -G {groups}";
            needs_update = true;
        }

        if needs_update {
            std::ops::server::shell(f"usermod{opts} {name}", hosts=hosts, sudo=sudo);
            summary = "updated";
        } else {
            runtime_set_changed(false);
        }
        return record_change("users", name, summary);
    }

    if exists {
        let cmd = match remove_home {
            true => f"userdel -r {name}",
            false => f"userdel {name}",
        };
        std::ops::server::shell(cmd, hosts=hosts, sudo=sudo);
        summary = "removed";
    } else {
        runtime_set_changed(false);
    }
    record_change("users", name, summary)
}

#[cfg(target_lang = "pwsh")]
pub const fn user(
    name: str,
    context hosts: str = "localhost",
    present: bool = true,
    uid: str = "",
    gid: str = "",
    groups: str = "",
    shell: str = "",
    home: str = "",
    _system: bool = false,
    _remove_home: bool = false,
    sudo: bool = true,
) -> bool {
    let exists = std::facts::users::exists(name);
    let mut summary = "no-op";

    if uid != "" || gid != "" || shell != "" || home != "" {
        summary = "unsupported on windows: uid/gid/shell/home";
    }

    if present {
        if !exists {
            std::ops::server::shell(
                f"New-LocalUser -Name \"{name}\" -NoPassword -AccountNeverExpires",
                hosts=hosts,
                sudo=sudo,
            );
            summary = "created";
        }
        if groups != "" {
            let list = groups.split(",");
            let mut idx = 0;
            while idx < list.len() {
                let group = list[idx];
                if group != "" {
                    std::ops::server::shell(
                        f"Add-LocalGroupMember -Group \"{group}\" -Member \"{name}\" -ErrorAction SilentlyContinue",
                        hosts=hosts,
                        sudo=sudo,
                    );
                }
                idx = idx + 1;
            }
            summary = "updated";
        } else if summary == "no-op" {
            runtime_set_changed(false);
        }
        return record_change("users", name, summary);
    }

    if exists {
        std::ops::server::shell(
            f"Remove-LocalUser -Name \"{name}\"",
            hosts=hosts,
            sudo=sudo,
        );
        summary = "removed";
    } else {
        runtime_set_changed(false);
    }
    record_change("users", name, summary)
}
