const fn record_change(op: str, target: str, summary: str) -> bool {
    let changed = runtime_last_changed();
    runtime_record_change(op, target, summary, changed);
    runtime_last_changed()
}

const fn parse_group_field(info: str, index: i64) -> str {
    let parts = info.split(":");
    if parts.len() > index {
        parts[index]
    } else {
        ""
    }
}

const fn members_missing(current: str, desired: str) -> bool {
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
pub const fn group(
    name: str,
    context hosts: str = "localhost",
    present: bool = true,
    gid: str = "",
    members: str = "",
    sudo: bool = true,
) -> bool {
    let exists = std::facts::groups::exists(name);
    let mut summary = "no-op";

    if present {
        if !exists {
            let mut opts = "";
            if gid != "" {
                opts = f"{opts} -g {gid}";
            }
            std::ops::server::shell(f"groupadd{opts} {name}", hosts=hosts, sudo=sudo);
            summary = "created";
            return record_change("groups", name, summary);
        }

        let info = std::facts::groups::info(name);
        let current_gid = parse_group_field(info, 2);
        let current_members = parse_group_field(info, 3);

        let mut needs_update = false;
        if gid != "" && gid != current_gid {
            std::ops::server::shell(f"groupmod -g {gid} {name}", hosts=hosts, sudo=sudo);
            needs_update = true;
        }
        if members != "" && members != current_members {
            std::ops::server::shell(f"gpasswd -M {members} {name}", hosts=hosts, sudo=sudo);
            needs_update = true;
        }

        if needs_update {
            summary = "updated";
        } else {
            runtime_set_changed(false);
        }
        return record_change("groups", name, summary);
    }

    if exists {
        std::ops::server::shell(f"groupdel {name}", hosts=hosts, sudo=sudo);
        summary = "removed";
    } else {
        runtime_set_changed(false);
    }
    record_change("groups", name, summary)
}

#[cfg(target_lang = "pwsh")]
pub const fn group(
    name: str,
    context hosts: str = "localhost",
    present: bool = true,
    gid: str = "",
    members: str = "",
    sudo: bool = true,
) -> bool {
    let exists = std::facts::groups::exists(name);
    let mut summary = "no-op";

    if gid != "" {
        summary = "unsupported on windows: gid";
    }

    if present {
        if !exists {
            std::ops::server::shell(
                f"New-LocalGroup -Name \"{name}\"",
                hosts=hosts,
                sudo=sudo,
            );
            summary = "created";
        }
        if members != "" {
            let current_members = std::facts::server::command(
                f"Get-LocalGroupMember -Group \"{name}\" | Select -ExpandProperty Name",
            );
            if members_missing(current_members, members) {
                let list = members.split(",");
                let mut idx = 0;
                while idx < list.len() {
                    let member = list[idx];
                    if member != "" && !current_members.contains(member) {
                        std::ops::server::shell(
                            f"Add-LocalGroupMember -Group \"{name}\" -Member \"{member}\"",
                            hosts=hosts,
                            sudo=sudo,
                        );
                    }
                    idx = idx + 1;
                }
                summary = "updated";
            } else {
                runtime_set_changed(false);
            }
        } else if summary == "no-op" {
            runtime_set_changed(false);
        }
        return record_change("groups", name, summary);
    }

    if exists {
        std::ops::server::shell(
            f"Remove-LocalGroup -Name \"{name}\"",
            hosts=hosts,
            sudo=sudo,
        );
        summary = "removed";
    } else {
        runtime_set_changed(false);
    }
    record_change("groups", name, summary)
}
