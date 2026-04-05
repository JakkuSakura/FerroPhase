const fn record_change(op: str, target: str, summary: str) -> bool {
    let changed = runtime_last_changed();
    runtime_record_change(op, target, summary, changed);
    runtime_last_changed()
}

#[cfg(target_lang = "bash")]
pub const fn entry(
    name: str,
    context hosts: str = "localhost",
    content: str = "",
    present: bool = true,
    file: str = "",
    mode: str = "0440",
    validate: bool = false,
    visudo_path: str = "/usr/sbin/visudo",
    sudo: bool = true,
) -> bool {
    let target = match file {
        "" => f"/etc/sudoers.d/{name}",
        _ => file,
    };
    let desired = match content {
        "" => f"{name} ALL=(ALL) NOPASSWD:ALL",
        _ => content,
    };
    let mut summary = "no-op";

    if present {
        let same = if std::facts::files::exists(target) {
            let current = std::facts::files::read_file(target);
            current == desired || current == f"{desired}\n"
        } else {
            false
        };

        if !same {
            let mut backup = "";
            if validate && std::facts::files::exists(target) {
                backup = f"{target}.fp-sudoers-backup";
                std::ops::server::shell(
                    f"cp {target} {backup}",
                    hosts=hosts,
                    sudo=sudo,
                );
            }
            std::ops::server::shell(
                f"printf '%s\\n' \"{desired}\" > {target}",
                hosts=hosts,
                sudo=sudo,
            );
            std::ops::server::shell(f"chmod {mode} {target}", hosts=hosts, sudo=sudo);
            if validate {
                let rollback = match backup {
                    "" => f"rm -f {target} && exit 1",
                    _ => f"mv {backup} {target} && exit 1",
                };
                std::ops::server::shell(
                    f"{visudo_path} -cf {target} || ({rollback})",
                    hosts=hosts,
                    sudo=sudo,
                );
                if backup != "" {
                    std::ops::server::shell(f"rm -f {backup}", hosts=hosts, sudo=sudo);
                }
            }
            summary = "updated";
        } else {
            runtime_set_changed(false);
        }
        return record_change("sudoers", target, summary);
    }

    if std::facts::files::exists(target) {
        std::ops::server::shell(f"rm -f {target}", hosts=hosts, sudo=sudo);
        summary = "removed";
    } else {
        runtime_set_changed(false);
    }
    record_change("sudoers", target, summary)
}

#[cfg(target_lang = "pwsh")]
pub const fn entry(
    name: str,
    context hosts: str = "localhost",
    _content: str = "",
    _present: bool = true,
    _file: str = "",
    _mode: str = "",
    _validate: bool = false,
    _visudo_path: str = "",
    _sudo: bool = true,
) -> bool {
    runtime_set_changed(false);
    runtime_record_change("sudoers", name, "unsupported on windows", false);
    runtime_last_changed()
}
