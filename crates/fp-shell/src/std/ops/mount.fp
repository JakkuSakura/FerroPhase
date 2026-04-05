const fn record_change(op: str, target: str, summary: str) -> bool {
    let changed = runtime_last_changed();
    runtime_record_change(op, target, summary, changed);
    runtime_last_changed()
}

const fn fstab_line(src: str, path: str, fstype: str, options: str, dump: str, pass: str) -> str {
    f"{src} {path} {fstype} {options} {dump} {pass}"
}

const fn regex_escape(pattern: str) -> str {
    let mut escaped = pattern.replace("\\", "\\\\");
    escaped = escaped.replace("/", "\\/");
    escaped = escaped.replace(".", "\\.");
    escaped = escaped.replace("^", "\\^");
    escaped = escaped.replace("$", "\\$");
    escaped = escaped.replace("*", "\\*");
    escaped = escaped.replace("?", "\\?");
    escaped = escaped.replace("+", "\\+");
    escaped = escaped.replace("(", "\\(");
    escaped = escaped.replace(")", "\\)");
    escaped = escaped.replace("[", "\\[");
    escaped = escaped.replace("]", "\\]");
    escaped = escaped.replace("{", "\\{");
    escaped = escaped.replace("}", "\\}");
    escaped = escaped.replace("|", "\\|");
    escaped
}

const fn sed_replace_escape(value: str) -> str {
    let mut escaped = value.replace("\\", "\\\\");
    escaped = escaped.replace("&", "\\&");
    escaped = escaped.replace("/", "\\/");
    escaped
}

const fn fstab_ensure(
    path: str,
    src: str,
    fstype: str,
    options: str,
    dump: str,
    pass: str,
    fstab_path: str,
    context hosts: str,
    sudo: bool,
) -> bool {
    let src_value = match src {
        "" => path,
        _ => src,
    };
    let fstype_value = match fstype {
        "" => "auto",
        _ => fstype,
    };
    let options_value = match options {
        "" => "defaults",
        _ => options,
    };
    let desired = fstab_line(src_value, path, fstype_value, options_value, dump, pass);
    let fstab_exists = std::facts::files::exists(fstab_path);
    if fstab_exists {
        let conf = std::facts::files::read_file(fstab_path);
        if conf.contains(desired) {
            return false;
        }
    }

    let path_regex = regex_escape(path);
    let escaped = sed_replace_escape(desired);
    if fstab_exists {
        std::ops::server::shell(
            f"(grep -q '^[^#]*[[:space:]]\\+{path_regex}[[:space:]]' {fstab_path} && sed -i.a-timestamp 's/^\\s*[^#].*[[:space:]]\\+{path_regex}[[:space:]].*$/{escaped}/' {fstab_path} && rm -f {fstab_path}.a-timestamp) || printf '%s\\n' \"{desired}\" >> {fstab_path}",
            hosts=hosts,
            sudo=sudo,
        );
        return true;
    }

    std::ops::server::shell(
        f"printf '%s\\n' \"{desired}\" > {fstab_path}",
        hosts=hosts,
        sudo=sudo,
    );
    true
}

const fn fstab_remove(
    path: str,
    fstab_path: str,
    context hosts: str,
    sudo: bool,
) -> bool {
    if std::facts::files::exists(fstab_path) {
        let conf = std::facts::files::read_file(fstab_path);
        if conf.contains(path) {
            std::ops::server::shell(
                f"sed -i.a-timestamp '/^\\s*[^#].*[[:space:]]\\+{regex_escape(path)}[[:space:]]/d' {fstab_path} && rm -f {fstab_path}.a-timestamp",
                hosts=hosts,
                sudo=sudo,
            );
            return true;
        }
    }
    false
}

#[cfg(target_lang = "bash")]
pub const fn mount(
    path: str,
    context hosts: str = "localhost",
    src: str = "",
    fstype: str = "",
    options: str = "",
    persist: bool = false,
    fstab_path: str = "/etc/fstab",
    dump: str = "0",
    pass: str = "0",
    present: bool = true,
    sudo: bool = true,
) -> bool {
    let info = std::facts::mounts::find(path);
    let mounted = info != "";
    let mut summary = "no-op";
    let mut mount_changed = false;
    let mut fstab_changed = false;

    if present {
        let mut needs_mount = false;
        if !mounted {
            needs_mount = true;
        } else if src != "" && !info.contains(src) {
            needs_mount = true;
        } else if fstype != "" && !info.contains(fstype) {
            needs_mount = true;
        } else if options != "" && !info.contains(options) {
            needs_mount = true;
        }

        if needs_mount {
            if mounted {
                std::ops::server::shell(f"umount {path}", hosts=hosts, sudo=sudo);
            }
            let src_value = match src {
                "" => path,
                _ => src,
            };
            let type_flag = match fstype {
                "" => "",
                _ => f"-t {fstype} ",
            };
            let opt_flag = match options {
                "" => "",
                _ => f"-o {options} ",
            };
            std::ops::server::shell(
                f"mount {type_flag}{opt_flag}{src_value} {path}",
                hosts=hosts,
                sudo=sudo,
            );
            summary = "mounted";
            mount_changed = true;
        }

        if persist {
            if fstab_ensure(path, src, fstype, options, dump, pass, fstab_path, hosts, sudo) {
                fstab_changed = true;
                if summary == "no-op" {
                    summary = "updated";
                }
            }
        }

        if !mount_changed && !fstab_changed {
            runtime_set_changed(false);
        }
        return record_change("mount", path, summary);
    }

    if mounted {
        std::ops::server::shell(f"umount {path}", hosts=hosts, sudo=sudo);
        summary = "unmounted";
        mount_changed = true;
    }

    if persist {
        if fstab_remove(path, fstab_path, hosts, sudo) {
            fstab_changed = true;
            if summary == "no-op" {
                summary = "removed";
            }
        }
    }

    if !mount_changed && !fstab_changed {
        runtime_set_changed(false);
    }
    record_change("mount", path, summary)
}

#[cfg(target_lang = "pwsh")]
pub const fn mount(
    path: str,
    context hosts: str = "localhost",
    _src: str = "",
    _fstype: str = "",
    _options: str = "",
    _persist: bool = false,
    _fstab_path: str = "",
    _dump: str = "",
    _pass: str = "",
    _present: bool = true,
    _sudo: bool = true,
) -> bool {
    runtime_set_changed(false);
    runtime_record_change("mount", path, "unsupported on windows", false);
    runtime_last_changed()
}
