const fn record_change(op: str, target: str, summary: str) -> bool {
    let changed = runtime_last_changed();
    runtime_record_change(op, target, summary, changed);
    runtime_last_changed()
}

const fn sysctl_line(key: str, value: str) -> str {
    f"{key} = {value}"
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

#[cfg(target_lang = "bash")]
pub const fn value(
    key: str,
    value: str,
    context hosts: str = "localhost",
    present: bool = true,
    persist: bool = true,
    conf_path: str = "/etc/sysctl.conf",
    sudo: bool = true,
) -> bool {
    let current = std::facts::server::command(f"sysctl -n {key} 2>/dev/null || true");
    let mut summary = "no-op";

    if present {
        if current == "" || current != value {
            std::ops::server::shell(f"sysctl -w {key}={value}", hosts=hosts, sudo=sudo);
            summary = "updated";
        }

        if persist {
            let key_regex = regex_escape(key);
            let desired = sysctl_line(key, value);
            let conf_exists = std::facts::files::exists(conf_path);
            let mut needs_persist = true;
            if conf_exists {
                let conf = std::facts::files::read_file(conf_path);
                if conf.contains(desired) {
                    needs_persist = false;
                }
            }
            if needs_persist {
                if conf_exists {
                    let escaped = sed_replace_escape(desired);
                    std::ops::server::shell(
                        f"(grep -q '^\\s*{key_regex}\\b' {conf_path} && sed -i.a-timestamp 's/^\\s*{key_regex}.*$/{escaped}/' {conf_path} && rm -f {conf_path}.a-timestamp) || printf '%s\\n' \"{desired}\" >> {conf_path}",
                        hosts=hosts,
                        sudo=sudo,
                    );
                } else {
                    std::ops::server::shell(
                        f"printf '%s\\n' \"{desired}\" > {conf_path}",
                        hosts=hosts,
                        sudo=sudo,
                    );
                }
                summary = "updated";
            } else if summary == "no-op" {
                runtime_set_changed(false);
            }
        } else if summary == "no-op" {
            runtime_set_changed(false);
        }

        return record_change("sysctl", key, summary);
    }

    if persist {
        if std::facts::files::exists(conf_path) {
            let conf = std::facts::files::read_file(conf_path);
            if conf.contains(key) {
                std::ops::server::shell(
                    f"sed -i.a-timestamp '/^\\s*{regex_escape(key)}\\b/d' {conf_path} && rm -f {conf_path}.a-timestamp",
                    hosts=hosts,
                    sudo=sudo,
                );
                summary = "removed";
            } else {
                runtime_set_changed(false);
            }
        } else {
            runtime_set_changed(false);
        }
    } else {
        runtime_set_changed(false);
    }
    record_change("sysctl", key, summary)
}

#[cfg(target_lang = "pwsh")]
pub const fn value(
    key: str,
    value: str,
    context hosts: str = "localhost",
    _present: bool = true,
    _persist: bool = true,
    _conf_path: str = "",
    _sudo: bool = true,
) -> bool {
    runtime_set_changed(false);
    runtime_record_change("sysctl", key, "unsupported on windows", false);
    runtime_last_changed()
}
