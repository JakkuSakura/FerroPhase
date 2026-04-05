const fn record_change(op: str, target: str, summary: str) -> bool {
    let changed = runtime_last_changed();
    runtime_record_change(op, target, summary, changed);
    runtime_last_changed()
}

const fn escape_bash_single_quotes(value: str) -> str {
    value.replace("'", "'\"'\"'")
}

#[cfg(target_lang = "bash")]
pub const fn entry(
    ip: str,
    names: str,
    context hosts: str = "localhost",
    present: bool = true,
    path: str = "/etc/hosts",
    sudo: bool = true,
) -> bool {
    let line = f"{ip} {names}";
    let content = std::facts::files::read_file(path);
    let mut summary = "no-op";
    let lines = content.split("\n");
    let mut idx = 0;
    let mut ip_total = 0;
    let mut ip_exact = 0;
    while idx < lines.len() {
        let entry = lines[idx];
        if entry.starts_with(f"{ip} ") {
            ip_total = ip_total + 1;
            if entry == line {
                ip_exact = ip_exact + 1;
            }
        }
        idx = idx + 1;
    }

    if present {
        if ip_total == 1 && ip_exact == 1 {
            runtime_set_changed(false);
        } else {
            let escaped_ip = escape_bash_single_quotes(ip);
            std::ops::server::shell(
                f"awk -v ip='{escaped_ip}' '$1 != ip' {path} > {path}.fp.tmp && mv {path}.fp.tmp {path}",
                hosts=hosts,
                sudo=sudo,
            );
            std::ops::server::shell(
                f"printf '%s\\n' \"{line}\" >> {path}",
                hosts=hosts,
                sudo=sudo,
            );
            summary = "updated";
        }
        return record_change("hosts", line, summary);
    }

    if ip_total > 0 {
        let escaped_ip = escape_bash_single_quotes(ip);
        std::ops::server::shell(
            f"awk -v ip='{escaped_ip}' '$1 != ip' {path} > {path}.fp.tmp && mv {path}.fp.tmp {path}",
            hosts=hosts,
            sudo=sudo,
        );
        summary = "removed";
    } else {
        runtime_set_changed(false);
    }
    record_change("hosts", line, summary)
}

#[cfg(target_lang = "pwsh")]
pub const fn entry(
    ip: str,
    names: str,
    context hosts: str = "localhost",
    _present: bool = true,
    _path: str = "",
    _sudo: bool = true,
) -> bool {
    runtime_set_changed(false);
    runtime_record_change("hosts", f"{ip} {names}", "unsupported on windows", false);
    runtime_last_changed()
}
