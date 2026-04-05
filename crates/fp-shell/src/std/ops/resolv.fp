const fn record_change(op: str, target: str, summary: str) -> bool {
    let changed = runtime_last_changed();
    runtime_record_change(op, target, summary, changed);
    runtime_last_changed()
}

const fn escape_bash_single_quotes(value: str) -> str {
    value.replace("'", "'\"'\"'")
}

#[cfg(target_lang = "bash")]
pub const fn resolv(
    nameserver: str,
    context hosts: str = "localhost",
    search: str = "",
    options: str = "",
    present: bool = true,
    path: str = "/etc/resolv.conf",
    sudo: bool = true,
) -> bool {
    let content = std::facts::files::read_file(path);
    let mut summary = "no-op";
    let mut changed = false;
    let lines = content.split("\n");
    let mut idx = 0;
    let mut nameserver_total = 0;
    let mut nameserver_exact = 0;
    let mut search_total = 0;
    let mut search_exact = 0;
    let mut options_total = 0;
    let mut options_exact = 0;
    while idx < lines.len() {
        let entry = lines[idx];
        if entry.starts_with("nameserver ") {
            nameserver_total = nameserver_total + 1;
            if entry == f"nameserver {nameserver}" {
                nameserver_exact = nameserver_exact + 1;
            }
        }
        if entry.starts_with("search ") {
            search_total = search_total + 1;
            if entry == f"search {search}" {
                search_exact = search_exact + 1;
            }
        }
        if entry.starts_with("options ") {
            options_total = options_total + 1;
            if entry == f"options {options}" {
                options_exact = options_exact + 1;
            }
        }
        idx = idx + 1;
    }

    if present {
        if nameserver != "" {
            let line = f"nameserver {nameserver}";
            if !(nameserver_total == 1 && nameserver_exact == 1) {
                let escaped_key = escape_bash_single_quotes("nameserver");
                std::ops::server::shell(
                    f"awk -v key='{escaped_key}' '$1 != key' {path} > {path}.fp.tmp && mv {path}.fp.tmp {path}",
                    hosts=hosts,
                    sudo=sudo,
                );
                std::ops::server::shell(
                    f"printf '%s\\n' \"{line}\" >> {path}",
                    hosts=hosts,
                    sudo=sudo,
                );
                changed = true;
            }
        }
        if search != "" {
            let line = f"search {search}";
            if !(search_total == 1 && search_exact == 1) {
                let escaped_key = escape_bash_single_quotes("search");
                std::ops::server::shell(
                    f"awk -v key='{escaped_key}' '$1 != key' {path} > {path}.fp.tmp && mv {path}.fp.tmp {path}",
                    hosts=hosts,
                    sudo=sudo,
                );
                std::ops::server::shell(
                    f"printf '%s\\n' \"{line}\" >> {path}",
                    hosts=hosts,
                    sudo=sudo,
                );
                changed = true;
            }
        }
        if options != "" {
            let line = f"options {options}";
            if !(options_total == 1 && options_exact == 1) {
                let escaped_key = escape_bash_single_quotes("options");
                std::ops::server::shell(
                    f"awk -v key='{escaped_key}' '$1 != key' {path} > {path}.fp.tmp && mv {path}.fp.tmp {path}",
                    hosts=hosts,
                    sudo=sudo,
                );
                std::ops::server::shell(
                    f"printf '%s\\n' \"{line}\" >> {path}",
                    hosts=hosts,
                    sudo=sudo,
                );
                changed = true;
            }
        }

        if changed {
            summary = "updated";
        } else {
            runtime_set_changed(false);
        }
        return record_change("resolv", path, summary);
    }

    if nameserver != "" && nameserver_exact > 0 {
        let line = f"nameserver {nameserver}";
        let escaped_line = escape_bash_single_quotes(line);
        std::ops::server::shell(
            f"awk -v line='{escaped_line}' '$0 != line' {path} > {path}.fp.tmp && mv {path}.fp.tmp {path}",
            hosts=hosts,
            sudo=sudo,
        );
        changed = true;
    }
    if search != "" && search_exact > 0 {
        let line = f"search {search}";
        let escaped_line = escape_bash_single_quotes(line);
        std::ops::server::shell(
            f"awk -v line='{escaped_line}' '$0 != line' {path} > {path}.fp.tmp && mv {path}.fp.tmp {path}",
            hosts=hosts,
            sudo=sudo,
        );
        changed = true;
    }
    if options != "" && options_exact > 0 {
        let line = f"options {options}";
        let escaped_line = escape_bash_single_quotes(line);
        std::ops::server::shell(
            f"awk -v line='{escaped_line}' '$0 != line' {path} > {path}.fp.tmp && mv {path}.fp.tmp {path}",
            hosts=hosts,
            sudo=sudo,
        );
        changed = true;
    }

    if changed {
        summary = "removed";
    } else {
        runtime_set_changed(false);
    }
    record_change("resolv", path, summary)
}

#[cfg(target_lang = "pwsh")]
pub const fn resolv(
    nameserver: str,
    context hosts: str = "localhost",
    _search: str = "",
    _options: str = "",
    _present: bool = true,
    _path: str = "",
    _sudo: bool = true,
) -> bool {
    runtime_set_changed(false);
    runtime_record_change("resolv", nameserver, "unsupported on windows", false);
    runtime_last_changed()
}
