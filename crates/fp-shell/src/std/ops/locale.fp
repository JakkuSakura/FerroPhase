const fn record_change(op: str, target: str, summary: str) -> bool {
    let changed = runtime_last_changed();
    runtime_record_change(op, target, summary, changed);
    runtime_last_changed()
}

const fn parse_lang(content: str) -> str {
    let lines = content.split("\n");
    let mut idx = 0;
    while idx < lines.len() {
        let line = lines[idx];
        if line.starts_with("LANG=") {
            return line.replace("LANG=", "");
        }
        idx = idx + 1;
    }
    ""
}

#[cfg(target_lang = "bash")]
pub const fn locale(
    name: str,
    context hosts: str = "localhost",
    present: bool = true,
    sudo: bool = true,
    conf_path: str = "/etc/locale.conf",
) -> bool {
    let mut current = std::facts::server::command("locale | grep '^LANG=' | head -n1 | cut -d= -f2");
    if current == "" && std::facts::files::exists(conf_path) {
        let conf = std::facts::files::read_file(conf_path);
        current = parse_lang(conf);
    }
    let desired = match present {
        true => name,
        false => "C",
    };
    let mut summary = "no-op";

    if current != desired {
        let localectl = std::facts::server::which("localectl");
        if localectl != "" {
            std::ops::server::shell(
                f"localectl set-locale LANG={desired}",
                hosts=hosts,
                sudo=sudo,
            );
        } else {
            std::ops::server::shell(
                f"printf '%s\\n' \"LANG={desired}\" > {conf_path}",
                hosts=hosts,
                sudo=sudo,
            );
        }
        summary = "updated";
    } else {
        runtime_set_changed(false);
    }

    record_change("locale", desired, summary)
}

#[cfg(target_lang = "pwsh")]
pub const fn locale(
    name: str,
    context hosts: str = "localhost",
    present: bool = true,
    sudo: bool = true,
    _conf_path: str = "",
) -> bool {
    let current = std::facts::server::command("Get-WinSystemLocale | Select -ExpandProperty Name");
    let desired = match present {
        true => name,
        false => "en-US",
    };
    let mut summary = "no-op";

    if current != desired {
        std::ops::server::shell(
            f"Set-WinSystemLocale -SystemLocale \"{desired}\"",
            hosts=hosts,
            sudo=sudo,
        );
        summary = "updated";
    } else {
        runtime_set_changed(false);
    }

    record_change("locale", desired, summary)
}
