use std::collections::hash_map::HashMap;

const APT_UPDATE_FILENAME: str = "/var/lib/apt/periodic/update-success-stamp";

const fn noninteractive_apt(command: str, force: bool) -> str {
    let base = match force {
        true => "DEBIAN_FRONTEND=noninteractive apt-get -y --force-yes",
        false => "DEBIAN_FRONTEND=noninteractive apt-get -y",
    };
    f"{base} -o Dpkg::Options::=\"--force-confdef\" -o Dpkg::Options::=\"--force-confold\" {command}"
}

const fn escape_regex(value: str) -> str {
    let mut out = value;
    out = out.replace("\\", "\\\\");
    out = out.replace(".", "\\.");
    out = out.replace("*", "\\*");
    out = out.replace("+", "\\+");
    out = out.replace("?", "\\?");
    out = out.replace("(", "\\(");
    out = out.replace(")", "\\)");
    out = out.replace("[", "\\[");
    out = out.replace("]", "\\]");
    out = out.replace("{", "\\{");
    out = out.replace("}", "\\}");
    out = out.replace("|", "\\|");
    out = out.replace("^", "\\^");
    out = out.replace("$", "\\$");
    out = out.replace("/", "\\/");
    out
}

const fn parse_apt_repo(src: str) -> HashMap<str, any> {
    let mut result = HashMap::new();
    let parts = src.split(" ");
    if parts.len() < 4 {
        return result;
    }
    let mut idx = 0;
    let mut repo_type = parts[0];
    let mut options = HashMap::new();
    idx = 1;
    if parts[idx].starts_with("[") {
        let mut options_text = parts[idx];
        options_text = options_text.replace("[", "").replace("]", "");
        let option_parts = options_text.split(" ");
        let mut opt_idx = 0;
        while opt_idx < option_parts.len() {
            let option = option_parts[opt_idx];
            let key_value = option.split("=");
            if key_value.len() == 2 {
                options.insert(key_value[0], key_value[1]);
            }
            opt_idx = opt_idx + 1;
        }
        idx = idx + 1;
    }
    let url = parts[idx];
    let distribution = parts[idx + 1];
    let mut components = Vec::new();
    let mut comp_idx = idx + 2;
    while comp_idx < parts.len() {
        let component = parts[comp_idx];
        if component != "" {
            components.push(component);
        }
        comp_idx = comp_idx + 1;
    }
    result.insert("type", repo_type);
    result.insert("url", url);
    result.insert("distribution", distribution);
    result.insert("components", components);
    result.insert("options", options);
    result
}

const fn repo_matches(existing: HashMap<str, any>, target: HashMap<str, any>) -> bool {
    if existing.len() == 0 || target.len() == 0 {
        return false;
    }
    existing.get_unchecked("type") == target.get_unchecked("type")
        && existing.get_unchecked("url") == target.get_unchecked("url")
        && existing.get_unchecked("distribution") == target.get_unchecked("distribution")
        && existing.get_unchecked("components") == target.get_unchecked("components")
        && existing.get_unchecked("options") == target.get_unchecked("options")
}

const fn list_contains_repo(items: Vec<any>, target: HashMap<str, any>) -> bool {
    let mut idx = 0;
    while idx < items.len() {
        if repo_matches(items[idx], target) {
            return true;
        }
        idx = idx + 1;
    }
    false
}

const fn quoted_package(name: str) -> str {
    if name.contains(">") || name.contains("<") {
        f"'{name}'"
    } else {
        name
    }
}

const fn package_parts(package: str) -> Vec<str> {
    package.split("=")
}

const fn map_versions_contains(versions: Vec<any>, version: str) -> bool {
    let mut idx = 0;
    while idx < versions.len() {
        if versions[idx] == version {
            return true;
        }
        idx = idx + 1;
    }
    false
}

const fn ensure_packages(
    packages: Vec<str>,
    current: HashMap<str, any>,
    present: bool,
    install_command: str,
    uninstall_command: str,
    upgrade_command: str,
    latest: bool,
) -> (Vec<str>, Vec<str>) {
    let mut to_install = Vec::new();
    let mut to_upgrade = Vec::new();
    let mut idx = 0;
    while idx < packages.len() {
        let pkg = packages[idx];
        let parts = package_parts(pkg);
        let name = parts[0];
        let has_version = parts.len() > 1;
        if present {
            if !current.contains_key(name) {
                to_install.push(quoted_package(pkg));
            } else {
                if has_version {
                    let versions = current.get_unchecked(name);
                    if !map_versions_contains(versions, parts[1]) {
                        to_install.push(quoted_package(pkg));
                    }
                } else if latest {
                    to_upgrade.push(quoted_package(pkg));
                }
            }
        } else {
            if current.contains_key(name) {
                to_install.push(quoted_package(pkg));
            }
        }
        idx = idx + 1;
    }
    (to_install, to_upgrade)
}

const fn join_packages(packages: Vec<str>) -> str {
    let mut rendered = Vec::new();
    let mut idx = 0;
    while idx < packages.len() {
        rendered.push(quoted_package(packages[idx]));
        idx = idx + 1;
    }
    rendered.join(" ")
}

pub const fn key(
    src: str = "",
    context hosts: str = "localhost",
    keyserver: str = "",
    keyid: Vec<str> = Vec::new(),
    sudo: bool = true,
) -> bool {
    let mut existing_keys = std::facts::apt::keys();
    if existing_keys == null {
        existing_keys = HashMap::new();
    }
    if src != "" {
        let key_data = std::facts::gpg::key(src);
        let mut resolved = Vec::new();
        if key_data != null && key_data.len() > 0 {
            resolved = key_data.keys;
        } else {
            resolved = keyid;
        }
        let mut missing = false;
        if resolved.len() == 0 {
            missing = true;
        } else {
            let mut idx = 0;
            while idx < resolved.len() {
                if !existing_keys.contains_key(resolved[idx]) {
                    missing = true;
                }
                idx = idx + 1;
            }
        }
        if missing {
            match src.contains("://") {
                true => std::ops::server::shell(
                    f"(wget -O - {src} || curl -sSLf {src}) | apt-key add -",
                    hosts=hosts,
                    sudo=sudo,
                ),
                false => std::ops::server::shell(f"apt-key add {src}", hosts=hosts, sudo=sudo),
            }
        }
    }

    if keyserver != "" {
        if keyid.len() == 0 {
            panic("`keyid` must be provided with `keyserver`");
        }
        let mut needed = Vec::new();
        let mut idx = 0;
        while idx < keyid.len() {
            let key = keyid[idx];
            if !existing_keys.contains_key(key) {
                needed.push(key);
            }
            idx = idx + 1;
        }
        if needed.len() > 0 {
            let joined = needed.join(" ");
            std::ops::server::shell(
                f"apt-key adv --keyserver {keyserver} --recv-keys {joined}",
                hosts=hosts,
                sudo=sudo,
            )
        }
    }
    runtime_last_changed()
}

pub const fn repo(
    src: str,
    context hosts: str = "localhost",
    filename: str = "",
    present: bool = true,
    sudo: bool = true,
) -> bool {
    let target = match filename {
        "" => "/etc/apt/sources.list",
        _ => f"/etc/apt/sources.list.d/{filename}.list",
    };
    let apt_sources = std::facts::apt::sources();
    let repo = parse_apt_repo(src);
    let is_present = match apt_sources {
        null => false,
        _ => list_contains_repo(apt_sources, repo),
    };

    match (is_present, present) {
        (false, true) => std::ops::server::shell(
            f"echo '{src}' >> {target}",
            hosts=hosts,
            sudo=sudo,
        ),
        (true, false) => {
            let regex = escape_regex(src);
            std::ops::server::shell(
                f"sed -i.a-timestamp '/^.*{regex}.*$/d' {target} && rm -f {target}.a-timestamp",
                hosts=hosts,
                sudo=sudo,
            )
        }
        _ => runtime_last_changed(),
    }
}

pub const fn ppa(
    src: str,
    context hosts: str = "localhost",
    present: bool = true,
    sudo: bool = true,
) -> bool {
    match present {
        true => std::ops::server::shell(f"apt-add-repository -y \"{src}\"", hosts=hosts, sudo=sudo),
        false => std::ops::server::shell(
            f"apt-add-repository -y --remove \"{src}\"",
            hosts=hosts,
            sudo=sudo,
        ),
    }
}

pub const fn deb(
    src: str,
    context hosts: str = "localhost",
    present: bool = true,
    force: bool = false,
    sudo: bool = true,
) -> bool {
    let mut package = src;
    let mut temp_filename = "_tempfile_";
    if src.contains("://") {
        let file_info = std::facts::files::file(temp_filename);
        if file_info == null {
            let curl = std::facts::server::which("curl");
            match curl == "" {
                true => std::ops::server::shell(
                    f"wget -q {src} -O {temp_filename} || ( rm -f {temp_filename} ; exit 1 )",
                    hosts=hosts,
                    sudo=sudo,
                ),
                false => std::ops::server::shell(
                    f"curl -sSLf {src} -o {temp_filename}",
                    hosts=hosts,
                    sudo=sudo,
                ),
            }
            std::ops::server::shell(f"mv {temp_filename} {temp_filename}", hosts=hosts, sudo=sudo);
        }
        package = temp_filename;
    }

    let info = std::facts::deb::package(package);
    let mut current = std::facts::deb::packages();
    if current == null {
        current = HashMap::new();
    }
    let mut exists = false;
    if info != null {
        let name = info.get_unchecked("name");
        let version = info.get_unchecked("version");
        if current.contains_key(name) {
            let versions = current.get_unchecked(name);
            exists = map_versions_contains(versions, version);
        }
    }

    match present {
        true => {
            if !exists {
                std::ops::server::shell(
                    f"dpkg --force-confdef --force-confold -i {package} 2> /dev/null || true",
                    hosts=hosts,
                    sudo=sudo,
                );
                let install_fix = noninteractive_apt("install -f", force);
                std::ops::server::shell(install_fix, hosts=hosts, sudo=sudo);
                std::ops::server::shell(
                    f"dpkg --force-confdef --force-confold -i {package}",
                    hosts=hosts,
                    sudo=sudo,
                )
            } else {
                runtime_last_changed()
            }
        }
        false => {
            if exists {
                let name = info.get_unchecked("name");
                let remove_command = noninteractive_apt("remove", force);
                std::ops::server::shell(f"{remove_command} {name}", hosts=hosts, sudo=sudo)
            } else {
                runtime_last_changed()
            }
        }
    }
}

pub const fn update(
    context hosts: str = "localhost",
    sudo: bool = true,
    cache_time: i64 = 0,
) -> bool {
    if cache_time > 0 {
        let file_info = std::facts::files::file(APT_UPDATE_FILENAME);
        if file_info != null {
            let mtime = file_info.get_unchecked("mtime");
            let now = std::facts::server::date();
            if mtime == now {
                return runtime_last_changed();
            }
        }
    }
    std::ops::server::shell("apt-get update", hosts=hosts, sudo=sudo);
    if cache_time > 0 {
        std::ops::server::shell(f"touch {APT_UPDATE_FILENAME}", hosts=hosts, sudo=sudo);
    }
    runtime_last_changed()
}

pub const fn upgrade(
    context hosts: str = "localhost",
    sudo: bool = true,
    auto_remove: bool = false,
) -> bool {
    let command = match auto_remove {
        true => "upgrade --autoremove",
        false => "upgrade",
    };
    std::ops::server::shell(noninteractive_apt(command, false), hosts=hosts, sudo=sudo)
}

pub const fn dist_upgrade(
    context hosts: str = "localhost",
    sudo: bool = true,
    auto_remove: bool = false,
) -> bool {
    let command = match auto_remove {
        true => "dist-upgrade --autoremove",
        false => "dist-upgrade",
    };
    std::ops::server::shell(noninteractive_apt(command, false), hosts=hosts, sudo=sudo)
}

pub const fn packages(
    packages: Vec<str> = Vec::new(),
    context hosts: str = "localhost",
    present: bool = true,
    latest: bool = false,
    update: bool = false,
    cache_time: i64 = 0,
    upgrade: bool = false,
    force: bool = false,
    no_recommends: bool = false,
    allow_downgrades: bool = false,
    extra_install_args: str = "",
    extra_uninstall_args: str = "",
    sudo: bool = true,
) -> bool {
    if update {
        std::ops::apt::update(hosts=hosts, sudo=sudo, cache_time=cache_time);
    }
    if upgrade {
        std::ops::apt::upgrade(hosts=hosts, sudo=sudo);
    }
    if packages.len() == 0 {
        return runtime_last_changed();
    }

    let mut install_args = Vec::new();
    install_args.push("install");
    if no_recommends {
        install_args.push("--no-install-recommends");
    }
    if allow_downgrades {
        install_args.push("--allow-downgrades");
    }
    let upgrade_command = noninteractive_apt(install_args.join(" "), force);
    if extra_install_args != "" {
        install_args.push(extra_install_args);
    }
    let install_command = noninteractive_apt(install_args.join(" "), force);

    let mut uninstall_args = Vec::new();
    uninstall_args.push("remove");
    if extra_uninstall_args != "" {
        uninstall_args.push(extra_uninstall_args);
    }
    let uninstall_command = noninteractive_apt(uninstall_args.join(" "), force);

    let mut current = std::facts::deb::packages();
    if current == null {
        current = HashMap::new();
    }
    let (to_install, to_upgrade) = ensure_packages(
        packages,
        current,
        present,
        install_command,
        uninstall_command,
        upgrade_command,
        latest,
    );

    if to_install.len() > 0 {
        let joined = to_install.join(" ");
        let command = match present {
            true => f"{install_command} {joined}",
            false => f"{uninstall_command} {joined}",
        };
        std::ops::server::shell(command, hosts=hosts, sudo=sudo);
    }
    if latest && to_upgrade.len() > 0 {
        let joined = to_upgrade.join(" ");
        std::ops::server::shell(f"{upgrade_command} {joined}", hosts=hosts, sudo=sudo);
    }
    runtime_last_changed()
}
