pub const fn copy(
    src: str,
    dest: str,
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    shell_copy(hosts, src, dest, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn copy_local(
    src: str,
    dest: str,
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    shell_copy_local(hosts, src, dest, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn copy_ssh(
    src: str,
    dest: str,
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    shell_copy_ssh(hosts, src, dest, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn copy_docker(
    src: str,
    dest: str,
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    shell_copy_docker(hosts, src, dest, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn copy_kubectl(
    src: str,
    dest: str,
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    shell_copy_kubectl(hosts, src, dest, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn copy_winrm(
    src: str,
    dest: str,
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    shell_copy_winrm(hosts, src, dest, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn copy_chroot(
    src: str,
    dest: str,
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    shell_copy_chroot(hosts, src, dest, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn template(
    src: str,
    dest: str,
    context hosts: str = "localhost",
    vars: str,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    shell_template(hosts, src, dest, vars, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn template_local(
    src: str,
    dest: str,
    context hosts: str = "localhost",
    vars: str,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    shell_template_local(hosts, src, dest, vars, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn template_ssh(
    src: str,
    dest: str,
    context hosts: str = "localhost",
    vars: str,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    shell_template_ssh(hosts, src, dest, vars, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn template_chroot(
    src: str,
    dest: str,
    context hosts: str = "localhost",
    vars: str,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    shell_template_chroot(hosts, src, dest, vars, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn rsync(
    src: str,
    dest: str,
    context hosts: str = "localhost",
    archive: bool = true,
    compress: bool = true,
    delete: bool = false,
    checksum: bool = false,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    let flags = rsync_flag_string(archive, compress, delete, checksum);
    shell_rsync(hosts, flags, src, dest, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn rsync_local(
    src: str,
    dest: str,
    context hosts: str = "localhost",
    archive: bool = true,
    compress: bool = true,
    delete: bool = false,
    checksum: bool = false,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    let flags = rsync_flag_string(archive, compress, delete, checksum);
    shell_rsync_local(hosts, flags, src, dest, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn rsync_remote(
    src: str,
    dest: str,
    context hosts: str = "localhost",
    archive: bool = true,
    compress: bool = true,
    delete: bool = false,
    checksum: bool = false,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    let flags = rsync_flag_string(archive, compress, delete, checksum);
    shell_rsync_remote(hosts, flags, src, dest, only_if, unless, creates, removes);
    runtime_last_changed()
}

pub const fn rsync_chroot(
    src: str,
    dest: str,
    context hosts: str = "localhost",
    archive: bool = true,
    compress: bool = true,
    delete: bool = false,
    checksum: bool = false,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    let flags = rsync_flag_string(archive, compress, delete, checksum);
    shell_rsync_chroot(hosts, flags, src, dest, only_if, unless, creates, removes);
    runtime_last_changed()
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

const fn adjust_regex(line: str, escape: bool) -> str {
    let rendered = match escape {
        true => escape_regex(line),
        false => line,
    };
    f"^.*{rendered}.*$"
}

const fn block_marker(marker: str, mark: str) -> str {
    let template = match marker {
        "" => "# {mark} PYINFRA BLOCK",
        _ => marker,
    };
    template.replace("{mark}", mark)
}

const fn block_marker_key(value: str) -> str {
    match value {
        "" => "None",
        _ => value,
    }
}

const fn block_out_prep(path: str, include_mode: bool, backup: bool) -> str {
    let tmp_dir = "_tempdir_";
    let mut out_prep = f"OUT=\"$(TMPDIR={tmp_dir} mktemp -t pyinfra.XXXXXX)\" && ";
    if include_mode {
        out_prep = f"{out_prep} MODE=\"$(stat -c %a {path} 2>/dev/null || stat -f %Lp {path} 2>/dev/null)\" &&";
    }
    out_prep = f"{out_prep} OWNER=\"$(stat -c \"%u:%g\" {path} 2>/dev/null || stat -f \"%u:%g\" {path} 2>/dev/null || echo $(id -un):$(id -gn))\" &&";
    match backup {
        true => f"cp {path} {path}.a-timestamp && {out_prep}",
        false => out_prep,
    }
}

const fn block_real_out(path: str, include_mode: bool) -> str {
    match include_mode {
        true => f" && mv \"$OUT\" {path}  && chown \"$OWNER\" {path}  && chmod \"$MODE\" {path}",
        false => f" && mv \"$OUT\" {path}  && chown \"$OWNER\" {path}  {path}",
    }
}

pub const fn directory(
    path: str,
    context hosts: str = "localhost",
    present: bool = true,
    user: str = "",
    group: str = "",
    mode: i64 = -1,
    recursive: bool = false,
    force: bool = false,
    force_backup: bool = true,
    force_backup_dir: str = "",
    _no_check_owner_mode: bool = false,
    _no_fail_on_link: bool = false,
    sudo: bool = true,
) -> bool {
    let mut info = std::facts::files::directory(path);
    if info == false {
        if _no_fail_on_link && std::facts::files::link(path) == true {
            return runtime_last_changed();
        }
        if !force {
            panic(f"{path} exists and is not a directory");
        }
        match force_backup {
            true => {
                let backup = match force_backup_dir {
                    "" => f"{path}.a-timestamp",
                    _ => f"{force_backup_dir}/{path}.a-timestamp",
                };
                std::ops::server::shell(f"mv {path} {backup}", hosts=hosts, sudo=sudo);
            }
            false => {
                std::ops::server::shell(f"rm -rf {path}", hosts=hosts, sudo=sudo);
            }
        }
        info = null;
    }

    match present {
        false => {
            if info != null && info != false {
                std::ops::server::shell(f"rm -rf {path}", hosts=hosts, sudo=sudo);
            }
            return runtime_last_changed();
        }
        true => {}
    }

    if info == null {
        std::ops::server::shell(f"mkdir -p {path}", hosts=hosts, sudo=sudo);
        if mode >= 0 {
            let flag = match recursive {
                true => "-R ",
                false => "",
            };
            std::ops::server::shell(f"chmod {flag}{mode} {path}", hosts=hosts, sudo=sudo);
        }
        if user != "" || group != "" {
            let flag = match recursive {
                true => "-R ",
                false => "",
            };
            let command = match (user != "", group != "") {
                (true, true) => f"chown {flag}{user}:{group} {path}",
                (true, false) => f"chown {flag}{user} {path}",
                (false, true) => f"chgrp {flag}{group} {path}",
                _ => "",
            };
            if command != "" {
                std::ops::server::shell(command, hosts=hosts, sudo=sudo);
            }
        }
        return runtime_last_changed();
    }

    if _no_check_owner_mode {
        return runtime_last_changed();
    }

    let mut changed = false;
    if mode >= 0 && info.get_unchecked("mode") != mode {
        let flag = match recursive {
            true => "-R ",
            false => "",
        };
        std::ops::server::shell(f"chmod {flag}{mode} {path}", hosts=hosts, sudo=sudo);
        changed = true;
    }
    if (user != "" && info.get_unchecked("user") != user)
        || (group != "" && info.get_unchecked("group") != group)
    {
        let flag = match recursive {
            true => "-R ",
            false => "",
        };
        let command = match (user != "", group != "") {
            (true, true) => f"chown {flag}{user}:{group} {path}",
            (true, false) => f"chown {flag}{user} {path}",
            (false, true) => f"chgrp {flag}{group} {path}",
            _ => "",
        };
        if command != "" {
            std::ops::server::shell(command, hosts=hosts, sudo=sudo);
            changed = true;
        }
    }
    if !changed {
        runtime_last_changed()
    } else {
        runtime_last_changed()
    }
}

pub const fn block(
    path: str,
    context hosts: str = "localhost",
    content: str = "",
    present: bool = true,
    line: str = "",
    backup: bool = false,
    escape_regex_characters: bool = false,
    try_prevent_shell_expansion: bool = false,
    before: bool = false,
    after: bool = false,
    marker: str = "",
    begin: str = "",
    end: str = "",
    sudo: bool = true,
) -> bool {
    let mark_1 = block_marker(marker, match begin { "" => "BEGIN", _ => begin });
    let mark_2 = block_marker(marker, match end { "" => "END", _ => end });

    let current = std::facts::files::block(
        path,
        block_marker_key(marker),
        block_marker_key(begin),
        block_marker_key(end),
    );

    if present {
        if content == "" {
            panic("'content' must be supplied when 'present' == True");
        }
        if line != "" && before == after {
            panic("only one of 'before' or 'after' used when 'line` is specified");
        }
        if line == "" && before != after {
            panic("'line' must be supplied or 'before' and 'after' must be equal");
        }

        let the_block = f"{mark_1}\n{content}\n{mark_2}";
        let block_literal = match try_prevent_shell_expansion {
            true => f"'{the_block}'",
            false => f"\"{the_block}\"",
        };
        let out_prep = block_out_prep(path, current != null, backup);
        let real_out = block_real_out(path, current != null);

        if current == null || (current == "" && before == after) {
            let here = "PYINFRAHERE";
            let original = match current == null {
                true => "/dev/null",
                false => path,
            };
            let first = match before {
                true => " - ",
                false => f"{original}",
            };
            let second = match before {
                true => f"{original}",
                false => " - ",
            };
            let command = f"{out_prep} ( awk '{{print}}' {first} {second} > \"$OUT\" <<{here}\n{the_block}\n{here}\n ) {real_out}";
            return std::ops::server::shell(command, hosts=hosts, sudo=sudo);
        }

        if current == "" {
            let regex = adjust_regex(line, escape_regex_characters);
            let print_before = match before {
                true => "{ print }",
                false => "",
            };
            let print_after = match after {
                true => "{ print }",
                false => "",
            };
            let prog =
                f"awk 'BEGIN {{x=ARGV[2]; ARGV[2]=\"\"}} {print_after} f!=1 && /{regex}/ {{ print x; f=1}} END {{if (f==0) print x }} {print_before}'";
            let command =
                f"{out_prep} {prog} {path} {block_literal} > \"$OUT\" {real_out}";
            return std::ops::server::shell(command, hosts=hosts, sudo=sudo);
        }

        if current != content {
            let prog = f"awk 'BEGIN {{f=1; x=ARGV[2]; ARGV[2]=\"\"}}/{mark_1}/ {{print; print x; f=0}} /{mark_2}/ {{print; f=1; next}} f'";
            let command =
                f"{out_prep} {prog} {path} \"{content}\" > \"$OUT\" {real_out}";
            return std::ops::server::shell(command, hosts=hosts, sudo=sudo);
        }
        return runtime_last_changed();
    }

    if current == null || current == "" {
        return runtime_last_changed();
    }
    let out_prep = block_out_prep(path, true, backup);
    let real_out = block_real_out(path, true);
    let command = f"{out_prep} awk '/{mark_1}/,/{mark_2}/ {{next}} 1' {path} > $OUT {real_out}";
    std::ops::server::shell(command, hosts=hosts, sudo=sudo)
}
