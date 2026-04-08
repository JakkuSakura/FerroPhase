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

const fn record_change(op: str, target: str, summary: str, changed: bool) -> bool {
    runtime_record_change(op, target, summary, changed);
    changed
}

const fn line_regex_exact(line: str, escape: bool) -> str {
    let rendered = match escape {
        true => escape_regex(line),
        false => line,
    };
    f"^{rendered}$"
}

#[cfg(target_lang = "bash")]
const fn regex_present(path: str, regex: str) -> bool {
    std::shell::process::ok(f"grep -Eq \"{regex}\" \"{path}\"")
}

#[cfg(target_lang = "pwsh")]
const fn regex_present(path: str, regex: str) -> bool {
    std::shell::process::ok(f"Select-String -Quiet -Pattern \"{regex}\" -LiteralPath \"{path}\"")
}

#[cfg(target_lang = "bash")]
const fn file_write_command(path: str, content: str) -> str {
    let marker = "PYINFRAHERE";
    f"cat <<'{marker}' > \"{path}\"\n{content}\n{marker}\n"
}

#[cfg(target_lang = "pwsh")]
const fn file_write_command(path: str, content: str) -> str {
    f"$content = @'\n{content}\n'@; Set-Content -LiteralPath \"{path}\" -Value $content"
}

#[cfg(target_lang = "bash")]
const fn file_append_command(path: str, content: str) -> str {
    let marker = "PYINFRAHERE";
    f"cat <<'{marker}' >> \"{path}\"\n{content}\n{marker}\n"
}

#[cfg(target_lang = "pwsh")]
const fn file_append_command(path: str, content: str) -> str {
    f"Add-Content -LiteralPath \"{path}\" -Value @'\n{content}\n'@"
}

#[cfg(target_lang = "bash")]
const fn file_touch_command(path: str) -> str {
    f"touch \"{path}\""
}

#[cfg(target_lang = "pwsh")]
const fn file_touch_command(path: str) -> str {
    f"New-Item -ItemType File -Path \"{path}\" -Force | Out-Null"
}

#[cfg(target_lang = "bash")]
const fn file_remove_command(path: str) -> str {
    f"rm -f \"{path}\""
}

#[cfg(target_lang = "pwsh")]
const fn file_remove_command(path: str) -> str {
    f"Remove-Item -LiteralPath \"{path}\" -Force -ErrorAction SilentlyContinue"
}

#[cfg(target_lang = "bash")]
const fn replace_command(path: str, regex: str, replacement: str) -> str {
    f"TMPFILE=\"$(mktemp)\" && sed -E 's/{regex}/{replacement}/g' \"{path}\" > \"$TMPFILE\" && mv \"$TMPFILE\" \"{path}\""
}

#[cfg(target_lang = "pwsh")]
const fn replace_command(path: str, regex: str, replacement: str) -> str {
    f"$content = Get-Content -Raw -LiteralPath \"{path}\"; $content = $content -replace \"{regex}\", \"{replacement}\"; Set-Content -LiteralPath \"{path}\" -Value $content"
}

#[cfg(target_lang = "bash")]
const fn replace_remove_command(path: str, regex: str) -> str {
    f"TMPFILE=\"$(mktemp)\" && sed -E '/{regex}/d' \"{path}\" > \"$TMPFILE\" && mv \"$TMPFILE\" \"{path}\""
}

#[cfg(target_lang = "pwsh")]
const fn replace_remove_command(path: str, regex: str) -> str {
    f"$content = Get-Content -Raw -LiteralPath \"{path}\"; $content = $content -replace \"{regex}\", \"\"; Set-Content -LiteralPath \"{path}\" -Value $content"
}

#[cfg(target_lang = "bash")]
const fn line_remove_command(path: str, regex: str) -> str {
    f"TMPFILE=\"$(mktemp)\" && sed -E '/{regex}/d' \"{path}\" > \"$TMPFILE\" && mv \"$TMPFILE\" \"{path}\""
}

#[cfg(target_lang = "pwsh")]
const fn line_remove_command(path: str, regex: str) -> str {
    f"$content = Get-Content -Raw -LiteralPath \"{path}\"; $content = $content -replace \"(?m)^.*{regex}.*\\\\r?\\\\n?\", \"\"; Set-Content -LiteralPath \"{path}\" -Value $content"
}

#[cfg(target_lang = "bash")]
const fn link_create_command(path: str, target: str) -> str {
    f"ln -sfn \"{target}\" \"{path}\""
}

#[cfg(target_lang = "pwsh")]
const fn link_create_command(path: str, target: str) -> str {
    f"New-Item -ItemType SymbolicLink -Path \"{path}\" -Target \"{target}\" -Force | Out-Null"
}

#[cfg(target_lang = "bash")]
const fn download_command(url: str, dest: str) -> str {
    f"command -v curl >/dev/null 2>&1 && curl -fsSL -o \"{dest}\" \"{url}\" || wget -O \"{dest}\" \"{url}\""
}

#[cfg(target_lang = "pwsh")]
const fn download_command(url: str, dest: str) -> str {
    f"$ProgressPreference = \"SilentlyContinue\"; Invoke-WebRequest -Uri \"{url}\" -OutFile \"{dest}\""
}

const fn assert_path_like(path: any, label: str) {
    if type_name!(path) != "String" {
        panic(f"`{label}` must be a string or `os.PathLike` object");
    }
}

const fn path_basename(path: str) -> str {
    let parts = path.split("/");
    if parts.len() == 0 {
        return path;
    }
    let mut idx = parts.len() - 1;
    while idx > 0 && parts[idx] == "" {
        idx = idx - 1;
    }
    parts[idx]
}

pub const fn directory(
    path: any,
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
    assert_path_like(path, "path");
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
    line: any = "",
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
        let line_type = type_name!(line);
        let line_has_regex = line_type.replace("Regex", "") != line_type;
        if line_type != "String" && !line_has_regex {
            panic("'line' must be a regex or a string");
        }
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

pub const fn file(
    path: any,
    context hosts: str = "localhost",
    present: bool = true,
    content: str = "",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
    sudo: bool = true,
) -> bool {
    assert_path_like(path, "path");
    let mut changed = false;
    if present {
        let info = std::facts::files::file(path);
        if info == false {
            panic(f"{path} exists and is not a file");
        }
    }
    if !present {
        if std::facts::files::is_file(path) {
            std::ops::server::shell(
                file_remove_command(path),
                hosts=hosts,
                sudo=sudo,
                only_if=only_if,
                unless=unless,
                creates=creates,
                removes=removes,
            );
            changed = runtime_last_changed();
        }
        return record_change("file", path, "absent", changed);
    }

    if content != "" {
        let mut matches = false;
        if std::facts::files::is_file(path) {
            let current = std::facts::files::read_file(path);
            matches = current == content;
        }
        if !matches {
            std::ops::server::shell(
                file_write_command(path, content),
                hosts=hosts,
                sudo=sudo,
                only_if=only_if,
                unless=unless,
                creates=creates,
                removes=removes,
            );
            changed = runtime_last_changed();
        }
        return record_change("file", path, "content", changed);
    }

    if !std::facts::files::is_file(path) {
        std::ops::server::shell(
            file_touch_command(path),
            hosts=hosts,
            sudo=sudo,
            only_if=only_if,
            unless=unless,
            creates=creates,
            removes=removes,
        );
        changed = runtime_last_changed();
    }
    record_change("file", path, "present", changed)
}

pub const fn line(
    path: str,
    line: str,
    context hosts: str = "localhost",
    present: bool = true,
    replace: str = "",
    append: bool = true,
    escape_regex_characters: bool = false,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
    sudo: bool = true,
) -> bool {
    let mut changed = false;
    let match_regex = match replace {
        "" => line_regex_exact(line, escape_regex_characters),
        _ => replace,
    };

    if present {
        if std::facts::files::is_file(path) {
            let found = regex_present(path, match_regex);
            if found {
                if replace != "" {
                    std::ops::server::shell(
                        replace_command(path, replace, line),
                        hosts=hosts,
                        sudo=sudo,
                        only_if=only_if,
                        unless=unless,
                        creates=creates,
                        removes=removes,
                    );
                    changed = runtime_last_changed();
                }
            } else if append {
                std::ops::server::shell(
                    file_append_command(path, line),
                    hosts=hosts,
                    sudo=sudo,
                    only_if=only_if,
                    unless=unless,
                    creates=creates,
                    removes=removes,
                );
                changed = runtime_last_changed();
            }
        } else if append {
            std::ops::server::shell(
                file_write_command(path, line),
                hosts=hosts,
                sudo=sudo,
                only_if=only_if,
                unless=unless,
                creates=creates,
                removes=removes,
            );
            changed = runtime_last_changed();
        }
        let summary = match replace != "" {
            true => "replace",
            false => "append",
        };
        return record_change("line", path, summary, changed);
    }

    if std::facts::files::is_file(path) && regex_present(path, match_regex) {
        std::ops::server::shell(
            line_remove_command(path, match_regex),
            hosts=hosts,
            sudo=sudo,
            only_if=only_if,
            unless=unless,
            creates=creates,
            removes=removes,
        );
        changed = runtime_last_changed();
    }
    record_change("line", path, "absent", changed)
}

pub const fn replace(
    path: str,
    regex: str,
    replace: str = "",
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
    sudo: bool = true,
) -> bool {
    let mut changed = false;
    if std::facts::files::is_file(path) && regex_present(path, regex) {
        let command = match replace {
            "" => replace_remove_command(path, regex),
            _ => replace_command(path, regex, replace),
        };
        std::ops::server::shell(
            command,
            hosts=hosts,
            sudo=sudo,
            only_if=only_if,
            unless=unless,
            creates=creates,
            removes=removes,
        );
        changed = runtime_last_changed();
    }
    let summary = match replace {
        "" => "remove",
        _ => "replace",
    };
    record_change("replace", path, summary, changed)
}

pub const fn move(
    src: any,
    dest: any,
    context hosts: str = "localhost",
    overwrite: bool = false,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
    sudo: bool = true,
) -> bool {
    assert_path_like(src, "src");
    assert_path_like(dest, "dest");

    let file_info = std::facts::files::file(src);
    let dir_info = std::facts::files::directory(src);
    let link_info = std::facts::files::link(src);
    if file_info == null && dir_info == null && link_info == null {
        panic(f"src {src} does not exist");
    }

    let dest_info = std::facts::files::directory(dest);
    if dest_info == null || dest_info == false {
        panic(f"dest {dest} is not an existing directory");
    }

    let dest_path = f"{dest}/{path_basename(src)}";
    let dest_file = std::facts::files::file(dest_path);
    if dest_file != null && dest_file != false {
        if !overwrite {
            panic(f"dest {dest_path} already exists and `overwrite` is unset");
        }
        std::ops::server::shell(
            f"rm -rf {dest_path}",
            hosts=hosts,
            sudo=sudo,
            only_if=only_if,
            unless=unless,
            creates=creates,
            removes=removes,
        );
    }

    std::ops::server::shell(
        f"mv {src} {dest}",
        hosts=hosts,
        sudo=sudo,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    );
    let changed = runtime_last_changed();
    record_change("move", src, "moved", changed)
}

pub const fn link(
    path: any,
    target: any = "",
    context hosts: str = "localhost",
    present: bool = true,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
    sudo: bool = true,
) -> bool {
    assert_path_like(path, "path");
    let info = std::facts::files::link(path);
    let mut changed = false;
    if present {
        if target == "" {
            panic("If present is True target must be provided");
        }
        assert_path_like(target, "target");
        if info == false {
            panic(f"{path} exists and is not a link");
        }
        if info == null {
            std::ops::server::shell(
                link_create_command(path, target),
                hosts=hosts,
                sudo=sudo,
                only_if=only_if,
                unless=unless,
                creates=creates,
                removes=removes,
            );
            changed = runtime_last_changed();
        }
        return record_change("link", path, "present", changed);
    }

    if info != null && info != false {
        std::ops::server::shell(
            file_remove_command(path),
            hosts=hosts,
            sudo=sudo,
            only_if=only_if,
            unless=unless,
            creates=creates,
            removes=removes,
        );
        changed = runtime_last_changed();
    }
    record_change("link", path, "absent", changed)
}

pub const fn download(
    url: str,
    dest: any,
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
    sudo: bool = false,
) -> bool {
    assert_path_like(dest, "dest");
    if std::facts::files::file(dest) == false {
        panic(f"Destination {dest} already exists and is not a file");
    }
    std::ops::server::shell(
        download_command(url, dest),
        hosts=hosts,
        sudo=sudo,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    );
    let changed = runtime_last_changed();
    record_change("download", dest, "fetched", changed)
}
