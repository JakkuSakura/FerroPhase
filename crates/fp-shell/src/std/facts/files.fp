#[cfg(target_lang = "bash")]
#[command = "test -L"]
extern "bash" fn link_exists_native(path: str) -> bool;
#[cfg(target_lang = "pwsh")]
#[command = "Test-Path"]
extern "pwsh" fn link_exists_native(path: str) -> bool;

pub const fn exists(path: str) -> bool {
    std::facts::path_exists(path)
}

pub const fn is_file(path: str) -> bool {
    std::facts::file_exists(path)
}

pub const fn is_directory(path: str) -> bool {
    std::facts::dir_exists(path)
}

pub const fn is_link(path: str) -> bool {
    link_exists_native(path)
}

#[cfg(target_lang = "bash")]
pub const fn read_file(path: str) -> str {
    std::shell::process::output(f"cat {path}")
}

#[cfg(target_lang = "pwsh")]
pub const fn read_file(path: str) -> str {
    std::shell::process::output(f"Get-Content -Raw -LiteralPath \"{path}\"")
}

#[cfg(target_lang = "bash")]
pub const fn sha1(path: str) -> str {
    std::shell::process::output(f"sha1sum {path} | awk '{{print $1}}'")
}

#[cfg(target_lang = "pwsh")]
pub const fn sha1(path: str) -> str {
    std::shell::process::output(
        f"Get-FileHash -Algorithm SHA1 -LiteralPath \"{path}\" | Select -ExpandProperty Hash",
    )
}

#[cfg(target_lang = "bash")]
pub const fn sha256(path: str) -> str {
    std::shell::process::output(f"sha256sum {path} | awk '{{print $1}}'")
}

#[cfg(target_lang = "pwsh")]
pub const fn sha256(path: str) -> str {
    std::shell::process::output(
        f"Get-FileHash -Algorithm SHA256 -LiteralPath \"{path}\" | Select -ExpandProperty Hash",
    )
}

#[cfg(target_lang = "bash")]
pub const fn md5(path: str) -> str {
    std::shell::process::output(f"md5sum {path} | awk '{{print $1}}'")
}

#[cfg(target_lang = "pwsh")]
pub const fn md5(path: str) -> str {
    std::shell::process::output(
        f"Get-FileHash -Algorithm MD5 -LiteralPath \"{path}\" | Select -ExpandProperty Hash",
    )
}

#[cfg(target_lang = "bash")]
pub const fn mode(path: str) -> str {
    std::shell::process::output(f"stat -c %a {path}")
}

#[cfg(target_lang = "pwsh")]
pub const fn mode(path: str) -> str {
    std::shell::process::output(f"(Get-Item -LiteralPath \"{path}\").Mode")
}

#[cfg(target_lang = "bash")]
pub const fn owner_user(path: str) -> str {
    std::shell::process::output(f"stat -c %U {path}")
}

#[cfg(target_lang = "pwsh")]
pub const fn owner_user(path: str) -> str {
    std::shell::process::output(f"(Get-Acl -LiteralPath \"{path}\").Owner")
}

#[cfg(target_lang = "bash")]
pub const fn owner_group(path: str) -> str {
    std::shell::process::output(f"stat -c %G {path}")
}

#[cfg(target_lang = "pwsh")]
pub const fn owner_group(path: str) -> str {
    std::shell::process::output(f"(Get-Acl -LiteralPath \"{path}\").Group")
}

#[cfg(target_lang = "bash")]
pub const fn size(path: str) -> str {
    std::shell::process::output(f"stat -c %s {path}")
}

#[cfg(target_lang = "pwsh")]
pub const fn size(path: str) -> str {
    std::shell::process::output(f"(Get-Item -LiteralPath \"{path}\").Length")
}

const fn normalize_block_mark(value: str, default: str) -> str {
    match value {
        "" => default,
        "None" => default,
        _ => value,
    }
}

const fn block_marker(marker: str, mark: str) -> str {
    let template = match marker {
        "" => "# {mark} PYINFRA BLOCK",
        "None" => "# {mark} PYINFRA BLOCK",
        _ => marker,
    };
    template.replace("{mark}", mark)
}

const fn contains_regex_meta(value: str) -> bool {
    value.contains("\\")
        || value.contains(".")
        || value.contains("*")
        || value.contains("+")
        || value.contains("?")
        || value.contains("(")
        || value.contains(")")
        || value.contains("[")
        || value.contains("]")
        || value.contains("{")
        || value.contains("}")
        || value.contains("|")
        || value.contains("^")
        || value.contains("$")
}

const fn escape_bash_single_quotes(value: str) -> str {
    value.replace("'", "'\"'\"'")
}

const fn escape_pwsh_single_quotes(value: str) -> str {
    value.replace("'", "''")
}

#[cfg(target_lang = "bash")]
const fn stat_mode(path: str) -> str {
    std::shell::process::output(f"stat -c %a {path} 2>/dev/null || stat -f %Lp {path}")
}

#[cfg(target_lang = "pwsh")]
const fn stat_mode(path: str) -> str {
    mode(path)
}

#[cfg(target_lang = "bash")]
const fn stat_user(path: str) -> str {
    std::shell::process::output(f"stat -c %U {path} 2>/dev/null || stat -f %Su {path}")
}

#[cfg(target_lang = "pwsh")]
const fn stat_user(path: str) -> str {
    owner_user(path)
}

#[cfg(target_lang = "bash")]
const fn stat_group(path: str) -> str {
    std::shell::process::output(f"stat -c %G {path} 2>/dev/null || stat -f %Sg {path}")
}

#[cfg(target_lang = "pwsh")]
const fn stat_group(path: str) -> str {
    owner_group(path)
}

#[cfg(target_lang = "bash")]
const fn stat_size(path: str) -> str {
    std::shell::process::output(f"stat -c %s {path} 2>/dev/null || stat -f %z {path}")
}

#[cfg(target_lang = "pwsh")]
const fn stat_size(path: str) -> str {
    size(path)
}

#[cfg(target_lang = "bash")]
const fn stat_mtime(path: str) -> str {
    std::shell::process::output(
        f"epoch=\"$(stat -c %Y {path} 2>/dev/null || stat -f %m {path})\"; date -d \"@${{epoch}}\" +%Y-%m-%dT%H:%M:%S%z 2>/dev/null || date -r \"${{epoch}}\" +%Y-%m-%dT%H:%M:%S%z",
    )
}

#[cfg(target_lang = "pwsh")]
const fn stat_mtime(path: str) -> str {
    std::shell::process::output(
        f"(Get-Item -LiteralPath \"{path}\").LastWriteTime.ToString(\"o\")",
    )
}

#[cfg(target_lang = "bash")]
const fn interpolate_pattern(pattern: str, interpolate_variables: bool) -> str {
    if !interpolate_variables {
        return pattern;
    }
    if !std::shell::process::ok("command -v envsubst >/dev/null 2>&1") {
        return pattern;
    }
    let escaped = escape_bash_single_quotes(pattern);
    std::shell::process::output(f"printf %s '{escaped}' | envsubst")
}

#[cfg(target_lang = "pwsh")]
const fn interpolate_pattern(pattern: str, interpolate_variables: bool) -> str {
    if !interpolate_variables {
        return pattern;
    }
    let escaped = escape_pwsh_single_quotes(pattern);
    // Only expands $VAR and ${VAR} via environment lookup; no eval or subexpression support.
    std::shell::process::output(
        f"$text = '{escaped}'; $text = [regex]::Replace($text, '\\$\\{{([A-Za-z_][A-Za-z0-9_]*)\\}}', {{ param($m) [Environment]::GetEnvironmentVariable($m.Groups[1].Value) }}); $text = [regex]::Replace($text, '\\$([A-Za-z_][A-Za-z0-9_]*)', {{ param($m) [Environment]::GetEnvironmentVariable($m.Groups[1].Value) }}); $text",
    )
}

#[cfg(target_lang = "bash")]
const fn find_in_file_native(path: str, pattern: str, use_regex: bool) -> bool {
    let flag = match use_regex {
        true => "-E",
        false => "-F",
    };
    let escaped_pattern = escape_bash_single_quotes(pattern);
    let escaped_path = escape_bash_single_quotes(path);
    std::shell::process::ok(f"grep {flag} -q -- '{escaped_pattern}' '{escaped_path}'")
}

#[cfg(target_lang = "pwsh")]
const fn find_in_file_native(path: str, pattern: str, use_regex: bool) -> bool {
    let escaped_pattern = escape_pwsh_single_quotes(pattern);
    let escaped_path = escape_pwsh_single_quotes(path);
    match use_regex {
        true => std::shell::process::ok(
            f"if (Select-String -LiteralPath '{escaped_path}' -Pattern '{escaped_pattern}' -Quiet) {{ exit 0 }} else {{ exit 1 }}",
        ),
        false => std::shell::process::ok(
            f"if (Select-String -LiteralPath '{escaped_path}' -Pattern '{escaped_pattern}' -SimpleMatch -Quiet) {{ exit 0 }} else {{ exit 1 }}",
        ),
    }
}

pub const fn file(path: str) -> any {
    if !exists(path) {
        return null;
    }
    if !is_file(path) {
        return false;
    }
    let mut info = HashMap::new();
    info.insert("mtime", stat_mtime(path));
    info.insert("size", stat_size(path));
    info.insert("mode", stat_mode(path));
    info.insert("user", stat_user(path));
    info.insert("group", stat_group(path));
    info
}

pub const fn directory(path: str) -> any {
    if !exists(path) {
        return null;
    }
    if !is_directory(path) {
        return false;
    }
    let mut info = HashMap::new();
    info.insert("mode", stat_mode(path));
    info.insert("user", stat_user(path));
    info.insert("group", stat_group(path));
    info
}

pub const fn block(path: str, marker: str, begin: str, end: str) -> any {
    if !exists(path) {
        return null;
    }
    if !is_file(path) {
        return false;
    }
    let begin_mark = normalize_block_mark(begin, "BEGIN");
    let end_mark = normalize_block_mark(end, "END");
    let marker_value = normalize_block_mark(marker, "");
    let start_line = block_marker(marker_value, begin_mark);
    let end_line = block_marker(marker_value, end_mark);
    let content = read_file(path);
    let lines = content.split("\n");
    let mut idx = 0;
    let mut in_block = false;
    let mut found = false;
    let mut out_lines = Vec::new();
    while idx < lines.len() {
        let line = lines[idx];
        if !in_block {
            if line == start_line {
                in_block = true;
                found = true;
            }
        } else {
            if line == end_line {
                return out_lines.join("\n");
            }
            out_lines.push(line);
        }
        idx = idx + 1;
    }
    if found {
        ""
    } else {
        ""
    }
}

#[cfg(target_lang = "bash")]
pub const fn link(path: str) -> any {
    std::shell::process::ok(f"test -L {path}")
}

#[cfg(target_lang = "pwsh")]
pub const fn link(path: str) -> any {
    std::shell::process::ok(
        f"if ((Get-Item -LiteralPath \"{path}\" -ErrorAction SilentlyContinue).LinkType -eq 'SymbolicLink') {{ exit 0 }} else {{ exit 1 }}",
    )
}

pub const fn find_in_file(path: str, pattern: str, interpolate_variables: bool) -> any {
    if !exists(path) || !is_file(path) {
        return false;
    }
    let rendered = interpolate_pattern(pattern, interpolate_variables);
    let use_regex = contains_regex_meta(rendered);
    find_in_file_native(path, rendered, use_regex)
}
use std::collections::hash_map::HashMap;
