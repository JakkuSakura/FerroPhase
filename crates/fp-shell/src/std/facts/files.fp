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

pub const fn file(path: str) -> any {
    null
}

pub const fn directory(path: str) -> any {
    null
}

pub const fn block(path: str, marker: str, begin: str, end: str) -> any {
    null
}

pub const fn link(path: str) -> any {
    false
}

pub const fn find_in_file(path: str, pattern: str, interpolate_variables: bool) -> any {
    null
}
