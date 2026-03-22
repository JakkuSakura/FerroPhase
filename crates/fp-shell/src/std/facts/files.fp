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

pub const fn read_file(path: str) -> str {
    std::shell::process::output(f"cat {path}")
}

pub const fn sha1(path: str) -> str {
    std::shell::process::output(f"sha1sum {path} | awk '{{print $1}}'")
}

pub const fn sha256(path: str) -> str {
    std::shell::process::output(f"sha256sum {path} | awk '{{print $1}}'")
}

pub const fn md5(path: str) -> str {
    std::shell::process::output(f"md5sum {path} | awk '{{print $1}}'")
}

pub const fn mode(path: str) -> str {
    std::shell::process::output(f"stat -c %a {path}")
}

pub const fn owner_user(path: str) -> str {
    std::shell::process::output(f"stat -c %U {path}")
}

pub const fn owner_group(path: str) -> str {
    std::shell::process::output(f"stat -c %G {path}")
}

pub const fn size(path: str) -> str {
    std::shell::process::output(f"stat -c %s {path}")
}
