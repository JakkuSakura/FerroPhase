#[cfg(target_lang = "bash")]
#[command = "command -v"]
extern "bash" fn command_available(command: str) -> bool;
#[cfg(target_lang = "pwsh")]
#[command = "Get-Command"]
extern "pwsh" fn command_available(command: str) -> bool;

#[cfg(target_lang = "bash")]
#[command = "test -f"]
extern "bash" fn file_exists_native(path: str) -> bool;
#[cfg(target_lang = "pwsh")]
#[command = "Test-Path -PathType Leaf"]
extern "pwsh" fn file_exists_native(path: str) -> bool;

#[cfg(target_lang = "bash")]
#[command = "test -d"]
extern "bash" fn dir_exists_native(path: str) -> bool;
#[cfg(target_lang = "pwsh")]
#[command = "Test-Path -PathType Container"]
extern "pwsh" fn dir_exists_native(path: str) -> bool;

#[cfg(target_lang = "bash")]
#[command = "test -e"]
extern "bash" fn path_exists_native(path: str) -> bool;
#[cfg(target_lang = "pwsh")]
#[command = "Test-Path"]
extern "pwsh" fn path_exists_native(path: str) -> bool;

pub const fn has_command(command: str) -> bool {
    command_available(command)
}

pub const fn file_exists(path: str) -> bool {
    file_exists_native(path)
}

pub const fn dir_exists(path: str) -> bool {
    dir_exists_native(path)
}

pub const fn path_exists(path: str) -> bool {
    path_exists_native(path)
}

pub const fn host_transport(host: str) -> str {
    std::hosts::transport(host)
}

pub const fn host_address(host: str) -> str {
    std::hosts::address(host)
}

pub const fn host_user(host: str) -> str {
    std::hosts::user(host)
}

pub const fn host_port(host: str) -> str {
    std::hosts::port(host)
}
