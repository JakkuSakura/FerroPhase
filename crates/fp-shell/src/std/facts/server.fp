#[cfg(target_lang = "bash")]
#[command = "whoami"]
extern "bash" fn current_user_native() -> str;
#[cfg(target_lang = "pwsh")]
#[command = "whoami"]
extern "pwsh" fn current_user_native() -> str;

#[cfg(target_lang = "bash")]
#[command = "bash -lc"]
extern "bash" fn user_home_native(user: str) -> str;
#[cfg(target_lang = "pwsh")]
#[command = "pwsh -Command"]
extern "pwsh" fn user_home_native(user: str) -> str;

#[cfg(target_lang = "bash")]
#[command = "printenv PATH"]
extern "bash" fn path_native() -> str;
#[cfg(target_lang = "pwsh")]
#[command = "$env:PATH"]
extern "pwsh" fn path_native() -> str;

#[cfg(target_lang = "bash")]
#[command = "uname -n"]
extern "bash" fn hostname_native() -> str;
#[cfg(target_lang = "pwsh")]
#[command = "hostname"]
extern "pwsh" fn hostname_native() -> str;

#[cfg(target_lang = "bash")]
#[command = "uname -s"]
extern "bash" fn kernel_native() -> str;
#[cfg(target_lang = "pwsh")]
#[command = "$PSVersionTable.Platform"]
extern "pwsh" fn kernel_native() -> str;

#[cfg(target_lang = "bash")]
#[command = "uname -r"]
extern "bash" fn kernel_version_native() -> str;
#[cfg(target_lang = "pwsh")]
#[command = "$PSVersionTable.PSVersion.ToString()"]
extern "pwsh" fn kernel_version_native() -> str;

#[cfg(target_lang = "bash")]
#[command = "uname -m"]
extern "bash" fn arch_native() -> str;
#[cfg(target_lang = "pwsh")]
#[command = "$env:PROCESSOR_ARCHITECTURE"]
extern "pwsh" fn arch_native() -> str;

#[cfg(target_lang = "bash")]
#[command = "date +%Y-%m-%dT%H:%M:%S%z"]
extern "bash" fn date_native() -> str;
#[cfg(target_lang = "pwsh")]
#[command = "Get-Date -Format o"]
extern "pwsh" fn date_native() -> str;

pub const fn current_user() -> str {
    current_user_native()
}

pub const fn home() -> str {
    user_home("")
}

pub const fn user_home(user: str) -> str {
    if user == "" {
        user_home_native("echo $HOME")
    } else {
        user_home_native(f"echo ~{user}")
    }
}

pub const fn path() -> str {
    path_native()
}

pub const fn hostname() -> str {
    hostname_native()
}

pub const fn kernel() -> str {
    kernel_native()
}

pub const fn kernel_version() -> str {
    kernel_version_native()
}

pub const fn arch() -> str {
    arch_native()
}

pub const fn date() -> str {
    date_native()
}

pub const fn command(command: str) -> str {
    std::shell::process::output(command)
}

#[cfg(target_lang = "bash")]
pub const fn which(command: str) -> str {
    std::shell::process::output(f"command -v {command}")
}

#[cfg(target_lang = "pwsh")]
pub const fn which(command: str) -> str {
    std::shell::process::output(
        f"Get-Command -Name \"{command}\" | Select -ExpandProperty Source",
    )
}
