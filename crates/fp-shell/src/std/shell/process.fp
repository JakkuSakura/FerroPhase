pub mod process {
    #[cfg(target_lang = "bash")]
    #[command = "bash -lc"]
    extern "bash" fn shell_status(command: str) -> bool;
    #[cfg(target_lang = "pwsh")]
    #[command = "pwsh -Command"]
    extern "pwsh" fn shell_status(command: str) -> bool;
    #[cfg(target_lang = "bash")]
    #[command = "bash -lc"]
    extern "bash" fn shell_output(command: str) -> str;
    #[cfg(target_lang = "pwsh")]
    #[command = "pwsh -Command"]
    extern "pwsh" fn shell_output(command: str) -> str;

    pub const fn raw(text: str) -> str {
        text
    }

    pub const fn pipe(lhs: str, rhs: str) -> str {
        f"{lhs} | {rhs}"
    }

    pub const fn stdout_to(command: str, path: str) -> str {
        f"{command} > {path}"
    }

    pub const fn stdout_append(command: str, path: str) -> str {
        f"{command} >> {path}"
    }

    pub const fn stderr_to(command: str, path: str) -> str {
        f"{command} 2> {path}"
    }

    pub const fn stderr_append(command: str, path: str) -> str {
        f"{command} 2>> {path}"
    }

    pub const fn run(command: str) {
        std::ops::server::shell(command);
    }

    pub const fn ok(command: str) -> bool {
        shell_status(command)
    }

    pub const fn output(command: str) -> str {
        shell_output(command)
    }
}
