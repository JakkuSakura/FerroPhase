#[cfg(unix)]
#[command = "sh -lc"]
extern "cli" fn shell_run(command: str);

#[cfg(windows)]
#[command = "pwsh -Command"]
extern "cli" fn shell_run(command: str);

#[cfg(unix)]
#[command = "sh -lc"]
extern "cli" fn shell_ok(command: str) -> bool;

#[cfg(windows)]
#[command = "pwsh -Command"]
extern "cli" fn shell_ok(command: str) -> bool;

#[cfg(unix)]
#[command = "sh -lc"]
extern "cli" fn shell_output(command: str) -> str;

#[cfg(windows)]
#[command = "pwsh -Command"]
extern "cli" fn shell_output(command: str) -> str;

#[cfg(unix)]
#[command = "sh -lc"]
extern "cli" fn shell_status_code(command: str) -> i64;

#[cfg(windows)]
#[command = "pwsh -Command"]
extern "cli" fn shell_status_code(command: str) -> i64;

pub fn run(command: str) {
    shell_run(command)
}

pub fn ok(command: str) -> bool {
    shell_ok(command)
}

pub fn output(command: str) -> str {
    shell_output(command)
}

pub fn status(command: str) -> i64 {
    shell_status_code(command)
}
