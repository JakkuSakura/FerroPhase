type RawProcessResult = {
    stdout: str,
    stderr: str,
    status: i64,
};

pub struct ProcessResult {
    stdout: str,
    stderr: str,
    status: i64,
}

impl ProcessResult {
    pub fn success(&self) -> bool {
        self.status == 0
    }

    pub fn status(&self) -> i64 {
        self.status
    }

    pub fn stdout(&self) -> &str {
        &self.stdout
    }

    pub fn stderr(&self) -> &str {
        &self.stderr
    }

    pub fn into_stdout(self) -> str {
        self.stdout
    }

    pub fn into_stderr(self) -> str {
        self.stderr
    }
}

pub fn exec(program: &str, args: Vec<&str>, cwd: &str) -> ProcessResult {
    let raw = intrinsic_process_exec(program, args, cwd);
    ProcessResult {
        stdout: raw.stdout,
        stderr: raw.stderr,
        status: raw.status,
    }
}

pub fn shell(command: str) -> ProcessResult {
    let raw = intrinsic_process_shell(command);
    ProcessResult {
        stdout: raw.stdout,
        stderr: raw.stderr,
        status: raw.status,
    }
}

#[lang = "libc_process_exec"]
fn intrinsic_process_exec(program: &str, args: Vec<&str>, cwd: &str) -> RawProcessResult {
    compile_error!("compiler intrinsic")
}

#[lang = "libc_process_shell"]
fn intrinsic_process_shell(command: str) -> RawProcessResult {
    compile_error!("compiler intrinsic")
}
