use std::option::Option;

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

pub struct Process {
    shell_command: str,
    program: str,
    args: Vec<&str>,
    cwd: str,
}

impl Process {
    pub fn new(program: &str) -> Process {
        Process {
            shell_command: "",
            program,
            args: Vec::new(),
            cwd: "",
        }
    }

    pub fn shell(command: str) -> Process {
        Process {
            shell_command: command,
            program: "",
            args: Vec::new(),
            cwd: "",
        }
    }

    pub fn arg(self, arg: &str) -> Process {
        let mut args = self.args;
        args.push(arg);

        Process {
            shell_command: self.shell_command,
            program: self.program,
            args,
            cwd: self.cwd,
        }
    }

    pub fn args(self, extra_args: Vec<&str>) -> Process {
        let mut process = self;
        let mut idx = 0;
        let extra_len = extra_args.len();
        while idx < extra_len {
            process = process.arg(extra_args[idx]);
            idx = idx + 1;
        }
        process
    }

    pub fn current_dir(self, cwd: &str) -> Process {
        Process {
            shell_command: self.shell_command,
            program: self.program,
            args: self.args,
            cwd,
        }
    }

    pub fn run(self) {
        let result = exec_command(self);
        if !result.success() {
            panic(f"process exited with status {result.status()}: {result.stderr()}");
        }

        let stdout = result.into_stdout();
        if stdout != "" {
            std::io::write_stdout(&stdout);
        }
    }

    pub fn ok(self) -> bool {
        self.output_result().success()
    }

    pub fn output(self) -> str {
        let result = self.output_result();
        if !result.success() {
            panic(f"process exited with status {result.status()}: {result.stderr()}");
        }

        result.into_stdout()
    }

    pub fn status(self) -> i64 {
        self.output_result().status()
    }

    pub fn output_result(self) -> ProcessResult {
        let rendered_command = render_process_command(self);
        let mocked: Option<std::test::CommandMockMatch> =
            std::test::apply_command_mock(&rendered_command);
        match mocked {
            Option::Some(mocked) => ProcessResult {
                stdout: mocked.stdout,
                stderr: mocked.stderr,
                status: mocked.status,
            },
            Option::None => ProcessResult {
                stdout: "",
                stderr: "",
                status: decode_exit_status(std::libc::system(&rendered_command)),
            },
        }
    }
}

pub struct Command {
    inner: Process,
}

impl Command {
    pub fn new(program: &str) -> Command {
        Command {
            inner: Process::new(program),
        }
    }

    pub fn shell(command: str) -> Command {
        Command {
            inner: Process::shell(command),
        }
    }

    pub fn arg(self, arg: &str) -> Command {
        Command {
            inner: self.inner.arg(arg),
        }
    }

    pub fn args(self, extra_args: Vec<&str>) -> Command {
        Command {
            inner: self.inner.args(extra_args),
        }
    }

    pub fn current_dir(self, cwd: &str) -> Command {
        Command {
            inner: self.inner.current_dir(cwd),
        }
    }

    pub fn run(self) {
        self.inner.run()
    }

    pub fn ok(self) -> bool {
        self.inner.ok()
    }

    pub fn output(self) -> str {
        self.inner.output()
    }

    pub fn status(self) -> i64 {
        self.inner.status()
    }

    pub fn output_result(self) -> ProcessResult {
        self.inner.output_result()
    }
}

fn exec_command(process: Process) -> ProcessResult {
    process.output_result()
}

pub fn run(command: str) {
    Process::shell(command).run()
}

pub fn ok(command: str) -> bool {
    Process::shell(command).ok()
}

pub fn output(command: str) -> str {
    Process::shell(command).output()
}

pub fn status(command: str) -> i64 {
    Process::shell(command).status()
}

pub fn run_argv(program: &str, args: Vec<&str>) {
    Process::new(program).args(args).run()
}

pub fn ok_argv(program: &str, args: Vec<&str>) -> bool {
    Process::new(program).args(args).ok()
}

pub fn output_argv(program: &str, args: Vec<&str>) -> str {
    Process::new(program).args(args).output()
}

pub fn status_argv(program: &str, args: Vec<&str>) -> i64 {
    Process::new(program).args(args).status()
}

pub fn run_argv_in(program: &str, args: Vec<&str>, cwd: &str) {
    Process::new(program).args(args).current_dir(cwd).run()
}

pub fn ok_argv_in(program: &str, args: Vec<&str>, cwd: &str) -> bool {
    Process::new(program).args(args).current_dir(cwd).ok()
}

pub fn output_argv_in(program: &str, args: Vec<&str>, cwd: &str) -> str {
    Process::new(program).args(args).current_dir(cwd).output()
}

pub fn status_argv_in(program: &str, args: Vec<&str>, cwd: &str) -> i64 {
    Process::new(program).args(args).current_dir(cwd).status()
}

fn render_process_command(process: Process) -> str {
    let command = match process.shell_command != "" {
        true => process.shell_command,
        false => render_argv_command(process.program, process.args),
    };
    match process.cwd != "" {
        true => wrap_command_with_cwd(&process.cwd, &command),
        false => command,
    }
}

fn render_argv_command(program: &str, args: Vec<&str>) -> str {
    let mut parts: Vec<str> = Vec::new();
    parts.push(quote_shell_arg(program));
    let mut idx = 0;
    while idx < args.len() {
        parts.push(quote_shell_arg(args[idx]));
        idx = idx + 1;
    }
    parts.join(" ")
}

fn decode_exit_status(status: i64) -> i64 {
    match status < 0 {
        true => -1,
        false => match status % 256 == 0 {
            true => status / 256,
            false => -(status % 256),
        },
    }
}

fn wrap_command_with_cwd(cwd: &str, command: &str) -> str {
    f"cd {quote_shell_arg(cwd)} && {command}"
}

fn quote_shell_arg(value: &str) -> str {
    let escaped = value.replace("'", "'\"'\"'");
    f"'{escaped}'"
}
