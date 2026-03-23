pub struct Command {
    shell_command: str,
    program: str,
    args: Vec<&str>,
    cwd: str,
}

impl Command {
    pub fn new(program: &str) -> Command {
        Command {
            shell_command: "",
            program,
            args: Vec::new(),
            cwd: "",
        }
    }

    pub fn shell(command: str) -> Command {
        Command {
            shell_command: command,
            program: "",
            args: Vec::new(),
            cwd: "",
        }
    }

    pub fn arg(self, arg: &str) -> Command {
        let mut args = self.args;
        args.push(arg);

        Command {
            shell_command: self.shell_command,
            program: self.program,
            args,
            cwd: self.cwd,
        }
    }

    pub fn args(self, extra_args: Vec<&str>) -> Command {
        let mut command = self;
        let mut idx = 0;
        let extra_len = extra_args.len();
        while idx < extra_len {
            command = command.arg(extra_args[idx]);
            idx = idx + 1;
        }
        command
    }

    pub fn current_dir(self, cwd: &str) -> Command {
        Command {
            shell_command: self.shell_command,
            program: self.program,
            args: self.args,
            cwd,
        }
    }

    pub fn run(self) {
        let result = self.exec();
        if !result.success() {
            panic(f"process exited with status {result.status()}: {result.stderr()}");
        }

        let stdout = result.into_stdout();
        if stdout != "" {
            std::io::write_stdout(&stdout);
        }
    }

    pub fn ok(self) -> bool {
        self.exec().success()
    }

    pub fn output(self) -> str {
        let result = self.exec();
        if !result.success() {
            panic(f"process exited with status {result.status()}: {result.stderr()}");
        }

        result.into_stdout()
    }

    pub fn status(self) -> i64 {
        self.exec().status()
    }

    fn exec(self) -> std::libc::ProcessResult {
        if self.shell_command != "" {
            return std::libc::shell(self.shell_command);
        }

        std::libc::exec(self.program, self.args, self.cwd)
    }
}

pub fn run(command: str) {
    Command::shell(command).run()
}

pub fn ok(command: str) -> bool {
    Command::shell(command).ok()
}

pub fn output(command: str) -> str {
    Command::shell(command).output()
}

pub fn status(command: str) -> i64 {
    Command::shell(command).status()
}

pub fn run_argv(program: &str, args: Vec<&str>) {
    Command::new(program).args(args).run()
}

pub fn ok_argv(program: &str, args: Vec<&str>) -> bool {
    Command::new(program).args(args).ok()
}

pub fn output_argv(program: &str, args: Vec<&str>) -> str {
    Command::new(program).args(args).output()
}

pub fn status_argv(program: &str, args: Vec<&str>) -> i64 {
    Command::new(program).args(args).status()
}

pub fn run_argv_in(program: &str, args: Vec<&str>, cwd: &str) {
    Command::new(program).args(args).current_dir(cwd).run()
}

pub fn ok_argv_in(program: &str, args: Vec<&str>, cwd: &str) -> bool {
    Command::new(program).args(args).current_dir(cwd).ok()
}

pub fn output_argv_in(program: &str, args: Vec<&str>, cwd: &str) -> str {
    Command::new(program).args(args).current_dir(cwd).output()
}

pub fn status_argv_in(program: &str, args: Vec<&str>, cwd: &str) -> i64 {
    Command::new(program).args(args).current_dir(cwd).status()
}
