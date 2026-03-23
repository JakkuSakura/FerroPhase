#[lang = "process_run"]
pub fn run(command: str) { compile_error!("compiler intrinsic") }

#[lang = "process_ok"]
pub fn ok(command: str) -> bool { compile_error!("compiler intrinsic") }

#[lang = "process_output"]
pub fn output(command: str) -> str { compile_error!("compiler intrinsic") }

#[lang = "process_status"]
pub fn status(command: str) -> i64 { compile_error!("compiler intrinsic") }

#[lang = "process_run_argv"]
pub fn run_argv(program: &str, args: Vec<&str>) { compile_error!("compiler intrinsic") }

#[lang = "process_ok_argv"]
pub fn ok_argv(program: &str, args: Vec<&str>) -> bool { compile_error!("compiler intrinsic") }

#[lang = "process_output_argv"]
pub fn output_argv(program: &str, args: Vec<&str>) -> str { compile_error!("compiler intrinsic") }

#[lang = "process_status_argv"]
pub fn status_argv(program: &str, args: Vec<&str>) -> i64 { compile_error!("compiler intrinsic") }

#[lang = "process_run_argv_in"]
pub fn run_argv_in(program: &str, args: Vec<&str>, cwd: &str) { compile_error!("compiler intrinsic") }

#[lang = "process_ok_argv_in"]
pub fn ok_argv_in(program: &str, args: Vec<&str>, cwd: &str) -> bool { compile_error!("compiler intrinsic") }

#[lang = "process_output_argv_in"]
pub fn output_argv_in(program: &str, args: Vec<&str>, cwd: &str) -> str { compile_error!("compiler intrinsic") }

#[lang = "process_status_argv_in"]
pub fn status_argv_in(program: &str, args: Vec<&str>, cwd: &str) -> i64 { compile_error!("compiler intrinsic") }
