#[op = "io_read_stdin_to_string"]
pub fn read_stdin_to_string() -> str { intrinsic_read_stdin_to_string() }

#[intrinsic = "io_read_stdin_to_string"]
fn intrinsic_read_stdin_to_string() -> str { compile_error!("compiler intrinsic") }

#[op = "io_write_stdout"]
pub fn write_stdout(text: &str) { intrinsic_write_stdout(text) }

#[intrinsic = "io_write_stdout"]
fn intrinsic_write_stdout(text: &str) { compile_error!("compiler intrinsic") }

#[op = "io_write_stderr"]
pub fn write_stderr(text: &str) { intrinsic_write_stderr(text) }

#[intrinsic = "io_write_stderr"]
fn intrinsic_write_stderr(text: &str) { compile_error!("compiler intrinsic") }
