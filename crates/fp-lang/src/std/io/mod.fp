#[lang = "io_read_stdin_to_string"]
pub fn read_stdin_to_string() -> str { compile_error!("compiler intrinsic") }

#[lang = "io_write_stdout"]
pub fn write_stdout(text: &str) { compile_error!("compiler intrinsic") }

#[lang = "io_write_stderr"]
pub fn write_stderr(text: &str) { compile_error!("compiler intrinsic") }
