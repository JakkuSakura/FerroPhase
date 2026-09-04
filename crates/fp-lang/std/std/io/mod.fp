#[op(func = "io_read_stdin_to_string")]
pub fn read_stdin_to_string() -> str { ::std::intrinsics::io::read_stdin_to_string() }

#[op(func = "io_write_stdout")]
pub fn write_stdout(text: &str) { ::libc::write(1, text.as_ptr() as *const void, text.len() as u64); }

#[op(func = "io_write_stderr")]
pub fn write_stderr(text: &str) { ::libc::write(2, text.as_ptr() as *const void, text.len() as u64); }
