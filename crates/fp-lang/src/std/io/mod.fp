#[op = "io_read_stdin_to_string"]
pub fn read_stdin_to_string() -> str { ::std::intrinsics::io::read_stdin_to_string() }

pub fn write_stdout(text: &str) { ::libc::write(1, text.as_ptr() as *const void, text.len() as u64); }

pub fn write_stderr(text: &str) { ::libc::write(2, text.as_ptr() as *const void, text.len() as u64); }
