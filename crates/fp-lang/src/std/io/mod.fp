#[op = "io_read_stdin_to_string"]
pub fn read_stdin_to_string() -> str { std::intrinsics::io::read_stdin_to_string() }

#[op = "io_write_stdout"]
pub fn write_stdout(text: &str) { std::intrinsics::io::write_stdout(text) }

#[op = "io_write_stderr"]
pub fn write_stderr(text: &str) { std::intrinsics::io::write_stderr(text) }
