use std::alloc::Vec;

pub struct CommandMockMatch {
    pub stdout: str,
    pub stderr: str,
    pub status: i64,
}

#[intrinsic = "test_command_mock_reset"]
pub fn command_mock_reset() { compile_error!("compiler intrinsic") }

#[intrinsic = "test_command_mock_push"]
pub fn command_mock_push(pattern: &str, stdout: &str, stderr: &str, status: i64) {
    compile_error!("compiler intrinsic")
}

#[intrinsic = "test_command_mock_take_calls"]
pub fn command_mock_take_calls() -> Vec<&str> { compile_error!("compiler intrinsic") }

#[intrinsic = "test_command_mock_apply"]
pub fn command_mock_apply(command: &str) -> Option<CommandMockMatch> {
    compile_error!("compiler intrinsic")
}
