
pub struct CommandMockMatch {
    pub stdout: str,
    pub stderr: str,
    pub status: i64,
}

#[intrinsic = "test_command_mock_take_calls"]
pub fn command_mock_take_calls() -> ::alloc::Vec<&str> { compile_error!("compiler intrinsic") }

#[intrinsic = "test_command_mock_apply"]
pub fn command_mock_apply(command: &str) -> ::core::option::Option<CommandMockMatch> {
    compile_error!("compiler intrinsic")
}
