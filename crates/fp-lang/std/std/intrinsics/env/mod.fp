#[intrinsic = "env_current_dir"]
pub fn current_dir() -> str { compile_error!("compiler intrinsic") }
