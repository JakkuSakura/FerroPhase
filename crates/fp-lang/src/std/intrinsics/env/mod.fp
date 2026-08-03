#[intrinsic = "env_current_dir"]
pub fn current_dir() -> str { compile_error!("compiler intrinsic") }

#[intrinsic = "env_temp_dir"]
pub fn temp_dir() -> str { compile_error!("compiler intrinsic") }

#[intrinsic = "env_home_dir"]
pub fn home_dir() -> str { compile_error!("compiler intrinsic") }

#[intrinsic = "env_var"]
pub fn var(name: &str) -> str { compile_error!("compiler intrinsic") }

#[intrinsic = "env_var_exists"]
pub fn exists(name: &str) -> bool { compile_error!("compiler intrinsic") }
