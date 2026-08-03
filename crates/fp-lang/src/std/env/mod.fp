#[op = "env_current_dir"]
pub fn current_dir() -> str { std::intrinsics::env::current_dir() }

#[op = "env_temp_dir"]
pub fn temp_dir() -> str { std::intrinsics::env::temp_dir() }

#[op = "env_home_dir"]
pub fn home_dir() -> str { std::intrinsics::env::home_dir() }

#[op = "env_var"]
pub fn var(name: &str) -> str { std::intrinsics::env::var(name) }

#[op = "env_var_exists"]
pub fn exists(name: &str) -> bool { std::intrinsics::env::exists(name) }
