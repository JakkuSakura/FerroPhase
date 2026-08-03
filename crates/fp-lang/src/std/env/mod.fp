#[op = "env_current_dir"]
pub fn current_dir() -> str { intrinsic_current_dir() }

#[intrinsic = "env_current_dir"]
fn intrinsic_current_dir() -> str { compile_error!("compiler intrinsic") }

#[op = "env_temp_dir"]
pub fn temp_dir() -> str { intrinsic_temp_dir() }

#[intrinsic = "env_temp_dir"]
fn intrinsic_temp_dir() -> str { compile_error!("compiler intrinsic") }

#[op = "env_home_dir"]
pub fn home_dir() -> str { intrinsic_home_dir() }

#[intrinsic = "env_home_dir"]
fn intrinsic_home_dir() -> str { compile_error!("compiler intrinsic") }

#[op = "env_var"]
pub fn var(name: &str) -> str { intrinsic_var(name) }

#[intrinsic = "env_var"]
fn intrinsic_var(name: &str) -> str { compile_error!("compiler intrinsic") }

#[op = "env_var_exists"]
pub fn exists(name: &str) -> bool { intrinsic_exists(name) }

#[intrinsic = "env_var_exists"]
fn intrinsic_exists(name: &str) -> bool { compile_error!("compiler intrinsic") }
