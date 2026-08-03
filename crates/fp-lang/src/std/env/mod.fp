#[op = "env_current_dir"]
pub fn current_dir() -> str { std::intrinsics::env::current_dir() }

pub fn temp_dir() -> str { unsafe { ::libc::getenv("TMPDIR").as_str_unchecked() } }

pub fn home_dir() -> str { unsafe { ::libc::getenv("HOME").as_str_unchecked() } }

pub fn var(name: &str) -> str { unsafe { ::libc::getenv(name).as_str_unchecked() } }

pub fn exists(name: &str) -> bool { !::libc::getenv(name).is_null() }
