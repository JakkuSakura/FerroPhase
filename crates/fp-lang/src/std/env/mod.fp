#[op = "env_current_dir"]
pub fn current_dir() -> str { ::std::intrinsics::env::current_dir() }

#[op = "env_temp_dir"]
pub fn temp_dir() -> str { unsafe { ::std::ffi::CStr::from_ptr(::libc::getenv(c"TMPDIR".as_ptr())).as_str_unchecked() } }

#[op = "env_home_dir"]
pub fn home_dir() -> str { unsafe { ::std::ffi::CStr::from_ptr(::libc::getenv(c"HOME".as_ptr())).as_str_unchecked() } }

#[op = "env_var"]
pub fn var(name: &str) -> str { unsafe { ::std::ffi::CStr::from_ptr(::libc::getenv(::std::ffi::CString::new(name).as_ptr())).as_str_unchecked() } }

#[op = "env_var_exists"]
pub fn exists(name: &str) -> bool { (::libc::getenv(::std::ffi::CString::new(name).as_ptr()) as i64) != 0 }
