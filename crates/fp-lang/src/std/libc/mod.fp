pub mod generated;

pub extern "C" fn system(command: &std::ffi::CStr) -> c_int;
pub extern "C" fn getenv(name: &std::ffi::CStr) -> &std::ffi::CStr;
pub extern "C" fn getenv_ptr(name: &std::ffi::CStr) -> *const generated::c_char;
pub extern "C" fn getcwd(buf: *mut generated::c_char, size: usize) -> &std::ffi::CStr;
