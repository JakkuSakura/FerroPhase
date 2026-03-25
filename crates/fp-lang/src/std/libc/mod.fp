pub mod generated;

pub extern "C" fn system(command: &std::ffi::CStr) -> c_int;
