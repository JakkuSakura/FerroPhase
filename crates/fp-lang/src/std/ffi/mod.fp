pub struct CStr {}

impl CStr {
    pub fn as_ptr(&self) -> *const ::libc::char { compile_error!("compiler intrinsic") }

    pub fn to_bytes(&self) -> &[u8] { compile_error!("compiler intrinsic") }

    pub fn to_bytes_with_nul(&self) -> &[u8] { compile_error!("compiler intrinsic") }

    pub unsafe fn as_str_unchecked(&self) -> &str { compile_error!("compiler intrinsic") }

    pub fn as_str(&self) -> std::result::Result<&str, std::string::Utf8Error> {
        compile_error!("compiler intrinsic")
    }
}
