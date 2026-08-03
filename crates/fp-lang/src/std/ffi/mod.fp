pub struct CStr {}

impl CStr {
    pub fn as_ptr(&self) -> *const ::libc::char {
        self as *const CStr as *const ::libc::char
    }

    pub fn to_bytes(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.as_ptr() as *const u8, ::libc::strlen(self)) }
    }

    pub fn to_bytes_with_nul(&self) -> &[u8] {
        unsafe {
            std::slice::from_raw_parts(
                self.as_ptr() as *const u8,
                ::libc::strlen(self) + 1,
            )
        }
    }

    pub unsafe fn as_str_unchecked(&self) -> &str {
        std::str::from_utf8_unchecked(self.to_bytes())
    }

    pub fn as_str(&self) -> std::result::Result<&str, std::str::Utf8Error> {
        std::str::from_utf8(self.to_bytes())
    }
}
