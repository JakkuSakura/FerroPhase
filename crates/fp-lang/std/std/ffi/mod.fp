pub struct CStr {
    ptr: *const ::libc::char,
}

impl CStr {
    pub fn from_ptr(ptr: *const ::libc::char) -> CStr { CStr { ptr } }

    pub fn as_ptr(&self) -> *const ::libc::char { self.ptr }

    pub fn to_bytes(&self) -> &[u8] { compile_error!("compiler intrinsic") }

    pub fn to_bytes_with_nul(&self) -> &[u8] { compile_error!("compiler intrinsic") }

    pub unsafe fn as_str_unchecked(&self) -> &str {
        let len = ::libc::strlen(self.ptr) as usize;
        raw_parts_to_str(self.ptr, len)
    }

    pub fn as_str(&self) -> ::core::result::Result<&str, ::alloc::string::Utf8Error> {
        compile_error!("compiler intrinsic")
    }
}

pub fn raw_parts_to_str(ptr: *const ::libc::char, len: usize) -> &str { compile_error!("compiler intrinsic") }

pub struct CString {
    ptr: *mut u8,
}

impl CString {
    pub fn new(s: &str) -> CString {
        let len = s.len();
        let buf = ::libc::malloc((len + 1) as u64) as *mut u8;
        ::libc::memcpy(buf as *mut void, s.as_ptr() as *const void, len as u64);
        let end = (buf as usize + len) as *mut u8;
        *end = 0;
        CString { ptr: buf }
    }

    pub fn as_ptr(&self) -> *const ::libc::char { self.ptr as *const ::libc::char }
}
