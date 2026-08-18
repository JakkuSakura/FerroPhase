pub struct String {
    ptr: *mut u8,
    len: usize,
    capacity: usize,
}

pub struct Utf8Error {}

impl String {
    pub fn new() -> String {
        String { ptr: 0 as *mut u8, len: 0, capacity: 0 }
    }

    pub fn with_capacity(capacity: usize) -> String {
        if capacity == 0 {
            String::new()
        } else {
            let buf = ::libc::malloc(capacity as u64) as *mut u8;
            String { ptr: buf, len: 0, capacity }
        }
    }

    pub fn capacity(&self) -> usize {
        self.capacity
    }

    pub fn extend(&mut self, s: &str) {
        let add_len = s.len();
        let new_len = self.len + add_len;
        if new_len > self.capacity {
            let doubled = self.capacity * 2;
            let new_capacity = if new_len > doubled { new_len } else { doubled };
            let new_buf = ::libc::malloc(new_capacity as u64) as *mut u8;
            ::libc::memcpy(new_buf, self.ptr, self.len as u64);
            self.ptr = new_buf;
            self.capacity = new_capacity;
        }
        let dest = (self.ptr as usize + self.len) as *mut u8;
        ::libc::memcpy(dest as *mut void, s.as_ptr() as *const void, add_len as u64);
        self.len = new_len;
    }

    pub fn as_str(&self) -> &str {
        ::std::ffi::raw_parts_to_str(self.ptr, self.len)
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn push_byte(&mut self, byte: u8) {
        let new_len = self.len + 1;
        if new_len > self.capacity {
            let doubled = self.capacity * 2;
            let new_capacity = if new_len > doubled { new_len } else { doubled };
            let new_buf = ::libc::malloc(new_capacity as u64) as *mut u8;
            ::libc::memcpy(new_buf, self.ptr, self.len as u64);
            self.ptr = new_buf;
            self.capacity = new_capacity;
        }
        let dest = (self.ptr as usize + self.len) as *mut u8;
        *dest = byte;
        self.len = new_len;
    }
}

impl str {
    pub fn len(&self) -> usize { compile_error!("compiler intrinsic") }

    pub fn lines(&self) -> ::std::alloc::Vec<&str> { compile_error!("compiler intrinsic") }

    /// Raw data pointer — only safe to pass to a C function that also
    /// receives an explicit length (e.g. `write`/`memcpy`'s buffer
    /// parameters). Never NUL-terminated; do not pass this to a C function
    /// that scans for a NUL terminator (use a `c"..."` literal or a real
    /// `CString`-style constructor for those instead).
    pub fn as_ptr(&self) -> *const u8 { compile_error!("compiler intrinsic") }

    pub fn starts_with(&self, prefix: &str) -> bool {
        compile_error!("compiler intrinsic")
    }

    pub fn ends_with(&self, suffix: &str) -> bool {
        compile_error!("compiler intrinsic")
    }

    pub fn contains(&self, needle: &str) -> bool {
        compile_error!("compiler intrinsic")
    }

    pub fn replace(&self, pattern: &str, replacement: &str) -> ::std::string::String {
        let mut result: ::std::string::String = ::std::string::String::new();
        let self_len = self.len();
        let pattern_len = pattern.len();
        let mut idx = 0;
        while idx < self_len {
            let mut matched = false;
            if pattern_len > 0 && idx + pattern_len <= self_len {
                matched = true;
                let mut j = 0;
                while j < pattern_len {
                    if self[idx + j] != pattern[j] {
                        matched = false;
                        break;
                    }
                    j = j + 1;
                }
            }
            if matched {
                result.extend(replacement);
                idx = idx + pattern_len;
            } else {
                result.push_byte(self[idx] as u8);
                idx = idx + 1;
            }
        }
        result
    }
}

impl String {
    pub fn starts_with(&self, prefix: &str) -> bool {
        compile_error!("compiler intrinsic")
    }

    pub fn ends_with(&self, suffix: &str) -> bool {
        compile_error!("compiler intrinsic")
    }

    pub fn contains(&self, needle: &str) -> bool {
        compile_error!("compiler intrinsic")
    }

    pub fn lines(&self) -> ::std::alloc::Vec<&str> {
        compile_error!("compiler intrinsic")
    }
}
