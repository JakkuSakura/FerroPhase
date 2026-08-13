pub struct Vec<T> {}

impl<T> Vec<T> {
    pub const fn new() -> Vec<T> {
        Vec {}
    }

    pub fn len(&self) -> usize { compile_error!("compiler intrinsic") }

    pub fn push(&mut self, value: T) { compile_error!("compiler intrinsic") }
}

impl Vec<&str> {
    pub fn join(&self, sep: &str) -> ::std::string::String {
        let len = self.len() as i64;
        let mut result: ::std::string::String = ::std::string::String::new();
        let mut idx = 0;
        while idx < len {
            if idx > 0 {
                result.extend(sep);
            }
            result.extend(self[idx]);
            idx = idx + 1;
        }
        result
    }
}

impl Vec<::std::string::String> {
    pub fn join(&self, sep: &str) -> ::std::string::String {
        let len = self.len() as i64;
        let mut result: ::std::string::String = ::std::string::String::new();
        let mut idx = 0;
        while idx < len {
            if idx > 0 {
                result.extend(sep);
            }
            result.extend(self[idx].as_str());
            idx = idx + 1;
        }
        result
    }
}
