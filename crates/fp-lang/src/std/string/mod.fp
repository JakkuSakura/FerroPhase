pub struct String {
    value: str,
}

impl str {
    pub fn len(&self) -> usize { compile_error!("compiler intrinsic") }

    pub fn starts_with(&self, prefix: &str) -> bool {
        compile_error!("compiler intrinsic")
    }

    pub fn ends_with(&self, suffix: &str) -> bool {
        compile_error!("compiler intrinsic")
    }

    pub fn contains(&self, needle: &str) -> bool {
        compile_error!("compiler intrinsic")
    }
}

impl String {
    pub fn len(&self) -> usize { compile_error!("compiler intrinsic") }

    pub fn starts_with(&self, prefix: &str) -> bool {
        compile_error!("compiler intrinsic")
    }

    pub fn ends_with(&self, suffix: &str) -> bool {
        compile_error!("compiler intrinsic")
    }

    pub fn contains(&self, needle: &str) -> bool {
        compile_error!("compiler intrinsic")
    }
}
