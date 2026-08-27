#[intrinsic = "path_join"]
pub fn join(lhs: &str, rhs: &str) -> str { compile_error!("compiler intrinsic") }

#[intrinsic = "path_parent"]
pub fn parent(path: &str) -> str { compile_error!("compiler intrinsic") }

#[intrinsic = "path_file_name"]
pub fn file_name(path: &str) -> str { compile_error!("compiler intrinsic") }

#[intrinsic = "path_extension"]
pub fn extension(path: &str) -> str { compile_error!("compiler intrinsic") }

#[intrinsic = "path_stem"]
pub fn stem(path: &str) -> str { compile_error!("compiler intrinsic") }

#[intrinsic = "path_is_absolute"]
pub fn is_absolute(path: &str) -> bool { compile_error!("compiler intrinsic") }

#[intrinsic = "path_normalize"]
pub fn normalize(path: &str) -> str { compile_error!("compiler intrinsic") }
