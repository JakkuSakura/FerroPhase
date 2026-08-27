
#[intrinsic = "fs_read_dir"]
pub fn read_dir(path: &::std::path::Path) -> ::alloc::Vec<&str> { compile_error!("compiler intrinsic") }

#[intrinsic = "fs_walk_dir"]
pub fn walk_dir(path: &::std::path::Path) -> ::alloc::Vec<&str> { compile_error!("compiler intrinsic") }

#[intrinsic = "fs_read_to_string"]
pub fn read_to_string(path: &::std::path::Path) -> str { compile_error!("compiler intrinsic") }

#[intrinsic = "fs_write_string"]
pub fn write_string(path: &::std::path::Path, content: &str) { compile_error!("compiler intrinsic") }

#[intrinsic = "fs_append_string"]
pub fn append_string(path: &::std::path::Path, content: &str) { compile_error!("compiler intrinsic") }

#[intrinsic = "fs_is_dir"]
pub fn is_dir(path: &::std::path::Path) -> bool { compile_error!("compiler intrinsic") }

#[intrinsic = "fs_is_file"]
pub fn is_file(path: &::std::path::Path) -> bool { compile_error!("compiler intrinsic") }

#[intrinsic = "fs_create_dir_all"]
pub fn create_dir_all(path: &::std::path::Path) { compile_error!("compiler intrinsic") }

#[intrinsic = "fs_remove_file"]
pub fn remove_file(path: &::std::path::Path) { compile_error!("compiler intrinsic") }

#[intrinsic = "fs_remove_dir_all"]
pub fn remove_dir_all(path: &::std::path::Path) { compile_error!("compiler intrinsic") }

#[intrinsic = "fs_glob"]
pub fn glob(pattern: &str) -> ::alloc::Vec<&str> { compile_error!("compiler intrinsic") }
