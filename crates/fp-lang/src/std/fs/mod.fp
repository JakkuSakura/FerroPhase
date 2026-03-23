#[lang = "fs_read_dir"]
pub fn read_dir(path: &str) -> Vec<&str> { compile_error!("compiler intrinsic") }

#[lang = "fs_walk_dir"]
pub fn walk_dir(path: &str) -> Vec<&str> { compile_error!("compiler intrinsic") }

#[lang = "fs_read_to_string"]
pub fn read_to_string(path: &str) -> str { compile_error!("compiler intrinsic") }

#[lang = "fs_write_string"]
pub fn write_string(path: &str, content: &str) { compile_error!("compiler intrinsic") }

#[lang = "fs_append_string"]
pub fn append_string(path: &str, content: &str) { compile_error!("compiler intrinsic") }

#[lang = "fs_exists"]
pub fn exists(path: &str) -> bool { compile_error!("compiler intrinsic") }

#[lang = "fs_is_dir"]
pub fn is_dir(path: &str) -> bool { compile_error!("compiler intrinsic") }

#[lang = "fs_is_file"]
pub fn is_file(path: &str) -> bool { compile_error!("compiler intrinsic") }

#[lang = "fs_create_dir_all"]
pub fn create_dir_all(path: &str) { compile_error!("compiler intrinsic") }

#[lang = "fs_remove_file"]
pub fn remove_file(path: &str) { compile_error!("compiler intrinsic") }

#[lang = "fs_remove_dir_all"]
pub fn remove_dir_all(path: &str) { compile_error!("compiler intrinsic") }

#[lang = "fs_glob"]
pub fn glob(pattern: &str) -> Vec<&str> { compile_error!("compiler intrinsic") }
