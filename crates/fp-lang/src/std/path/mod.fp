#[lang = "path_join"]
pub fn join(lhs: &str, rhs: &str) -> str { compile_error!("compiler intrinsic") }

#[lang = "path_parent"]
pub fn parent(path: &str) -> str { compile_error!("compiler intrinsic") }

#[lang = "path_file_name"]
pub fn file_name(path: &str) -> str { compile_error!("compiler intrinsic") }

#[lang = "path_extension"]
pub fn extension(path: &str) -> str { compile_error!("compiler intrinsic") }

#[lang = "path_stem"]
pub fn stem(path: &str) -> str { compile_error!("compiler intrinsic") }

#[lang = "path_is_absolute"]
pub fn is_absolute(path: &str) -> bool { compile_error!("compiler intrinsic") }

#[lang = "path_normalize"]
pub fn normalize(path: &str) -> str { compile_error!("compiler intrinsic") }

pub fn has_extension(path: &str, extension: &str) -> bool {
    self::extension(path) == extension
}

pub fn join_all(parts: Vec<&str>) -> str {
    if parts.len() == 0 {
        return "";
    }

    let mut out = parts[0];
    let mut idx = 1;
    while idx < parts.len() {
        out = join(out, parts[idx]);
        idx = idx + 1;
    }
    out
}
