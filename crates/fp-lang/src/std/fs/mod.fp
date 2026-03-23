extern "host" fn read_dir_impl(path: str) -> Vec<&str>;

extern "host" fn read_to_string_impl(path: str) -> str;

extern "host" fn write_string_impl(path: str, content: str);

extern "host" fn is_dir_impl(path: str) -> bool;

pub fn read_dir(path: &str) -> Vec<&str> {
    read_dir_impl(path)
}

pub fn read_to_string(path: &str) -> str {
    read_to_string_impl(path)
}

pub fn write_string(path: &str, content: &str) {
    write_string_impl(path, content)
}

pub fn is_dir(path: &str) -> bool {
    is_dir_impl(path)
}
