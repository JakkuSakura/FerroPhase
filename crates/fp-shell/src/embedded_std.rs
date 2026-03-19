mod generated {
    include!(concat!(env!("OUT_DIR"), "/embedded_shell_std.rs"));
}

pub fn read(path: &str) -> Option<&'static str> {
    generated::get(path)
}
