pub const fn status() -> str {
    std::shell::process::output("initctl list")
}
