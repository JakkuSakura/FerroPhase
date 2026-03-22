pub const fn containers() -> str {
    std::shell::process::output("vzlist -a -j")
}
