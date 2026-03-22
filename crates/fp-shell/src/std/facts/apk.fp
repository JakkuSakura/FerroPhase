pub const fn packages() -> str {
    std::shell::process::output("apk list --installed")
}
