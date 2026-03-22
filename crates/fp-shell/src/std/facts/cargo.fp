pub const fn packages() -> str {
    std::shell::process::output("cargo install --list")
}
