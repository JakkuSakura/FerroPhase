pub const fn packages() -> str {
    std::shell::process::output("choco list")
}

pub const fn version() -> str {
    std::shell::process::output("choco --version")
}
