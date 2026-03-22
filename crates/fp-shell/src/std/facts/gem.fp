pub const fn packages() -> str {
    std::shell::process::output("gem list --local")
}
