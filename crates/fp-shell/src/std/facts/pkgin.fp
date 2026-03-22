pub const fn packages() -> str {
    std::shell::process::output("pkgin list")
}
