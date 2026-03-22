pub const fn packages() -> str {
    std::shell::process::output("xbps-query -l")
}
