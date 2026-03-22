pub const fn packages() -> str {
    std::shell::process::output("pipx list --short")
}

pub const fn environment() -> str {
    std::shell::process::output("pipx environment")
}
