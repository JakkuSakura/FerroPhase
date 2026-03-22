pub const fn package(package: str) -> str {
    std::shell::process::output(f"snap info {package}")
}

pub const fn packages() -> str {
    std::shell::process::output("snap list")
}
