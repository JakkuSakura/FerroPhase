pub const fn package(package: str) -> str {
    std::shell::process::output(f"flatpak info {package}")
}

pub const fn packages() -> str {
    std::shell::process::output("flatpak list --columns=application")
}
