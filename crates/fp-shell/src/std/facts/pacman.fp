pub const fn packages() -> str {
    std::shell::process::output("pacman -Q")
}

pub const fn expand_package(package: str) -> str {
    std::shell::process::output(f"pacman -S --print-format '%n' {package} || true")
}
