pub const fn packages() -> str {
    std::shell::process::output("pkg info || pkg_info || true")
}
