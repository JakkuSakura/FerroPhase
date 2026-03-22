pub const fn info() -> str {
    std::shell::process::output("efibootmgr || true")
}
