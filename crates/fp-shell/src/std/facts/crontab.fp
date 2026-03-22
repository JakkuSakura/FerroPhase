pub const fn entries(user: str) -> str {
    if user == "" {
        std::shell::process::output("crontab -l || true")
    } else {
        std::shell::process::output(f"crontab -l -u {user} || true")
    }
}
