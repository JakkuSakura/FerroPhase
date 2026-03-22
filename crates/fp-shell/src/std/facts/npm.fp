pub const fn packages(directory: str) -> str {
    if directory == "" {
        std::shell::process::output("npm list -g --depth=0")
    } else {
        std::shell::process::output(f"! test -d {directory} || (cd {directory} && npm list -g --depth=0)")
    }
}
