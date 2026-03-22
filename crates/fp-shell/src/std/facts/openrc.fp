pub const fn status(runlevel: str) -> str {
    if runlevel == "" {
        std::shell::process::output("rc-status default")
    } else {
        std::shell::process::output(f"rc-status {runlevel}")
    }
}

pub const fn enabled(runlevel: str) -> str {
    if runlevel == "" {
        std::shell::process::output("rc-update show -v | grep default || true")
    } else {
        std::shell::process::output(f"rc-update show -v | grep {runlevel} || true")
    }
}
