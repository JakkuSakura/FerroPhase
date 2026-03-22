pub const fn version() -> str {
    std::shell::process::output("brew --version")
}

pub const fn packages() -> str {
    std::shell::process::output("brew list --versions")
}

pub const fn casks() -> str {
    std::shell::process::output(
        "if brew --version | grep -q -e 'Homebrew\\ +(1\\.|2\\.[0-5]).*' 1>/dev/null; then brew cask list --versions; else brew list --cask --versions; fi"
    )
}

pub const fn taps() -> str {
    std::shell::process::output("brew tap")
}
