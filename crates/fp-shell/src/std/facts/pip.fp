pub const fn packages(pip: str) -> str {
    if pip == "" {
        std::shell::process::output("pip freeze --all")
    } else {
        std::shell::process::output(f"{pip} freeze --all")
    }
}

pub const fn packages3() -> str {
    std::shell::process::output("pip3 freeze --all")
}
