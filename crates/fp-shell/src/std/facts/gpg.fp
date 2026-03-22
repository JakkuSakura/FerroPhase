pub const fn key(src: str) -> str {
    std::shell::process::output(
        f"(wget -O - {src} || curl -sSLf {src} || cat {src}) | gpg --with-colons"
    )
}

pub const fn keys(keyring: str) -> str {
    if keyring == "" {
        std::shell::process::output("gpg --list-keys --with-colons")
    } else {
        std::shell::process::output(
            f"gpg --list-keys --with-colons --keyring {keyring} --no-default-keyring"
        )
    }
}

pub const fn secret_keys(keyring: str) -> str {
    if keyring == "" {
        std::shell::process::output("gpg --list-secret-keys --with-colons")
    } else {
        std::shell::process::output(
            f"gpg --list-secret-keys --with-colons --keyring {keyring} --no-default-keyring"
        )
    }
}
