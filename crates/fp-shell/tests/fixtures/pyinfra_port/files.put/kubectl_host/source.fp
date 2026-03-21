const fn main() {
    std::ops::files::copy(
        "config.yaml",
        "/etc/myapp/config.yaml",
        hosts="api",
    );
}
