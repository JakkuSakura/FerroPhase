const fn main() {
    std::ops::files::copy(
        "package.tar.gz",
        "/srv/app/package.tar.gz",
        hosts="app",
    );
}
