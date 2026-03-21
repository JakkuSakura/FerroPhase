const fn main() {
    std::ops::files::copy(
        "artifact.zip",
        "C:/deploy/artifact.zip",
        hosts="win-1",
    );
}
