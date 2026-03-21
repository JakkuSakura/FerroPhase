const fn main() {
    std::ops::files::copy(
        "somefile.txt",
        "/home/somefile.txt",
        creates="/home/somefile.txt",
    );
}
