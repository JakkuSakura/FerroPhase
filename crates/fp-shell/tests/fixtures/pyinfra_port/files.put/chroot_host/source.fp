const fn main() {
    std::ops::files::copy(
        "rootfs.tar",
        "/tmp/rootfs.tar",
        hosts="builder",
    );
}
