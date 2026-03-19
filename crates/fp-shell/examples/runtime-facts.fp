const fn main() {
    if std::shell::facts::file_exists("/etc/hosts") {
        std::server::shell("echo hosts file present");
    }

    if std::shell::capabilities::has_rsync() {
        std::server::shell("echo rsync available");
    }
}
