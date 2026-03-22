const fn main() {
    if std::facts::files::is_file("/etc/hosts") {
        std::ops::server::shell("echo hosts file present");
    }

    if std::facts::server::which("git") != "" {
        std::ops::server::shell("echo git available");
    }

    if std::facts::systemd::is_active("sshd") {
        std::ops::server::shell("echo sshd active");
    }

    if std::shell::capabilities::has_rsync() {
        std::ops::server::shell("echo rsync available");
    }
}
