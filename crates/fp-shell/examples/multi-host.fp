const fn main() {
    with ["web-1", "web-2", "web-3"] {
        std::server::shell("uname -a");
        std::server::shell("sudo systemctl status fp-service");
    }
}
