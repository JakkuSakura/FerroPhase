const fn main() {
    with ["web-1", "web-2", "web-3"] {
        std::ops::server::shell("uname -a");
        std::ops::server::shell("sudo systemctl status fp-service");
    }
}
