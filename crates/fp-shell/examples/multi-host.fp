const fn main() {
    std::server::shell("uname -a", hosts=["web-1", "web-2", "web-3"]);
    std::server::shell("sudo systemctl status fp-service", hosts=["web-1", "web-2", "web-3"]);
}
