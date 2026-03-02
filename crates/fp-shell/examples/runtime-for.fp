const fn main() {
    for step in ["pre", "post"] {
        std::server::shell("echo rollout step", hosts=["web-1", "web-2"]);
        std::server::shell("sudo systemctl status fp-service", hosts=["web-1", "web-2"]);
    }
}
