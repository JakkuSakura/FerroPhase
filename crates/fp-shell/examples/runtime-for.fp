const fn main() {
    for step in ["pre", "post"] {
        with ["web-1", "web-2"] {
            std::server::shell(f"echo rollout step={step}");
            std::server::shell("sudo systemctl status fp-service");
        }
    }
}
