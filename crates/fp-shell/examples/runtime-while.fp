const fn main() {
    while std::shell::process::ok("test -f /tmp/fp-rollout.lock") {
        std::server::shell("sleep 1");
    }

    with ["web-1", "web-2"] {
        std::server::shell("sudo systemctl restart fp-service");
    }
}
