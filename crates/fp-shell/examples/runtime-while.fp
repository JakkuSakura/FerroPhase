const fn main() {
    while std::server::shell("test -f /tmp/fp-rollout.lock") {
        std::server::shell("sleep 1");
    }

    std::host::on(["web-1", "web-2"], || {
        std::server::shell("sudo systemctl restart fp-service");
    });
}
