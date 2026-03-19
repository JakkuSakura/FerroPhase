const fn main() {
    std::server::shell("echo local pre-check");

    with "web-1" {
        std::service::restart(name="fp-service");
    }

    with ["web-1", "web-2"] {
        std::server::shell("sudo journalctl -u fp-service -n 10");
    }

    std::server::shell("echo local post-check");
}
