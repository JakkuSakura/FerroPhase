const fn main() {
    std::ops::server::shell("echo local pre-check");

    with "web-1" {
        std::ops::services::restart(name="fp-service");
    }

    with ["web-1", "web-2"] {
        std::ops::server::shell("sudo journalctl -u fp-service -n 10");
    }

    std::ops::server::shell("echo local post-check");
}
