const fn main() {
    std::server::shell("echo local pre-check");

    std::host::on("web-1", || {
        std::service::restart(name="fp-service");
    });

    std::server::shell("sudo journalctl -u fp-service -n 10", hosts=["web-1", "web-2"]);

    std::server::shell("echo local post-check");
}
