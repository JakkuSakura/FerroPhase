const fn main() {
    if std::server::shell("test -f /etc/fp-service/enabled") {
        std::host::on("web-1", || {
            std::service::restart(name="fp-service");
        });
    } else {
        std::server::shell("echo service disabled", hosts=["web-1"]);
    }
}
