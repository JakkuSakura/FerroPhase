const fn main() {
    with "web-1" {
        if std::shell::process::ok("test -f /etc/fp-service/enabled") {
            std::service::restart(name="fp-service");
        } else {
            std::server::shell("echo service disabled");
        }
    }
}
