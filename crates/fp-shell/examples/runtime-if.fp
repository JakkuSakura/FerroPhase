const fn main() {
    with "web-1" {
        if std::shell::process::ok("test -f /etc/fp-service/enabled") {
            std::ops::services::restart(name="fp-service");
        } else {
            std::ops::server::shell("echo service disabled");
        }
    }
}
