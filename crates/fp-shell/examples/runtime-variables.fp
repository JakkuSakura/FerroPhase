const fn restart_service(host: str, service: str) {
    with host {
        std::ops::server::shell(f"echo restarting service={service} on host={host}");
        std::ops::server::shell(f"sudo systemctl restart {service}");
    }
}

const fn main() {
    let service = "fp-service";

    for host in ["web-1", "web-2"] {
        with host {
            std::ops::server::shell(f"echo loop host={host}");
            std::ops::server::shell("hostname");
        }
        restart_service(host, service);
    }
}
