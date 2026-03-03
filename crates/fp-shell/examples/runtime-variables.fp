const fn restart_service(host: str, service: str) {
    std::server::shell(f"echo restarting service={service} on host={host}");
    std::server::shell(f"sudo systemctl restart {service}", hosts=host);
}

const fn main() {
    let service = "fp-service";

    for host in ["web-1", "web-2"] {
        std::server::shell(f"echo loop host={host}");
        std::server::shell("hostname", hosts=[host]);
        restart_service(host, service);
    }
}
