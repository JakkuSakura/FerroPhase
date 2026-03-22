pub const fn service(service: str, context hosts: str = "localhost", running: bool = true, restarted: bool, command: str, sudo: bool) -> bool {
    if command != "" {
        std::ops::server::shell(f"launchctl {command} {service}", hosts=hosts, sudo=sudo)
    } else {
        if restarted {
            std::ops::server::shell(f"launchctl stop {service}", hosts=hosts, sudo=sudo);
            std::ops::server::shell(f"launchctl start {service}", hosts=hosts, sudo=sudo);
            true
        } else {
            if running {
                std::ops::server::shell(f"launchctl start {service}", hosts=hosts, sudo=sudo)
            } else {
                std::ops::server::shell(f"launchctl stop {service}", hosts=hosts, sudo=sudo)
            }
        }
    }
}
