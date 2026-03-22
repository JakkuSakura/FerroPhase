pub const fn is_active(service: str) -> bool {
    std::shell::process::ok(f"systemctl is-active --quiet {normalize_service(service)}")
}

pub const fn is_enabled(service: str) -> bool {
    std::shell::process::ok(f"systemctl is-enabled --quiet {normalize_service(service)}")
}

pub const fn service_status(service: str) -> str {
    std::shell::process::output(f"systemctl status {normalize_service(service)}")
}

pub const fn show_property(service: str, property: str) -> str {
    std::shell::process::output(
        f"systemctl show --property {property} --value {normalize_service(service)}"
    )
}

const fn normalize_service(service: str) -> str {
    if std::shell::process::ok(
        f"printf '%s' {service} | grep -Eq '\\.(service|socket|device|mount|automount|swap|target|path|timer|slice|scope)$'"
    ) {
        service
    } else {
        f"{service}.service"
    }
}
