pub const fn service_script(service: str, jail: str) -> str {
    if jail == "" {
        std::shell::process::output(
            f"for entry in `service -l`; do if [ \"{service}\" = \"$entry\" ]; then echo \"$entry\"; fi; done"
        )
    } else {
        std::shell::process::output(
            f"for entry in `service -j {jail} -l`; do if [ \"{service}\" = \"$entry\" ]; then echo \"$entry\"; fi; done"
        )
    }
}

pub const fn service_status(service: str, jail: str) -> str {
    if jail == "" {
        std::shell::process::output(
            f"service {service} status > /dev/null 2>&1 && printf '%s\\n' running || true"
        )
    } else {
        std::shell::process::output(
            f"service -j {jail} {service} status > /dev/null 2>&1 && printf '%s\\n' running || true"
        )
    }
}

pub const fn sysrc(parameter: str, jail: str) -> str {
    if jail == "" {
        std::shell::process::output(f"sysrc -in -- {parameter} || true")
    } else {
        std::shell::process::output(f"sysrc -j {jail} -in -- {parameter} || true")
    }
}

pub const fn package(package: str, jail: str) -> str {
    if jail == "" {
        std::shell::process::output(f"pkg info -E -- {package} 2>/dev/null || true")
    } else {
        std::shell::process::output(f"pkg -j {jail} info -E -- {package} 2>/dev/null || true")
    }
}
