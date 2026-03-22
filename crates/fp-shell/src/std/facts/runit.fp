pub const fn status(service: str, svdir: str) -> str {
    if service == "" {
        std::shell::process::output(
            f"export SVDIR=\\\"{svdir}\\\" && cd \"$SVDIR\" && find * -maxdepth 0 -exec sv status {{}} + 2>/dev/null"
        )
    } else {
        std::shell::process::output(f"SVDIR=\\\"{svdir}\\\" sv status {service}")
    }
}

pub const fn managed(service: str, svdir: str) -> str {
    if service == "" {
        std::shell::process::output(f"cd {svdir} && find -mindepth 1 -maxdepth 1 -type l -printf '%f\\n'")
    } else {
        std::shell::process::output(f"cd {svdir} && test -h {service} && echo {service} || true")
    }
}
