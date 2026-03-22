pub const fn container_running(name: str) -> bool {
    std::shell::process::ok(
        f"docker inspect --format '{{{{.State.Running}}}}' {name} | grep -q true"
    )
}

pub const fn image_present(name: str) -> bool {
    std::shell::process::ok(f"docker image inspect {name}")
}

pub const fn container_id(name: str) -> str {
    std::shell::process::output(f"docker inspect --format '{{{{.Id}}}}' {name}")
}
