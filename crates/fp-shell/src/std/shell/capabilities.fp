pub mod capabilities {
    pub const fn has_rsync() -> bool {
        std::facts::has_command("rsync")
    }

    pub const fn has_ssh() -> bool {
        std::facts::has_command("ssh")
    }

    pub const fn has_docker() -> bool {
        std::facts::has_command("docker")
    }

    pub const fn has_kubectl() -> bool {
        std::facts::has_command("kubectl")
    }

    pub const fn has_pwsh() -> bool {
        std::facts::has_command("pwsh")
    }

    pub const fn host_supports_rsync(host: str) -> bool {
        match std::facts::host_transport(host) {
            "ssh" => has_rsync() && has_ssh(),
            "docker" => has_rsync() && has_docker(),
            "kubectl" => has_rsync() && has_kubectl(),
            "winrm" => has_rsync() && has_pwsh(),
            "chroot" => has_rsync(),
            "local" => has_rsync(),
            _ => false,
        }
    }
}
