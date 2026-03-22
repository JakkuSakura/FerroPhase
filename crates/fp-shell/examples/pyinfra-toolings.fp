const fn main() {
    if std::facts::server::which("git") != "" {
        std::ops::server::shell("echo git present");
    }

    if std::facts::server::which("pipx") != "" {
        std::ops::pipx::ensure_path();
        std::ops::pipx::packages("ruff", latest=true);
    }

    if std::facts::files::is_directory("/srv") {
        std::ops::server_utils::wait(port="22");
    }

    let gems = std::facts::gem::packages();
    std::ops::server::shell(gems);

    let flatpaks = std::facts::flatpak::packages();
    std::ops::server::shell(flatpaks);

    with "web-1" {
        std::ops::git::pull(
            path="/srv/fp-service",
            rebase=true,
        );

        std::ops::flatpak::packages(
            "org.videolan.VLC",
            remote="flathub",
        );

        std::ops::snap::packages(
            "lxd",
            channel="4.0/stable",
            latest=true,
        );

        std::ops::zypper::packages(
            "vim",
            clean=true,
        );

        std::ops::systemd::service(
            service="fp-service",
            restarted=true,
            enabled=true,
        );
    }
}
