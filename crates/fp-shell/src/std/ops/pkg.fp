pub const fn packages(
    packages: str,
    context hosts: str = "localhost",
    present: bool = true,
    pkg_path: str,
    sudo: bool,
) -> bool {
    if present {
        if pkg_path == "" {
            std::ops::server::shell(f"pkg install -y {packages}", hosts=hosts, sudo=sudo)
        } else {
            std::ops::server::shell(
                f"PKG_PATH={pkg_path} pkg install -y {packages}",
                hosts=hosts,
                sudo=sudo,
            )
        }
    } else {
        std::ops::server::shell(f"pkg delete -y {packages}", hosts=hosts, sudo=sudo)
    }
}
