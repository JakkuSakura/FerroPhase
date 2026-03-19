pub mod service {
    #[defaults(hosts = "localhost", sudo = true)]
    pub const fn restart(
        name: str,
        context hosts: str,
        sudo: bool,
        only_if: str,
        unless: str,
        creates: str,
        removes: str,
    ) -> bool {
        std::server::shell(
            f"systemctl restart {name}",
            hosts=hosts,
            sudo=sudo,
            only_if=only_if,
            unless=unless,
            creates=creates,
            removes=removes,
        )
    }
}
