pub const fn restart(
    name: str,
    context hosts: str = "localhost",
    sudo: bool = true,
    only_if: str,
    unless: str,
    creates: str,
    removes: str,
) -> bool {
    std::ops::server::shell(
        f"systemctl restart {name}",
        hosts=hosts,
        sudo=sudo,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    )
}
