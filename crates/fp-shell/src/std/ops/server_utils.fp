pub const fn wait(
    port: str,
    context hosts: str = "localhost",
    interval: str,
    sudo: bool,
) -> bool {
    if interval == "" {
        std::ops::server::shell(
            f"while ! (netstat -an | grep LISTEN | grep -e '\\\\.{port}' -e ':{port}'); do echo waiting for port {port}; sleep 1; done",
            hosts=hosts,
            sudo=sudo,
        )
    } else {
        std::ops::server::shell(
            f"while ! (netstat -an | grep LISTEN | grep -e '\\\\.{port}' -e ':{port}'); do echo waiting for port {port}; sleep {interval}; done",
            hosts=hosts,
            sudo=sudo,
        )
    }
}

pub const fn reboot(
    context hosts: str = "localhost",
    sudo: bool,
    only_if: str,
    unless: str,
    creates: str,
    removes: str,
) -> bool {
    std::ops::server::shell(
        "reboot",
        hosts=hosts,
        sudo=sudo,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    )
}
