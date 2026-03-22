pub const fn update(
    context hosts: str = "localhost",
    sudo: bool = true,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    std::ops::server::shell(
        "DEBIAN_FRONTEND=noninteractive apt-get update -y",
        hosts=hosts,
        sudo=sudo,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    )
}

pub const fn upgrade(
    context hosts: str = "localhost",
    sudo: bool = true,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    std::ops::server::shell(
        "DEBIAN_FRONTEND=noninteractive apt-get upgrade -y",
        hosts=hosts,
        sudo=sudo,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    )
}

pub const fn packages(
    packages: str,
    context hosts: str = "localhost",
    present: bool = true,
    latest: bool = false,
    update: bool = false,
    sudo: bool = true,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    if update {
        std::ops::apt::update(
            hosts=hosts,
            sudo=sudo,
            only_if=only_if,
            unless=unless,
            creates=creates,
            removes=removes,
        );
    }
    if present {
        if latest {
            std::ops::server::shell(
                f"DEBIAN_FRONTEND=noninteractive apt-get install -y --only-upgrade {packages}",
                hosts=hosts,
                sudo=sudo,
                only_if=only_if,
                unless=unless,
                creates=creates,
                removes=removes,
            )
        } else {
            std::ops::server::shell(
                f"DEBIAN_FRONTEND=noninteractive apt-get install -y {packages}",
                hosts=hosts,
                sudo=sudo,
                only_if=only_if,
                unless=unless,
                creates=creates,
                removes=removes,
            )
        }
    } else {
        std::ops::server::shell(
            f"DEBIAN_FRONTEND=noninteractive apt-get remove -y {packages}",
            hosts=hosts,
            sudo=sudo,
            only_if=only_if,
            unless=unless,
            creates=creates,
            removes=removes,
        )
    }
}

pub const fn repo(
    src: str,
    context hosts: str = "localhost",
    filename: str = "",
    present: bool = true,
    sudo: bool = true,
) -> bool {
    if filename == "" {
        if present {
            std::ops::server::shell(
                f"printf '%s\n' {src} >> /etc/apt/sources.list",
                hosts=hosts,
                sudo=sudo,
            )
        } else {
            std::ops::server::shell(
                f"grep -v -F {src} /etc/apt/sources.list > /etc/apt/sources.list.fp-shell && mv /etc/apt/sources.list.fp-shell /etc/apt/sources.list",
                hosts=hosts,
                sudo=sudo,
            )
        }
    } else {
        if present {
            std::ops::server::shell(
                f"printf '%s\n' {src} >> /etc/apt/sources.list.d/{filename}.list",
                hosts=hosts,
                sudo=sudo,
            )
        } else {
            std::ops::server::shell(
                f"grep -v -F {src} /etc/apt/sources.list.d/{filename}.list > /etc/apt/sources.list.d/{filename}.list.fp-shell && mv /etc/apt/sources.list.d/{filename}.list.fp-shell /etc/apt/sources.list.d/{filename}.list",
                hosts=hosts,
                sudo=sudo,
            )
        }
    }
}
