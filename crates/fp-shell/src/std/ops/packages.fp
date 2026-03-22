pub const fn apt_update(
    context hosts: str = "localhost",
    sudo: bool = true,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    std::ops::server::shell(
        "apt-get update",
        hosts=hosts,
        sudo=sudo,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    )
}

pub const fn apt_install(
    packages: str,
    context hosts: str = "localhost",
    sudo: bool = true,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    std::ops::server::shell(
        f"apt-get install -y {packages}",
        hosts=hosts,
        sudo=sudo,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    )
}

pub const fn yum_install(
    packages: str,
    context hosts: str = "localhost",
    sudo: bool = true,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    std::ops::server::shell(
        f"yum install -y {packages}",
        hosts=hosts,
        sudo=sudo,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    )
}

pub const fn dnf_install(
    packages: str,
    context hosts: str = "localhost",
    sudo: bool = true,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    std::ops::server::shell(
        f"dnf install -y {packages}",
        hosts=hosts,
        sudo=sudo,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    )
}

pub const fn apk_add(
    packages: str,
    context hosts: str = "localhost",
    sudo: bool = true,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    std::ops::server::shell(
        f"apk add {packages}",
        hosts=hosts,
        sudo=sudo,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    )
}

pub const fn pacman_sync(
    packages: str,
    context hosts: str = "localhost",
    sudo: bool = true,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    std::ops::server::shell(
        f"pacman -S --noconfirm {packages}",
        hosts=hosts,
        sudo=sudo,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    )
}

pub const fn brew_install(
    packages: str,
    context hosts: str = "localhost",
    sudo: bool = false,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    std::ops::server::shell(
        f"brew install {packages}",
        hosts=hosts,
        sudo=sudo,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    )
}

pub const fn choco_install(
    packages: str,
    context hosts: str = "localhost",
    sudo: bool = false,
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    std::ops::server::shell(
        f"choco install -y {packages}",
        hosts=hosts,
        sudo=sudo,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    )
}
