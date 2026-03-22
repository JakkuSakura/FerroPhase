pub const fn update(context hosts: str = "localhost", sudo: bool = false) -> bool {
    std::ops::server::shell("brew update", hosts=hosts, sudo=sudo)
}

pub const fn upgrade(context hosts: str = "localhost", sudo: bool = false) -> bool {
    std::ops::server::shell("brew upgrade", hosts=hosts, sudo=sudo)
}

pub const fn packages(
    packages: str,
    context hosts: str = "localhost",
    present: bool = true,
    latest: bool = false,
    update: bool = false,
    upgrade: bool = false,
    sudo: bool = false,
) -> bool {
    if update {
        std::ops::brew::update(hosts=hosts, sudo=sudo);
    }
    if upgrade {
        std::ops::brew::upgrade(hosts=hosts, sudo=sudo);
    }
    if present {
        if latest {
            std::ops::server::shell(f"brew upgrade {packages}", hosts=hosts, sudo=sudo)
        } else {
            std::ops::server::shell(f"brew install {packages}", hosts=hosts, sudo=sudo)
        }
    } else {
        std::ops::server::shell(f"brew uninstall {packages}", hosts=hosts, sudo=sudo)
    }
}

pub const fn casks(
    casks: str,
    context hosts: str = "localhost",
    present: bool = true,
    latest: bool = false,
    upgrade: bool = false,
    sudo: bool = false,
) -> bool {
    if upgrade {
        std::ops::server::shell("brew upgrade --cask", hosts=hosts, sudo=sudo);
    }
    if present {
        if latest {
            std::ops::server::shell(f"brew upgrade --cask {casks}", hosts=hosts, sudo=sudo)
        } else {
            std::ops::server::shell(f"brew install --cask {casks}", hosts=hosts, sudo=sudo)
        }
    } else {
        std::ops::server::shell(f"brew uninstall --cask {casks}", hosts=hosts, sudo=sudo)
    }
}

pub const fn tap(src: str, context hosts: str = "localhost", present: bool = true, url: str = "", sudo: bool = false) -> bool {
    if present {
        if url == "" {
            std::ops::server::shell(f"brew tap {src}", hosts=hosts, sudo=sudo)
        } else {
            std::ops::server::shell(f"brew tap {src} {url}", hosts=hosts, sudo=sudo)
        }
    } else {
        std::ops::server::shell(f"brew untap {src}", hosts=hosts, sudo=sudo)
    }
}
