pub const fn virtualenv(
    path: str,
    context hosts: str = "localhost",
    python: str = "",
    venv: bool = false,
    site_packages: bool = false,
    always_copy: bool = false,
    present: bool = true,
    sudo: bool = false,
) -> bool {
    if present {
        if venv {
            if python == "" {
                std::ops::server::shell(f"python -m venv {path}", hosts=hosts, sudo=sudo)
            } else {
                std::ops::server::shell(f"{python} -m venv {path}", hosts=hosts, sudo=sudo)
            }
        } else {
            std::ops::server::shell(f"virtualenv {path}", hosts=hosts, sudo=sudo)
        }
    } else {
        std::ops::server::shell(f"rm -rf {path}", hosts=hosts, sudo=sudo)
    }
}

pub const fn packages(
    packages: str,
    context hosts: str = "localhost",
    present: bool = true,
    latest: bool = false,
    requirements: str = "",
    pip: str = "",
    virtualenv: str = "",
    sudo: bool = false,
) -> bool {
    if virtualenv != "" {
        std::ops::pip::virtualenv(path=virtualenv, hosts=hosts, sudo=sudo);
    }
    if pip == "" {
        if requirements != "" {
            if present {
                if latest {
                    std::ops::server::shell(f"pip install --upgrade -r {requirements}", hosts=hosts, sudo=sudo);
                } else {
                    std::ops::server::shell(f"pip install -r {requirements}", hosts=hosts, sudo=sudo);
                }
            } else {
                std::ops::server::shell(f"pip uninstall --yes -r {requirements}", hosts=hosts, sudo=sudo);
            }
        }
        if packages == "" {
            true
        } else {
            if present {
                if latest {
                    std::ops::server::shell(f"pip install --upgrade {packages}", hosts=hosts, sudo=sudo)
                } else {
                    std::ops::server::shell(f"pip install {packages}", hosts=hosts, sudo=sudo)
                }
            } else {
                std::ops::server::shell(f"pip uninstall --yes {packages}", hosts=hosts, sudo=sudo)
            }
        }
    } else {
        if requirements != "" {
            if present {
                if latest {
                    std::ops::server::shell(f"{pip} install --upgrade -r {requirements}", hosts=hosts, sudo=sudo);
                } else {
                    std::ops::server::shell(f"{pip} install -r {requirements}", hosts=hosts, sudo=sudo);
                }
            } else {
                std::ops::server::shell(f"{pip} uninstall --yes -r {requirements}", hosts=hosts, sudo=sudo);
            }
        }
        if packages == "" {
            true
        } else {
            if present {
                if latest {
                    std::ops::server::shell(f"{pip} install --upgrade {packages}", hosts=hosts, sudo=sudo)
                } else {
                    std::ops::server::shell(f"{pip} install {packages}", hosts=hosts, sudo=sudo)
                }
            } else {
                std::ops::server::shell(f"{pip} uninstall --yes {packages}", hosts=hosts, sudo=sudo)
            }
        }
    }
}
