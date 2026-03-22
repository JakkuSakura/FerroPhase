pub const fn update(context hosts: str = "localhost", sudo: bool = true) -> bool {
    std::ops::server::shell("zypper update -y", hosts=hosts, sudo=sudo)
}

pub const fn key(src: str, context hosts: str = "localhost", sudo: bool = true) -> bool {
    std::ops::server::shell(f"rpm --import {src}", hosts=hosts, sudo=sudo)
}

pub const fn repo(
    src: str,
    context hosts: str = "localhost",
    baseurl: str,
    present: bool = true,
    description: str,
    enabled: bool,
    gpgcheck: bool,
    gpgkey: str,
    repo_type: str,
    sudo: bool = true,
) -> bool {
    if present {
        if baseurl == "" {
            std::ops::server::shell(
                f"curl -fsSL {src} -o /etc/zypp/repos.d/fp-shell.repo",
                hosts=hosts,
                sudo=sudo,
            )
        } else {
            if enabled {
                if gpgcheck {
                    std::ops::server::shell(
                        f"printf '[{src}]\\nname={description}\\nenabled=1\\ngpgcheck=1\\ngpgkey={gpgkey}\\nbaseurl={baseurl}\\ntype={repo_type}\\n' > /etc/zypp/repos.d/{src}.repo",
                        hosts=hosts,
                        sudo=sudo,
                    )
                } else {
                    std::ops::server::shell(
                        f"printf '[{src}]\\nname={description}\\nenabled=1\\ngpgcheck=0\\ngpgkey={gpgkey}\\nbaseurl={baseurl}\\ntype={repo_type}\\n' > /etc/zypp/repos.d/{src}.repo",
                        hosts=hosts,
                        sudo=sudo,
                    )
                }
            } else {
                if gpgcheck {
                    std::ops::server::shell(
                        f"printf '[{src}]\\nname={description}\\nenabled=0\\ngpgcheck=1\\ngpgkey={gpgkey}\\nbaseurl={baseurl}\\ntype={repo_type}\\n' > /etc/zypp/repos.d/{src}.repo",
                        hosts=hosts,
                        sudo=sudo,
                    )
                } else {
                    std::ops::server::shell(
                        f"printf '[{src}]\\nname={description}\\nenabled=0\\ngpgcheck=0\\ngpgkey={gpgkey}\\nbaseurl={baseurl}\\ntype={repo_type}\\n' > /etc/zypp/repos.d/{src}.repo",
                        hosts=hosts,
                        sudo=sudo,
                    )
                }
            }
        }
    } else {
        std::ops::server::shell(
            f"rm -f /etc/zypp/repos.d/{src}.repo /etc/zypp/repos.d/fp-shell.repo",
            hosts=hosts,
            sudo=sudo,
        )
    }
}

pub const fn packages(
    packages: str,
    context hosts: str = "localhost",
    present: bool = true,
    latest: bool,
    update: bool,
    clean: bool,
    sudo: bool = true,
) -> bool {
    if clean {
        std::ops::server::shell("zypper clean --all", hosts=hosts, sudo=sudo);
    }
    if update {
        std::ops::zypper::update(hosts=hosts, sudo=sudo);
    }
    if present {
        if latest {
            std::ops::server::shell(
                f"zypper update -y {packages}",
                hosts=hosts,
                sudo=sudo,
            )
        } else {
            std::ops::server::shell(
                f"zypper --non-interactive install -y {packages}",
                hosts=hosts,
                sudo=sudo,
            )
        }
    } else {
        std::ops::server::shell(
            f"zypper --non-interactive remove -y {packages}",
            hosts=hosts,
            sudo=sudo,
        )
    }
}
