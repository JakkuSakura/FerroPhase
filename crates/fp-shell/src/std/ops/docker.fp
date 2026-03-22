pub const fn container(
    container: str,
    context hosts: str = "localhost",
    image: str,
    ports: str,
    networks: str,
    volumes: str,
    env_vars: str,
    pull_always: bool,
    present: bool = true,
    force: bool,
    start: bool,
    sudo: bool,
) -> bool {
    if pull_always {
        std::ops::server::shell(f"docker pull {image}", hosts=hosts, sudo=sudo);
    }
    if present {
        if force {
            std::ops::server::shell(f"docker rm -f {container} 2>/dev/null || true", hosts=hosts, sudo=sudo);
        }
        if image != "" {
            std::ops::server::shell(
                f"docker create --name {container} {ports} {networks} {volumes} {env_vars} {image}",
                hosts=hosts,
                sudo=sudo,
            );
        }
        if start {
            std::ops::server::shell(f"docker start {container}", hosts=hosts, sudo=sudo)
        } else {
            std::ops::server::shell(f"docker stop {container} || true", hosts=hosts, sudo=sudo)
        }
    } else {
        std::ops::server::shell(f"docker rm -f {container}", hosts=hosts, sudo=sudo)
    }
}

pub const fn image(image: str, context hosts: str = "localhost", present: bool = true, sudo: bool) -> bool {
    if present {
        std::ops::server::shell(f"docker pull {image}", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"docker image rm {image}", hosts=hosts, sudo=sudo)
    }
}

pub const fn volume(volume: str, context hosts: str = "localhost", driver: str, labels: str, present: bool = true, sudo: bool) -> bool {
    if present {
        std::ops::server::shell(f"docker volume create --driver {driver} {labels} {volume}", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"docker volume rm {volume}", hosts=hosts, sudo=sudo)
    }
}

pub const fn network(network: str, context hosts: str = "localhost", driver: str, subnet: str, gateway: str, opts: str, labels: str, ingress: bool, attachable: bool, present: bool = true, sudo: bool) -> bool {
    if present {
        if ingress {
            if attachable {
                std::ops::server::shell(
                    f"docker network create --driver {driver} --subnet {subnet} --gateway {gateway} {opts} {labels} --ingress --attachable {network}",
                    hosts=hosts,
                    sudo=sudo,
                )
            } else {
                std::ops::server::shell(
                    f"docker network create --driver {driver} --subnet {subnet} --gateway {gateway} {opts} {labels} --ingress {network}",
                    hosts=hosts,
                    sudo=sudo,
                )
            }
        } else {
            if attachable {
                std::ops::server::shell(
                    f"docker network create --driver {driver} --subnet {subnet} --gateway {gateway} {opts} {labels} --attachable {network}",
                    hosts=hosts,
                    sudo=sudo,
                )
            } else {
                std::ops::server::shell(
                    f"docker network create --driver {driver} --subnet {subnet} --gateway {gateway} {opts} {labels} {network}",
                    hosts=hosts,
                    sudo=sudo,
                )
            }
        }
    } else {
        std::ops::server::shell(f"docker network rm {network}", hosts=hosts, sudo=sudo)
    }
}

pub const fn prune(context hosts: str = "localhost", all: bool, volumes: bool, filter: str, sudo: bool) -> bool {
    if all {
        if volumes {
            if filter == "" {
                std::ops::server::shell("docker system prune -f --all --volumes", hosts=hosts, sudo=sudo)
            } else {
                std::ops::server::shell(f"docker system prune -f --all --volumes --filter {filter}", hosts=hosts, sudo=sudo)
            }
        } else {
            if filter == "" {
                std::ops::server::shell("docker system prune -f --all", hosts=hosts, sudo=sudo)
            } else {
                std::ops::server::shell(f"docker system prune -f --all --filter {filter}", hosts=hosts, sudo=sudo)
            }
        }
    } else {
        if volumes {
            if filter == "" {
                std::ops::server::shell("docker system prune -f --volumes", hosts=hosts, sudo=sudo)
            } else {
                std::ops::server::shell(f"docker system prune -f --volumes --filter {filter}", hosts=hosts, sudo=sudo)
            }
        } else {
            if filter == "" {
                std::ops::server::shell("docker system prune -f", hosts=hosts, sudo=sudo)
            } else {
                std::ops::server::shell(f"docker system prune -f --filter {filter}", hosts=hosts, sudo=sudo)
            }
        }
    }
}
