pub const fn keyscan(hostname: str, context hosts: str = "localhost", force: bool, port: str, sudo: bool) -> bool {
    std::ops::server::shell("mkdir -p ~/.ssh", hosts=hosts, sudo=sudo);
    if force {
        std::ops::server::shell(f"ssh-keygen -R {hostname} || true; ssh-keyscan -p {port} {hostname} >> ~/.ssh/known_hosts", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"grep -q {hostname} ~/.ssh/known_hosts 2>/dev/null || ssh-keyscan -p {port} {hostname} >> ~/.ssh/known_hosts", hosts=hosts, sudo=sudo)
    }
}

pub const fn command(hostname: str, command: str, context hosts: str = "localhost", user: str, port: str, sudo: bool) -> bool {
    if user == "" {
        std::ops::server::shell(f"ssh -p {port} {hostname} {command}", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"ssh -p {port} {user}@{hostname} {command}", hosts=hosts, sudo=sudo)
    }
}

pub const fn upload(hostname: str, filename: str, context hosts: str = "localhost", remote_filename: str, port: str, user: str, use_remote_sudo: bool, ssh_keyscan: bool, sudo: bool) -> bool {
    if ssh_keyscan {
        std::ops::ssh::keyscan(hostname, hosts=hosts, port=port, sudo=sudo);
    }
    if user == "" {
        std::ops::server::shell(f"scp -P {port} {filename} {hostname}:{remote_filename}", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"scp -P {port} {filename} {user}@{hostname}:{remote_filename}", hosts=hosts, sudo=sudo)
    }
}

pub const fn download(hostname: str, filename: str, context hosts: str = "localhost", local_filename: str, force: bool, port: str, user: str, ssh_keyscan: bool, sudo: bool) -> bool {
    if ssh_keyscan {
        std::ops::ssh::keyscan(hostname, hosts=hosts, port=port, sudo=sudo);
    }
    if user == "" {
        std::ops::server::shell(f"scp -P {port} {hostname}:{filename} {local_filename}", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"scp -P {port} {user}@{hostname}:{filename} {local_filename}", hosts=hosts, sudo=sudo)
    }
}
