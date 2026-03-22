pub const fn clone(
    repo: str,
    dest: str,
    context hosts: str = "localhost",
    branch: str,
    depth: str,
    sudo: bool,
    only_if: str,
    unless: str,
    creates: str,
    removes: str,
) -> bool {
    if branch == "" {
        if depth == "" {
            std::ops::server::shell(
                f"git clone {repo} {dest}",
                hosts=hosts,
                sudo=sudo,
                only_if=only_if,
                unless=unless,
                creates=creates,
                removes=removes,
            )
        } else {
            std::ops::server::shell(
                f"git clone --depth {depth} {repo} {dest}",
                hosts=hosts,
                sudo=sudo,
                only_if=only_if,
                unless=unless,
                creates=creates,
                removes=removes,
            )
        }
    } else {
        if depth == "" {
            std::ops::server::shell(
                f"git clone --branch {branch} {repo} {dest}",
                hosts=hosts,
                sudo=sudo,
                only_if=only_if,
                unless=unless,
                creates=creates,
                removes=removes,
            )
        } else {
            std::ops::server::shell(
                f"git clone --branch {branch} --depth {depth} {repo} {dest}",
                hosts=hosts,
                sudo=sudo,
                only_if=only_if,
                unless=unless,
                creates=creates,
                removes=removes,
            )
        }
    }
}

pub const fn pull(
    path: str,
    context hosts: str = "localhost",
    rebase: bool,
    sudo: bool,
    only_if: str,
    unless: str,
    creates: str,
    removes: str,
) -> bool {
    if rebase {
        std::ops::server::shell(
            "git pull --rebase",
            hosts=hosts,
            cwd=path,
            sudo=sudo,
            only_if=only_if,
            unless=unless,
            creates=creates,
            removes=removes,
        )
    } else {
        std::ops::server::shell(
            "git pull",
            hosts=hosts,
            cwd=path,
            sudo=sudo,
            only_if=only_if,
            unless=unless,
            creates=creates,
            removes=removes,
        )
    }
}

pub const fn checkout(
    path: str,
    rev: str,
    context hosts: str = "localhost",
    sudo: bool,
    only_if: str,
    unless: str,
    creates: str,
    removes: str,
) -> bool {
    std::ops::server::shell(
        f"git checkout {rev}",
        hosts=hosts,
        cwd=path,
        sudo=sudo,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    )
}
