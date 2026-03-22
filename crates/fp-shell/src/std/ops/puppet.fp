pub const fn agent(context hosts: str = "localhost", server: str, port: str, sudo: bool) -> bool {
    if server == "" {
        if port == "" {
            std::ops::server::shell("puppet agent -t", hosts=hosts, sudo=sudo)
        } else {
            std::ops::server::shell(f"puppet agent -t --masterport={port}", hosts=hosts, sudo=sudo)
        }
    } else {
        if port == "" {
            std::ops::server::shell(f"puppet agent -t --server={server}", hosts=hosts, sudo=sudo)
        } else {
            std::ops::server::shell(f"puppet agent -t --server={server} --masterport={port}", hosts=hosts, sudo=sudo)
        }
    }
}
