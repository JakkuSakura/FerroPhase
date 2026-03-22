pub const fn cpus() -> str {
    std::shell::process::output("getconf NPROCESSORS_ONLN 2> /dev/null || getconf _NPROCESSORS_ONLN")
}

pub const fn memory() -> str {
    std::shell::process::output("vmstat -s")
}

pub const fn block_devices() -> str {
    std::shell::process::output("df")
}

pub const fn network_devices() -> str {
    std::shell::process::output("ip addr show 2> /dev/null || ifconfig -a")
}
