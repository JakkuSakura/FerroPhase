pub const fn chains(table: str, version: str) -> str {
    if version == "6" {
        std::shell::process::output(f"ip6tables -t {table} -S")
    } else {
        std::shell::process::output(f"iptables -t {table} -S")
    }
}

pub const fn rules(table: str, version: str) -> str {
    if version == "6" {
        std::shell::process::output(f"ip6tables-save -t {table}")
    } else {
        std::shell::process::output(f"iptables-save -t {table}")
    }
}
