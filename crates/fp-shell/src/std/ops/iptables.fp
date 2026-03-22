pub const fn chain(chain: str, context hosts: str = "localhost", present: bool = true, table: str, policy: str, version: str, sudo: bool) -> bool {
    if version == "6" {
        if present {
            std::ops::server::shell(f"ip6tables -t {table} -N {chain} || true", hosts=hosts, sudo=sudo);
            if policy != "" {
                std::ops::server::shell(f"ip6tables -t {table} -P {chain} {policy}", hosts=hosts, sudo=sudo);
            }
            true
        } else {
            std::ops::server::shell(f"ip6tables -t {table} -X {chain}", hosts=hosts, sudo=sudo)
        }
    } else {
        if present {
            std::ops::server::shell(f"iptables -t {table} -N {chain} || true", hosts=hosts, sudo=sudo);
            if policy != "" {
                std::ops::server::shell(f"iptables -t {table} -P {chain} {policy}", hosts=hosts, sudo=sudo);
            }
            true
        } else {
            std::ops::server::shell(f"iptables -t {table} -X {chain}", hosts=hosts, sudo=sudo)
        }
    }
}

pub const fn rule(
    chain: str,
    jump: str,
    context hosts: str = "localhost",
    present: bool = true,
    table: str,
    append: bool = true,
    version: str,
    protocol: str,
    not_protocol: str,
    source: str,
    not_source: str,
    destination: str,
    not_destination: str,
    in_interface: str,
    not_in_interface: str,
    out_interface: str,
    not_out_interface: str,
    to_destination: str,
    to_source: str,
    to_ports: str,
    log_prefix: str,
    destination_port: str,
    source_port: str,
    extras: str,
    sudo: bool,
) -> bool {
    if version == "6" {
        if present {
            if append {
                std::ops::server::shell(
                    f"ip6tables -t {table} -A {chain} {protocol} {not_protocol} {source} {not_source} {destination} {not_destination} {in_interface} {not_in_interface} {out_interface} {not_out_interface} {extras} --dport {destination_port} --sport {source_port} -j {jump} --log-prefix {log_prefix} --to-destination {to_destination} --to-source {to_source} --to-ports {to_ports}",
                    hosts=hosts,
                    sudo=sudo,
                )
            } else {
                std::ops::server::shell(
                    f"ip6tables -t {table} -I {chain} {protocol} {not_protocol} {source} {not_source} {destination} {not_destination} {in_interface} {not_in_interface} {out_interface} {not_out_interface} {extras} --dport {destination_port} --sport {source_port} -j {jump} --log-prefix {log_prefix} --to-destination {to_destination} --to-source {to_source} --to-ports {to_ports}",
                    hosts=hosts,
                    sudo=sudo,
                )
            }
        } else {
            std::ops::server::shell(
                f"ip6tables -t {table} -D {chain} {protocol} {not_protocol} {source} {not_source} {destination} {not_destination} {in_interface} {not_in_interface} {out_interface} {not_out_interface} {extras} --dport {destination_port} --sport {source_port} -j {jump} --log-prefix {log_prefix} --to-destination {to_destination} --to-source {to_source} --to-ports {to_ports}",
                hosts=hosts,
                sudo=sudo,
            )
        }
    } else {
        if present {
            if append {
                std::ops::server::shell(
                    f"iptables -t {table} -A {chain} {protocol} {not_protocol} {source} {not_source} {destination} {not_destination} {in_interface} {not_in_interface} {out_interface} {not_out_interface} {extras} --dport {destination_port} --sport {source_port} -j {jump} --log-prefix {log_prefix} --to-destination {to_destination} --to-source {to_source} --to-ports {to_ports}",
                    hosts=hosts,
                    sudo=sudo,
                )
            } else {
                std::ops::server::shell(
                    f"iptables -t {table} -I {chain} {protocol} {not_protocol} {source} {not_source} {destination} {not_destination} {in_interface} {not_in_interface} {out_interface} {not_out_interface} {extras} --dport {destination_port} --sport {source_port} -j {jump} --log-prefix {log_prefix} --to-destination {to_destination} --to-source {to_source} --to-ports {to_ports}",
                    hosts=hosts,
                    sudo=sudo,
                )
            }
        } else {
            std::ops::server::shell(
                f"iptables -t {table} -D {chain} {protocol} {not_protocol} {source} {not_source} {destination} {not_destination} {in_interface} {not_in_interface} {out_interface} {not_out_interface} {extras} --dport {destination_port} --sport {source_port} -j {jump} --log-prefix {log_prefix} --to-destination {to_destination} --to-source {to_source} --to-ports {to_ports}",
                hosts=hosts,
                sudo=sudo,
            )
        }
    }
}
