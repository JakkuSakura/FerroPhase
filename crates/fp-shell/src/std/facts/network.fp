#[cfg(target_lang = "bash")]
pub const fn interfaces() -> str {
    std::shell::process::output("ip -o link show 2>/dev/null || ifconfig -a")
}

#[cfg(target_lang = "pwsh")]
pub const fn interfaces() -> str {
    std::shell::process::output("Get-NetAdapter | Format-Table -AutoSize Name,Status,MacAddress,LinkSpeed")
}

#[cfg(target_lang = "bash")]
pub const fn addresses() -> str {
    std::shell::process::output("ip addr show 2>/dev/null || ifconfig -a")
}

#[cfg(target_lang = "pwsh")]
pub const fn addresses() -> str {
    std::shell::process::output(
        "Get-NetIPAddress | Format-Table -AutoSize IPAddress,InterfaceAlias,AddressFamily",
    )
}

#[cfg(target_lang = "bash")]
pub const fn routes() -> str {
    std::shell::process::output("ip route show 2>/dev/null || netstat -rn")
}

#[cfg(target_lang = "pwsh")]
pub const fn routes() -> str {
    std::shell::process::output("Get-NetRoute | Format-Table -AutoSize DestinationPrefix,NextHop,InterfaceAlias")
}
