#[cfg(target_lang = "pwsh")]
pub const fn last_reboot() -> str {
    std::shell::process::output(
        "Get-CimInstance -ClassName Win32_OperatingSystem | Select -ExpandProperty LastBootUptime",
    )
}

#[cfg(target_lang = "pwsh")]
pub const fn os_name() -> str {
    std::shell::process::output("systeminfo.exe | findstr /c:\"OS Name:\"")
}

#[cfg(target_lang = "pwsh")]
pub const fn os_version() -> str {
    std::shell::process::output("systeminfo.exe | findstr /c:\"OS Version:\"")
}

#[cfg(target_lang = "pwsh")]
pub const fn system_type() -> str {
    std::shell::process::output("systeminfo.exe | findstr /c:\"System Type:\"")
}

#[cfg(target_lang = "pwsh")]
pub const fn bios() -> str {
    std::shell::process::output("Get-CimInstance -ClassName Win32_BIOS")
}

#[cfg(target_lang = "pwsh")]
pub const fn processors() -> str {
    std::shell::process::output(
        "Get-CimInstance -ClassName Win32_Processor | Format-List -Property *",
    )
}

#[cfg(target_lang = "pwsh")]
pub const fn local_groups() -> str {
    std::shell::process::output("cmd /c 'net localgroup | findstr [^*]'")
}

#[cfg(target_lang = "pwsh")]
pub const fn where(command: str) -> str {
    std::shell::process::output(f"where.exe {command}")
}

#[cfg(target_lang = "pwsh")]
pub const fn hotfixes() -> str {
    std::shell::process::output(
        "Get-CimInstance -ClassName Win32_QuickFixEngineering | Format-List -Property *",
    )
}

#[cfg(target_lang = "pwsh")]
pub const fn local_drives_info() -> str {
    std::shell::process::output(
        "Get-CimInstance -ClassName Win32_LogicalDisk -Filter \"DriveType=3\" | Format-List -Property *",
    )
}

#[cfg(target_lang = "pwsh")]
pub const fn logged_in_user_info() -> str {
    std::shell::process::output(
        "Get-CimInstance -ClassName Win32_ComputerSystem -Property UserName | Format-List -Property *",
    )
}

#[cfg(target_lang = "pwsh")]
pub const fn logon_session_info() -> str {
    std::shell::process::output(
        "Get-CimInstance -ClassName Win32_LogonSession | Format-List -Property *",
    )
}

#[cfg(target_lang = "pwsh")]
pub const fn aliases() -> str {
    std::shell::process::output("Get-Alias | Format-List -Property *")
}

#[cfg(target_lang = "pwsh")]
pub const fn services() -> str {
    std::shell::process::output(
        "Get-CimInstance -ClassName Win32_Service | Format-List -Property *",
    )
}

#[cfg(target_lang = "pwsh")]
pub const fn service_info(service: str) -> str {
    std::shell::process::output(f"Get-Service -Name {service} | Format-List -Property *")
}

#[cfg(target_lang = "pwsh")]
pub const fn processes() -> str {
    std::shell::process::output("Get-Process | Format-List -Property *")
}

#[cfg(target_lang = "pwsh")]
pub const fn network_configuration() -> str {
    std::shell::process::output(
        "Get-CimInstance -Class Win32_NetworkAdapterConfiguration | Format-List -Property *",
    )
}

#[cfg(target_lang = "pwsh")]
pub const fn installer_applications() -> str {
    std::shell::process::output(
        "Get-CimInstance -Class Win32_Product | Format-List -Property *",
    )
}

#[cfg(target_lang = "pwsh")]
pub const fn computer_info() -> str {
    std::shell::process::output("Get-ComputerInfo | Format-List -Property *")
}
