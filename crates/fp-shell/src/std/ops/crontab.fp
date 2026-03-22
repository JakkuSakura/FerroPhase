pub const fn crontab(
    command: str,
    context hosts: str = "localhost",
    present: bool = true,
    user: str,
    cron_name: str,
    minute: str,
    hour: str,
    month: str,
    day_of_week: str,
    day_of_month: str,
    special_time: str,
    interpolate_variables: bool,
    sudo: bool,
) -> bool {
    let temp = "/tmp/fp-shell-crontab";
    if user == "" {
        if special_time == "" {
            if cron_name == "" {
                if present {
                    std::ops::server::shell(
                        f"crontab -l 2>/dev/null > {temp} || true; grep -qF {command} {temp} || (printf '%s\\n' {command} >> {temp}; printf '%s\\n' '{minute} {hour} {day_of_month} {month} {day_of_week} {command}' >> {temp}); crontab {temp}",
                        hosts=hosts,
                        sudo=sudo,
                    )
                } else {
                    std::ops::server::shell(
                        f"crontab -l 2>/dev/null > {temp} || true; grep -vF {command} {temp} > {temp}.next || true; crontab {temp}.next",
                        hosts=hosts,
                        sudo=sudo,
                    )
                }
            } else {
                if present {
                    std::ops::server::shell(
                        f"crontab -l 2>/dev/null > {temp} || true; grep -qF '# pyinfra-name={cron_name}' {temp} || (printf '%s\\n' '# pyinfra-name={cron_name}' >> {temp}; printf '%s\\n' '{minute} {hour} {day_of_month} {month} {day_of_week} {command}' >> {temp}); crontab {temp}",
                        hosts=hosts,
                        sudo=sudo,
                    )
                } else {
                    std::ops::server::shell(
                        f"crontab -l 2>/dev/null > {temp} || true; grep -vF {command} {temp} | grep -vF '# pyinfra-name={cron_name}' > {temp}.next || true; crontab {temp}.next",
                        hosts=hosts,
                        sudo=sudo,
                    )
                }
            }
        } else {
            if present {
                std::ops::server::shell(
                    f"crontab -l 2>/dev/null > {temp} || true; grep -qF {command} {temp} || printf '%s\\n' '{special_time} {command}' >> {temp}; crontab {temp}",
                    hosts=hosts,
                    sudo=sudo,
                )
            } else {
                std::ops::server::shell(
                    f"crontab -l 2>/dev/null > {temp} || true; grep -vF {command} {temp} > {temp}.next || true; crontab {temp}.next",
                    hosts=hosts,
                    sudo=sudo,
                )
            }
        }
    } else {
        if special_time == "" {
            if present {
                std::ops::server::shell(
                    f"crontab -l -u {user} 2>/dev/null > {temp} || true; grep -qF {command} {temp} || printf '%s\\n' '{minute} {hour} {day_of_month} {month} {day_of_week} {command}' >> {temp}; crontab -u {user} {temp}",
                    hosts=hosts,
                    sudo=sudo,
                )
            } else {
                std::ops::server::shell(
                    f"crontab -l -u {user} 2>/dev/null > {temp} || true; grep -vF {command} {temp} > {temp}.next || true; crontab -u {user} {temp}.next",
                    hosts=hosts,
                    sudo=sudo,
                )
            }
        } else {
            if present {
                std::ops::server::shell(
                    f"crontab -l -u {user} 2>/dev/null > {temp} || true; grep -qF {command} {temp} || printf '%s\\n' '{special_time} {command}' >> {temp}; crontab -u {user} {temp}",
                    hosts=hosts,
                    sudo=sudo,
                )
            } else {
                std::ops::server::shell(
                    f"crontab -l -u {user} 2>/dev/null > {temp} || true; grep -vF {command} {temp} > {temp}.next || true; crontab -u {user} {temp}.next",
                    hosts=hosts,
                    sudo=sudo,
                )
            }
        }
    }
}
