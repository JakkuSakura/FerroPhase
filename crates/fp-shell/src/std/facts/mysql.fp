pub const fn databases(user: str, password: str, host: str, port: str) -> str {
    let auth = mysql_auth_args(user, password, host, port);
    std::shell::process::output(f"mysql {auth} -Be \"SELECT * FROM information_schema.SCHEMATA\"")
}

pub const fn users(user: str, password: str, host: str, port: str) -> str {
    let auth = mysql_auth_args(user, password, host, port);
    std::shell::process::output(f"mysql {auth} -Be \"SELECT * FROM mysql.user\"")
}

pub const fn user_grants(target_user: str, target_host: str, user: str, password: str, host: str, port: str) -> str {
    let auth = mysql_auth_args(user, password, host, port);
    std::shell::process::output(
        f"mysql {auth} -Be \"SHOW GRANTS FOR '{target_user}'@'{target_host}'\" || true"
    )
}

const fn mysql_auth_args(user: str, password: str, host: str, port: str) -> str {
    if user == "" {
        ""
    } else {
        if password == "" {
            if host == "" {
                if port == "" {
                    f"-u\\\"{user}\\\""
                } else {
                    f"-u\\\"{user}\\\" -P{port}"
                }
            } else {
                if port == "" {
                    f"-u\\\"{user}\\\" -h{host}"
                } else {
                    f"-u\\\"{user}\\\" -h{host} -P{port}"
                }
            }
        } else {
            if host == "" {
                if port == "" {
                    f"-u\\\"{user}\\\" -p\\\"{password}\\\""
                } else {
                    f"-u\\\"{user}\\\" -p\\\"{password}\\\" -P{port}"
                }
            } else {
                if port == "" {
                    f"-u\\\"{user}\\\" -p\\\"{password}\\\" -h{host}"
                } else {
                    f"-u\\\"{user}\\\" -p\\\"{password}\\\" -h{host} -P{port}"
                }
            }
        }
    }
}
