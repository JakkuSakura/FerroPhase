pub const fn sql(sql: str, context hosts: str = "localhost", database: str, mysql_user: str, mysql_password: str, mysql_host: str, mysql_port: str, sudo: bool) -> bool {
    let auth = mysql_auth_args(database, mysql_user, mysql_password, mysql_host, mysql_port);
    std::ops::server::shell(f"mysql {auth} -Be {sql}", hosts=hosts, sudo=sudo)
}

pub const fn user(user: str, context hosts: str = "localhost", present: bool = true, user_hostname: str, password: str, privileges: str, mysql_user: str, mysql_password: str, mysql_host: str, mysql_port: str, sudo: bool) -> bool {
    let auth = mysql_auth_args("", mysql_user, mysql_password, mysql_host, mysql_port);
    if present {
        std::ops::server::shell(f"mysql {auth} -Be \"CREATE USER IF NOT EXISTS '{user}'@'{user_hostname}' IDENTIFIED BY '{password}'\"", hosts=hosts, sudo=sudo);
        if privileges != "" {
            std::ops::server::shell(f"mysql {auth} -Be \"GRANT {privileges} ON *.* TO '{user}'@'{user_hostname}'\"", hosts=hosts, sudo=sudo);
        }
        true
    } else {
        std::ops::server::shell(f"mysql {auth} -Be \"DROP USER '{user}'@'{user_hostname}'\"", hosts=hosts, sudo=sudo)
    }
}

pub const fn database(database: str, context hosts: str = "localhost", present: bool = true, collate: str, charset: str, user: str, user_hostname: str, user_privileges: str, mysql_user: str, mysql_password: str, mysql_host: str, mysql_port: str, sudo: bool) -> bool {
    let auth = mysql_auth_args("", mysql_user, mysql_password, mysql_host, mysql_port);
    if present {
        if charset == "" {
            std::ops::server::shell(f"mysql {auth} -Be \"CREATE DATABASE IF NOT EXISTS {database}\"", hosts=hosts, sudo=sudo);
        } else {
            std::ops::server::shell(
                f"mysql {auth} -Be \"CREATE DATABASE IF NOT EXISTS {database} CHARACTER SET {charset} COLLATE {collate}\"",
                hosts=hosts,
                sudo=sudo,
            );
        }
        if user != "" {
            std::ops::server::shell(
                f"mysql {auth} -Be \"GRANT {user_privileges} ON {database}.* TO '{user}'@'{user_hostname}'\"",
                hosts=hosts,
                sudo=sudo,
            );
        }
        true
    } else {
        std::ops::server::shell(f"mysql {auth} -Be \"DROP DATABASE {database}\"", hosts=hosts, sudo=sudo)
    }
}

const fn mysql_auth_args(database: str, user: str, password: str, host: str, port: str) -> str {
    if database == "" {
        if user == "" {
            if password == "" {
                if host == "" {
                    if port == "" { "" } else { f"-P{port}" }
                } else {
                    if port == "" { f"-h{host}" } else { f"-h{host} -P{port}" }
                }
            } else {
                if host == "" {
                    if port == "" { f"-p\\\"{password}\\\"" } else { f"-p\\\"{password}\\\" -P{port}" }
                } else {
                    if port == "" { f"-p\\\"{password}\\\" -h{host}" } else { f"-p\\\"{password}\\\" -h{host} -P{port}" }
                }
            }
        } else {
            if password == "" {
                if host == "" {
                    if port == "" { f"-u\\\"{user}\\\"" } else { f"-u\\\"{user}\\\" -P{port}" }
                } else {
                    if port == "" { f"-u\\\"{user}\\\" -h{host}" } else { f"-u\\\"{user}\\\" -h{host} -P{port}" }
                }
            } else {
                if host == "" {
                    if port == "" { f"-u\\\"{user}\\\" -p\\\"{password}\\\"" } else { f"-u\\\"{user}\\\" -p\\\"{password}\\\" -P{port}" }
                } else {
                    if port == "" { f"-u\\\"{user}\\\" -p\\\"{password}\\\" -h{host}" } else { f"-u\\\"{user}\\\" -p\\\"{password}\\\" -h{host} -P{port}" }
                }
            }
        }
    } else {
        let base = mysql_auth_args("", user, password, host, port);
        if base == "" { database } else { f"{database} {base}" }
    }
}
