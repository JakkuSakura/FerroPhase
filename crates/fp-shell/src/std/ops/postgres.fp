pub const fn sql(sql: str, context hosts: str = "localhost", psql_user: str, psql_password: str, psql_host: str, psql_port: str, psql_database: str, sudo: bool) -> bool {
    let auth = psql_auth_args(psql_user, psql_password, psql_host, psql_port, psql_database);
    std::ops::server::shell(f"{auth} -Ac {sql}", hosts=hosts, sudo=sudo)
}

pub const fn role(role: str, context hosts: str = "localhost", present: bool = true, password: str, login: bool, superuser: bool, inherit: bool, createdb: bool, createrole: bool, replication: bool, connection_limit: str, psql_user: str, psql_password: str, psql_host: str, psql_port: str, psql_database: str, sudo: bool) -> bool {
    let auth = psql_auth_args(psql_user, psql_password, psql_host, psql_port, psql_database);
    if present {
        std::ops::server::shell(
            f"{auth} -Ac \"CREATE ROLE \\\"{role}\\\" LOGIN {login} SUPERUSER {superuser} INHERIT {inherit} CREATEDB {createdb} CREATEROLE {createrole} REPLICATION {replication} CONNECTION LIMIT {connection_limit} PASSWORD '{password}'\"",
            hosts=hosts,
            sudo=sudo,
        )
    } else {
        std::ops::server::shell(f"{auth} -Ac \"DROP ROLE \\\"{role}\\\"\"", hosts=hosts, sudo=sudo)
    }
}

pub const fn database(database: str, context hosts: str = "localhost", present: bool = true, owner: str, template: str, encoding: str, lc_collate: str, lc_ctype: str, tablespace: str, connection_limit: str, psql_user: str, psql_password: str, psql_host: str, psql_port: str, psql_database: str, sudo: bool) -> bool {
    let auth = psql_auth_args(psql_user, psql_password, psql_host, psql_port, psql_database);
    if present {
        std::ops::server::shell(
            f"{auth} -Ac \"CREATE DATABASE \\\"{database}\\\" OWNER \\\"{owner}\\\" TEMPLATE {template} ENCODING '{encoding}' LC_COLLATE '{lc_collate}' LC_CTYPE '{lc_ctype}' TABLESPACE {tablespace} CONNECTION LIMIT {connection_limit}\"",
            hosts=hosts,
            sudo=sudo,
        )
    } else {
        std::ops::server::shell(f"{auth} -Ac \"DROP DATABASE \\\"{database}\\\"\"", hosts=hosts, sudo=sudo)
    }
}

pub const fn dump(filename: str, context hosts: str = "localhost", database: str, psql_user: str, psql_password: str, psql_host: str, psql_port: str, sudo: bool) -> bool {
    let auth = psql_auth_args(psql_user, psql_password, psql_host, psql_port, database);
    std::ops::server::shell(f"{auth} --format=custom > {filename}", hosts=hosts, sudo=sudo)
}

pub const fn load(filename: str, context hosts: str = "localhost", database: str, psql_user: str, psql_password: str, psql_host: str, psql_port: str, sudo: bool) -> bool {
    let auth = psql_auth_args(psql_user, psql_password, psql_host, psql_port, database);
    std::ops::server::shell(f"{auth} < {filename}", hosts=hosts, sudo=sudo)
}

const fn psql_auth_args(user: str, password: str, host: str, port: str, database: str) -> str {
    if password == "" {
        if database == "" {
            if user == "" {
                if host == "" { if port == "" { "psql" } else { f"psql -p {port}" } } else { if port == "" { f"psql -h {host}" } else { f"psql -h {host} -p {port}" } }
            } else {
                if host == "" { if port == "" { f"psql -U {user}" } else { f"psql -U {user} -p {port}" } } else { if port == "" { f"psql -U {user} -h {host}" } else { f"psql -U {user} -h {host} -p {port}" } }
            }
        } else {
            if user == "" {
                if host == "" { if port == "" { f"psql -d {database}" } else { f"psql -d {database} -p {port}" } } else { if port == "" { f"psql -d {database} -h {host}" } else { f"psql -d {database} -h {host} -p {port}" } }
            } else {
                if host == "" { if port == "" { f"psql -d {database} -U {user}" } else { f"psql -d {database} -U {user} -p {port}" } } else { if port == "" { f"psql -d {database} -U {user} -h {host}" } else { f"psql -d {database} -U {user} -h {host} -p {port}" } }
            }
        }
    } else {
        if database == "" {
            if user == "" {
                if host == "" { if port == "" { f"PGPASSWORD=\\\"{password}\\\" psql" } else { f"PGPASSWORD=\\\"{password}\\\" psql -p {port}" } } else { if port == "" { f"PGPASSWORD=\\\"{password}\\\" psql -h {host}" } else { f"PGPASSWORD=\\\"{password}\\\" psql -h {host} -p {port}" } }
            } else {
                if host == "" { if port == "" { f"PGPASSWORD=\\\"{password}\\\" psql -U {user}" } else { f"PGPASSWORD=\\\"{password}\\\" psql -U {user} -p {port}" } } else { if port == "" { f"PGPASSWORD=\\\"{password}\\\" psql -U {user} -h {host}" } else { f"PGPASSWORD=\\\"{password}\\\" psql -U {user} -h {host} -p {port}" } }
            }
        } else {
            if user == "" {
                if host == "" { if port == "" { f"PGPASSWORD=\\\"{password}\\\" psql -d {database}" } else { f"PGPASSWORD=\\\"{password}\\\" psql -d {database} -p {port}" } } else { if port == "" { f"PGPASSWORD=\\\"{password}\\\" psql -d {database} -h {host}" } else { f"PGPASSWORD=\\\"{password}\\\" psql -d {database} -h {host} -p {port}" } }
            } else {
                if host == "" { if port == "" { f"PGPASSWORD=\\\"{password}\\\" psql -d {database} -U {user}" } else { f"PGPASSWORD=\\\"{password}\\\" psql -d {database} -U {user} -p {port}" } } else { if port == "" { f"PGPASSWORD=\\\"{password}\\\" psql -d {database} -U {user} -h {host}" } else { f"PGPASSWORD=\\\"{password}\\\" psql -d {database} -U {user} -h {host} -p {port}" } }
            }
        }
    }
}
