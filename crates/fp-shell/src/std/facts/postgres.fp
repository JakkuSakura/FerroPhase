pub const fn roles(user: str, password: str, host: str, port: str, database: str) -> str {
    let auth = psql_auth_args(user, password, host, port, database);
    std::shell::process::output(f"{auth} -Ac \"SELECT * FROM pg_catalog.pg_roles\"")
}

pub const fn databases(user: str, password: str, host: str, port: str, database: str) -> str {
    let auth = psql_auth_args(user, password, host, port, database);
    std::shell::process::output(
        f"{auth} -Ac \"SELECT pg_catalog.pg_encoding_to_char(encoding), *, pg_catalog.pg_get_userbyid(datdba) AS owner FROM pg_catalog.pg_database\""
    )
}

const fn psql_auth_args(user: str, password: str, host: str, port: str, database: str) -> str {
    if password == "" {
        if database == "" {
            if user == "" {
                if host == "" {
                    if port == "" {
                        "psql"
                    } else {
                        f"psql -p {port}"
                    }
                } else {
                    if port == "" {
                        f"psql -h {host}"
                    } else {
                        f"psql -h {host} -p {port}"
                    }
                }
            } else {
                if host == "" {
                    if port == "" {
                        f"psql -U {user}"
                    } else {
                        f"psql -U {user} -p {port}"
                    }
                } else {
                    if port == "" {
                        f"psql -U {user} -h {host}"
                    } else {
                        f"psql -U {user} -h {host} -p {port}"
                    }
                }
            }
        } else {
            if user == "" {
                if host == "" {
                    if port == "" {
                        f"psql -d {database}"
                    } else {
                        f"psql -d {database} -p {port}"
                    }
                } else {
                    if port == "" {
                        f"psql -d {database} -h {host}"
                    } else {
                        f"psql -d {database} -h {host} -p {port}"
                    }
                }
            } else {
                if host == "" {
                    if port == "" {
                        f"psql -d {database} -U {user}"
                    } else {
                        f"psql -d {database} -U {user} -p {port}"
                    }
                } else {
                    if port == "" {
                        f"psql -d {database} -U {user} -h {host}"
                    } else {
                        f"psql -d {database} -U {user} -h {host} -p {port}"
                    }
                }
            }
        }
    } else {
        if database == "" {
            if user == "" {
                if host == "" {
                    if port == "" {
                        f"PGPASSWORD=\\\"{password}\\\" psql"
                    } else {
                        f"PGPASSWORD=\\\"{password}\\\" psql -p {port}"
                    }
                } else {
                    if port == "" {
                        f"PGPASSWORD=\\\"{password}\\\" psql -h {host}"
                    } else {
                        f"PGPASSWORD=\\\"{password}\\\" psql -h {host} -p {port}"
                    }
                }
            } else {
                if host == "" {
                    if port == "" {
                        f"PGPASSWORD=\\\"{password}\\\" psql -U {user}"
                    } else {
                        f"PGPASSWORD=\\\"{password}\\\" psql -U {user} -p {port}"
                    }
                } else {
                    if port == "" {
                        f"PGPASSWORD=\\\"{password}\\\" psql -U {user} -h {host}"
                    } else {
                        f"PGPASSWORD=\\\"{password}\\\" psql -U {user} -h {host} -p {port}"
                    }
                }
            }
        } else {
            if user == "" {
                if host == "" {
                    if port == "" {
                        f"PGPASSWORD=\\\"{password}\\\" psql -d {database}"
                    } else {
                        f"PGPASSWORD=\\\"{password}\\\" psql -d {database} -p {port}"
                    }
                } else {
                    if port == "" {
                        f"PGPASSWORD=\\\"{password}\\\" psql -d {database} -h {host}"
                    } else {
                        f"PGPASSWORD=\\\"{password}\\\" psql -d {database} -h {host} -p {port}"
                    }
                }
            } else {
                if host == "" {
                    if port == "" {
                        f"PGPASSWORD=\\\"{password}\\\" psql -d {database} -U {user}"
                    } else {
                        f"PGPASSWORD=\\\"{password}\\\" psql -d {database} -U {user} -p {port}"
                    }
                } else {
                    if port == "" {
                        f"PGPASSWORD=\\\"{password}\\\" psql -d {database} -U {user} -h {host}"
                    } else {
                        f"PGPASSWORD=\\\"{password}\\\" psql -d {database} -U {user} -h {host} -p {port}"
                    }
                }
            }
        }
    }
}
