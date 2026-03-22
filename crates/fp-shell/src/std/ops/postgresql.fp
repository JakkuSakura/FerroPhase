pub const fn sql(sql: str, context hosts: str, psql_user: str, psql_password: str, psql_host: str, psql_port: str, psql_database: str, sudo: bool) -> bool {
    std::ops::postgres::sql(sql, hosts=hosts, psql_user=psql_user, psql_password=psql_password, psql_host=psql_host, psql_port=psql_port, psql_database=psql_database, sudo=sudo)
}

pub const fn role(role: str, context hosts: str, present: bool, password: str, login: bool, superuser: bool, inherit: bool, createdb: bool, createrole: bool, replication: bool, connection_limit: str, psql_user: str, psql_password: str, psql_host: str, psql_port: str, psql_database: str, sudo: bool) -> bool {
    std::ops::postgres::role(role, hosts=hosts, present=present, password=password, login=login, superuser=superuser, inherit=inherit, createdb=createdb, createrole=createrole, replication=replication, connection_limit=connection_limit, psql_user=psql_user, psql_password=psql_password, psql_host=psql_host, psql_port=psql_port, psql_database=psql_database, sudo=sudo)
}

pub const fn database(database: str, context hosts: str, present: bool, owner: str, template: str, encoding: str, lc_collate: str, lc_ctype: str, tablespace: str, connection_limit: str, psql_user: str, psql_password: str, psql_host: str, psql_port: str, psql_database: str, sudo: bool) -> bool {
    std::ops::postgres::database(database, hosts=hosts, present=present, owner=owner, template=template, encoding=encoding, lc_collate=lc_collate, lc_ctype=lc_ctype, tablespace=tablespace, connection_limit=connection_limit, psql_user=psql_user, psql_password=psql_password, psql_host=psql_host, psql_port=psql_port, psql_database=psql_database, sudo=sudo)
}

pub const fn dump(filename: str, context hosts: str, database: str, psql_user: str, psql_password: str, psql_host: str, psql_port: str, sudo: bool) -> bool {
    std::ops::postgres::dump(filename, hosts=hosts, database=database, psql_user=psql_user, psql_password=psql_password, psql_host=psql_host, psql_port=psql_port, sudo=sudo)
}

pub const fn load(filename: str, context hosts: str, database: str, psql_user: str, psql_password: str, psql_host: str, psql_port: str, sudo: bool) -> bool {
    std::ops::postgres::load(filename, hosts=hosts, database=database, psql_user=psql_user, psql_password=psql_password, psql_host=psql_host, psql_port=psql_port, sudo=sudo)
}
