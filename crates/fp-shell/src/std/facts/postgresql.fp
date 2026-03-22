pub const fn roles(user: str, password: str, host: str, port: str, database: str) -> str {
    std::facts::postgres::roles(user, password, host, port, database)
}

pub const fn databases(user: str, password: str, host: str, port: str, database: str) -> str {
    std::facts::postgres::databases(user, password, host, port, database)
}
