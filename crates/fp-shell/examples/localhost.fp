const fn main() {
    std::server::shell("echo localhost deployment");
    std::files::copy(src="./config/local.env", dest="./.env");
}
