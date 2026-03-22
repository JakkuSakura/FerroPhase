const fn main() {
    std::ops::server::shell("echo localhost deployment");
    std::ops::files::copy(src="./config/local.env", dest="./.env");
}
