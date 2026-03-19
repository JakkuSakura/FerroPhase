const fn main() {
    with "web-1" {
        std::server::shell("sudo systemctl restart fp-service");
        std::files::copy(
            src="./config/prod.env",
            dest="/etc/fp-service/.env",
        );
    }
}
