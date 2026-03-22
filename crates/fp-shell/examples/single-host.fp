const fn main() {
    with "web-1" {
        std::ops::server::shell("sudo systemctl restart fp-service");
        std::ops::files::copy(
            src="./config/prod.env",
            dest="/etc/fp-service/.env",
        );
    }
}
