const fn main() {
    std::host::on("web-1", || {
        std::server::shell("sudo systemctl restart fp-service");
        std::files::copy(src="./config/prod.env", dest="/etc/fp-service/.env");
    });
}
