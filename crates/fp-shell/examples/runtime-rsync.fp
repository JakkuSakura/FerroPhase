const fn main() {
    let host = "web-1";
    with host {
        std::ops::files::rsync(
            src="./dist/",
            dest="/srv/fp-service/dist/",
            delete=true,
            checksum=true,
        );

        std::ops::server::shell("sudo systemctl restart fp-service");
    }
}
