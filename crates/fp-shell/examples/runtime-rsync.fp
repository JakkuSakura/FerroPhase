const fn main() {
    let host = "web-1";
    std::files::rsync(
        src="./dist/",
        dest="/srv/fp-service/dist/",
        hosts=host,
        delete=true,
        checksum=true,
    );

    std::host::on(host, || {
        std::server::shell("sudo systemctl restart fp-service");
    });
}
