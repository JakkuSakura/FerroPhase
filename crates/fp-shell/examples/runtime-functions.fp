const fn sync_and_restart(host: str, service: str) {
    std::files::rsync(
        src="./dist/",
        dest="/srv/fp-service/dist/",
        hosts=host,
        delete=true,
    );

    std::host::on(host, || {
        std::service::restart(name=service);
    });
}

const fn main() {
    sync_and_restart("web-1", "fp-service");
    sync_and_restart("web-2", "fp-service");
}
