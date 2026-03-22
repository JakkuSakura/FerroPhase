const fn sync_and_restart(host: str, service: str) {
    with host {
        std::ops::files::rsync(
            src="./dist/",
            dest="/srv/fp-service/dist/",
            delete=true,
        );

        std::ops::services::restart(name=service);
    }
}

const fn main() {
    sync_and_restart("web-1", "fp-service");
    sync_and_restart("web-2", "fp-service");
}
