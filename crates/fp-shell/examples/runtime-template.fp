const fn main() {
    std::files::template(
        src="./templates/fp-service.conf.tpl",
        dest="/etc/fp-service/fp-service.conf",
    );
}
