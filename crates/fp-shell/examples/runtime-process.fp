const fn main() {
    let pipeline = std::shell::process::pipe(
        std::shell::process::raw("printf deploy"),
        std::shell::process::stdout_to(std::shell::process::raw("cat"), "/tmp/fp-shell.log"),
    );

    std::shell::process::run(pipeline);
}
