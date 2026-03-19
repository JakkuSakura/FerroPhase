pub mod server {
    #[defaults(hosts = "localhost")]
    pub const fn shell(
        command: str,
        context hosts: str,
        only_if: str,
        unless: str,
        creates: str,
        removes: str,
        sudo: bool,
        cwd: str,
    ) -> bool {
        let command = command_with_options(command, cwd, sudo);
        shell_run(hosts, command, only_if, unless, creates, removes);
        runtime_last_changed()
    }
}
