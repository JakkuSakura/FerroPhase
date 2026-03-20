pub mod files {
    #[defaults(hosts = "localhost")]
    pub const fn copy(
        src: str,
        dest: str,
        context hosts: str,
        only_if: str,
        unless: str,
        creates: str,
        removes: str,
    ) -> bool {
        shell_copy(hosts, src, dest, only_if, unless, creates, removes);
        runtime_last_changed()
    }

    #[defaults(hosts = "localhost")]
    pub const fn copy_local(
        src: str,
        dest: str,
        context hosts: str,
        only_if: str,
        unless: str,
        creates: str,
        removes: str,
    ) -> bool {
        shell_copy_local(hosts, src, dest, only_if, unless, creates, removes);
        runtime_last_changed()
    }

    #[defaults(hosts = "localhost")]
    pub const fn copy_ssh(
        src: str,
        dest: str,
        context hosts: str,
        only_if: str,
        unless: str,
        creates: str,
        removes: str,
    ) -> bool {
        shell_copy_ssh(hosts, src, dest, only_if, unless, creates, removes);
        runtime_last_changed()
    }

    #[defaults(hosts = "localhost")]
    pub const fn copy_docker(
        src: str,
        dest: str,
        context hosts: str,
        only_if: str,
        unless: str,
        creates: str,
        removes: str,
    ) -> bool {
        shell_copy_docker(hosts, src, dest, only_if, unless, creates, removes);
        runtime_last_changed()
    }

    #[defaults(hosts = "localhost")]
    pub const fn copy_kubectl(
        src: str,
        dest: str,
        context hosts: str,
        only_if: str,
        unless: str,
        creates: str,
        removes: str,
    ) -> bool {
        shell_copy_kubectl(hosts, src, dest, only_if, unless, creates, removes);
        runtime_last_changed()
    }

    #[defaults(hosts = "localhost")]
    pub const fn copy_winrm(
        src: str,
        dest: str,
        context hosts: str,
        only_if: str,
        unless: str,
        creates: str,
        removes: str,
    ) -> bool {
        shell_copy_winrm(hosts, src, dest, only_if, unless, creates, removes);
        runtime_last_changed()
    }

    #[defaults(hosts = "localhost")]
    pub const fn template(
        src: str,
        dest: str,
        context hosts: str,
        vars: str,
        only_if: str,
        unless: str,
        creates: str,
        removes: str,
    ) -> bool {
        shell_template(hosts, src, dest, vars, only_if, unless, creates, removes);
        runtime_last_changed()
    }

    #[defaults(hosts = "localhost")]
    pub const fn template_local(
        src: str,
        dest: str,
        context hosts: str,
        vars: str,
        only_if: str,
        unless: str,
        creates: str,
        removes: str,
    ) -> bool {
        shell_template_local(hosts, src, dest, vars, only_if, unless, creates, removes);
        runtime_last_changed()
    }

    #[defaults(hosts = "localhost")]
    pub const fn template_ssh(
        src: str,
        dest: str,
        context hosts: str,
        vars: str,
        only_if: str,
        unless: str,
        creates: str,
        removes: str,
    ) -> bool {
        shell_template_ssh(hosts, src, dest, vars, only_if, unless, creates, removes);
        runtime_last_changed()
    }

    #[defaults(hosts = "localhost", archive = true, compress = true)]
    pub const fn rsync(
        src: str,
        dest: str,
        context hosts: str,
        archive: bool,
        compress: bool,
        delete: bool,
        checksum: bool,
        only_if: str,
        unless: str,
        creates: str,
        removes: str,
    ) -> bool {
        let flags = rsync_flag_string(archive, compress, delete, checksum);
        shell_rsync(hosts, flags, src, dest, only_if, unless, creates, removes);
        runtime_last_changed()
    }

    #[defaults(hosts = "localhost", archive = true, compress = true)]
    pub const fn rsync_local(
        src: str,
        dest: str,
        context hosts: str,
        archive: bool,
        compress: bool,
        delete: bool,
        checksum: bool,
        only_if: str,
        unless: str,
        creates: str,
        removes: str,
    ) -> bool {
        let flags = rsync_flag_string(archive, compress, delete, checksum);
        shell_rsync_local(hosts, flags, src, dest, only_if, unless, creates, removes);
        runtime_last_changed()
    }

    #[defaults(hosts = "localhost", archive = true, compress = true)]
    pub const fn rsync_remote(
        src: str,
        dest: str,
        context hosts: str,
        archive: bool,
        compress: bool,
        delete: bool,
        checksum: bool,
        only_if: str,
        unless: str,
        creates: str,
        removes: str,
    ) -> bool {
        let flags = rsync_flag_string(archive, compress, delete, checksum);
        shell_rsync_remote(hosts, flags, src, dest, only_if, unless, creates, removes);
        runtime_last_changed()
    }
}
