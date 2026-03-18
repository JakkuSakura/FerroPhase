mod fp_shell_std {
    pub struct Inventory {
        groups: any,
        hosts: any,
    }

    pub struct Host {
        transport: &'static str,
        address?: &'static str,
        user?: &'static str,
        password?: &'static str,
        port?: u16,
        container?: &'static str,
        pod?: &'static str,
        namespace?: &'static str,
        context?: &'static str,
        scheme?: &'static str,
    }
}
