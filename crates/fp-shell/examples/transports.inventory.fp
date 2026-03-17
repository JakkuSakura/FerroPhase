use std::collections::HashMap;

struct Inventory {
    groups: HashMap<&'static str, [&'static str; 5]>,
    hosts: HashMap<&'static str, Host>,
}

struct Host {
    transport: &'static str,
    address: &'static str,
    user: &'static str,
    password: &'static str,
    port: u16,
    container: &'static str,
    pod: &'static str,
    namespace: &'static str,
    context: &'static str,
    scheme: &'static str,
}

const fn inventory() -> Inventory {
    Inventory {
        groups: HashMap::from([(
            "all",
            [
                "localhost",
                "ssh-web",
                "docker-app",
                "k8s-api",
                "windows-admin",
            ],
        )]),
        hosts: HashMap::from([
            (
                "localhost",
                Host {
                    transport: "local",
                    address: "",
                    user: "",
                    password: "",
                    port: 0,
                    container: "",
                    pod: "",
                    namespace: "",
                    context: "",
                    scheme: "",
                },
            ),
            (
                "ssh-web",
                Host {
                    transport: "ssh",
                    address: "10.0.0.11",
                    user: "deploy",
                    password: "",
                    port: 22,
                    container: "",
                    pod: "",
                    namespace: "",
                    context: "",
                    scheme: "",
                },
            ),
            (
                "docker-app",
                Host {
                    transport: "docker",
                    address: "",
                    user: "root",
                    password: "",
                    port: 0,
                    container: "app",
                    pod: "",
                    namespace: "",
                    context: "",
                    scheme: "",
                },
            ),
            (
                "k8s-api",
                Host {
                    transport: "kubectl",
                    address: "",
                    user: "",
                    password: "",
                    port: 0,
                    container: "api",
                    pod: "api-7f9f6",
                    namespace: "prod",
                    context: "prod-cluster",
                    scheme: "",
                },
            ),
            (
                "windows-admin",
                Host {
                    transport: "winrm",
                    address: "10.0.0.21",
                    user: "Administrator",
                    password: "change-me",
                    port: 5985,
                    container: "",
                    pod: "",
                    namespace: "",
                    context: "",
                    scheme: "http",
                },
            ),
        ]),
    }
}
