use std::collections::HashMap;
use std::hosts::{Host, Inventory};

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
