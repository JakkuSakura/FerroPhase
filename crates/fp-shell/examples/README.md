# `fp-shell` examples

Compile a Ferro shell program to Bash:

```bash
cargo run -p fp-shell -- compile crates/fp-shell/examples/transports.fp \
  --inventory crates/fp-shell/examples/transports.inventory.toml \
  --target bash
```

Compile the same program to PowerShell:

```bash
cargo run -p fp-shell -- compile crates/fp-shell/examples/transports.fp \
  --inventory crates/fp-shell/examples/transports.inventory.toml \
  --target powershell
```

Compile using a Ferro inventory instead of TOML:

```bash
cargo run -p fp-shell -- compile crates/fp-shell/examples/transports.fp \
  --inventory crates/fp-shell/examples/transports.inventory.fp \
  --target bash
```

Inventory is transport-driven only.

You can also express inventory as Ferro source in a `.fp` file by defining
`const fn inventory() -> Inventory` and returning a struct whose serialized shape
matches the TOML document.

Minimal sketch:

```fp
use std::collections::HashMap;

struct Inventory {
    groups: HashMap<&'static str, [&'static str; 2]>,
    hosts: HashMap<&'static str, Host>,
}

struct Host {
    transport: &'static str,
    address: &'static str,
    user: &'static str,
}

const fn inventory() -> Inventory {
    Inventory {
        groups: HashMap::from([("web", ["web-1", "web-2"])]),
        hosts: HashMap::from([
            ("web-1", Host { transport: "ssh", address: "10.0.0.11", user: "deploy" }),
            ("web-2", Host { transport: "ssh", address: "10.0.0.12", user: "deploy" }),
        ]),
    }
}
```

For `.fp` inventory, empty strings and `0` are normalized as absent optional
fields during loading.

- `local` runs on the machine executing the generated script.
- `ssh` uses `ssh` and `scp` in Bash, and `ssh` and `scp` in PowerShell.
- `docker` uses `docker exec` and `docker cp`.
- `kubectl` uses `kubectl exec` and `kubectl cp`.
- `winrm` uses `evil-winrm` in Bash and `New-PSSession` in PowerShell.

Current limits:

- `rsync` is only supported for SSH endpoints.
- Bash `winrm` only supports command execution.
- PowerShell `winrm` requires `password` in inventory for non-interactive execution.
