pub const fn call(code: str, context hosts: str = "localhost", python: str, sudo: bool) -> bool {
    if python == "" {
        std::ops::server::shell(f"python -c {code}", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"{python} -c {code}", hosts=hosts, sudo=sudo)
    }
}

pub const fn ansible_module(src: str, context hosts: str = "localhost", function: str, args: str, kwargs: str, python: str, sudo: bool) -> bool {
    if python == "" {
        std::ops::server::shell(
            f"python -c \"import importlib.util; spec=importlib.util.spec_from_file_location('fp_ansible_module', '{src}'); mod=importlib.util.module_from_spec(spec); spec.loader.exec_module(mod); getattr(mod, '{function}')({args})\"",
            hosts=hosts,
            sudo=sudo,
        )
    } else {
        std::ops::server::shell(
            f"{python} -c \"import importlib.util; spec=importlib.util.spec_from_file_location('fp_ansible_module', '{src}'); mod=importlib.util.module_from_spec(spec); spec.loader.exec_module(mod); getattr(mod, '{function}')({args})\"",
            hosts=hosts,
            sudo=sudo,
        )
    }
}

pub const fn raise_exception(message: str, context hosts: str = "localhost", python: str, sudo: bool) -> bool {
    if python == "" {
        std::ops::server::shell(f"python -c \"raise Exception({message})\"", hosts=hosts, sudo=sudo)
    } else {
        std::ops::server::shell(f"{python} -c \"raise Exception({message})\"", hosts=hosts, sudo=sudo)
    }
}
