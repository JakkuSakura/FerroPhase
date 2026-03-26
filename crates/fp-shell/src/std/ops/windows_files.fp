#[cfg(target_lang = "pwsh")]
pub const fn download(
    src: str,
    dest: str,
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    std::ops::server::shell(
        f"$ProgressPreference = \"SilentlyContinue\"; Invoke-WebRequest -Uri {src} -OutFile {dest}",
        hosts=hosts,
        sudo=false,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    )
}

#[cfg(target_lang = "pwsh")]
pub const fn put(
    src: str,
    dest: str,
    context hosts: str = "localhost",
    only_if: str = "",
    unless: str = "",
    creates: str = "",
    removes: str = "",
) -> bool {
    std::ops::files::copy(
        src,
        dest,
        hosts=hosts,
        only_if=only_if,
        unless=unless,
        creates=creates,
        removes=removes,
    )
}
