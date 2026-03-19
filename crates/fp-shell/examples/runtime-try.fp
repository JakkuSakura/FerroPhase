const fn main() {
    try {
        defer std::server::shell("echo cleanup");

        std::server::shell("echo deploy body");
    } catch err {
        std::server::shell(f"echo deploy failed={err}");
    } else {
        std::server::shell("echo deploy success");
    } finally {
        std::server::shell("echo deploy finally");
    }
}
