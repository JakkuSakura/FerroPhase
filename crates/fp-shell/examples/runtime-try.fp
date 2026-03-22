const fn main() {
    try {
        defer std::ops::server::shell("echo cleanup");

        std::ops::server::shell("echo deploy body");
    } catch err {
        std::ops::server::shell(f"echo deploy failed={err}");
    } else {
        std::ops::server::shell("echo deploy success");
    } finally {
        std::ops::server::shell("echo deploy finally");
    }
}
