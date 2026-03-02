const fn main() {
    let remote = "web-1";
    let check_cmd = "uptime";
    let retry_count = 3;
    let total_retry = retry_count * 2 + 1;

    std::server::shell(check_cmd, hosts=remote);
    std::server::shell(retry_count);
    std::server::shell(total_retry);

    std::host::on(["web-1", "web-2"], || {
        std::server::shell("echo inside closure scope");
    });
}
