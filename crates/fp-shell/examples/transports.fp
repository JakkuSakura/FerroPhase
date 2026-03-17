const fn main() {
    std::server::shell("echo local hello");
    std::server::shell("echo ssh hello", hosts="ssh-web");
    std::server::shell("echo docker hello", hosts="docker-app");
    std::server::shell("echo kubectl hello", hosts="k8s-api");
    std::server::shell("Write-Host winrm hello", hosts="windows-admin");
}
