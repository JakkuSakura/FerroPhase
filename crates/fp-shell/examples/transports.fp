const fn main() {
    std::ops::server::shell("echo local hello");
    with "ssh-web" {
        std::ops::server::shell("echo ssh hello");
    }
    with "docker-app" {
        std::ops::server::shell("echo docker hello");
    }
    with "k8s-api" {
        std::ops::server::shell("echo kubectl hello");
    }
    with "windows-admin" {
        std::ops::server::shell("Write-Host winrm hello");
    }
}
