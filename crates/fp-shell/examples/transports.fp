const fn main() {
    std::server::shell("echo local hello");
    with "ssh-web" {
        std::server::shell("echo ssh hello");
    }
    with "docker-app" {
        std::server::shell("echo docker hello");
    }
    with "k8s-api" {
        std::server::shell("echo kubectl hello");
    }
    with "windows-admin" {
        std::server::shell("Write-Host winrm hello");
    }
}
