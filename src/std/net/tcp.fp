use std::net::addr::SocketAddr;

#[unimplemented]
pub struct TcpStream {}

impl TcpStream {
    pub async fn connect(addr: SocketAddr) -> TcpStream {
        loop {}
    }

    pub async fn read(&mut self, buf: &mut [u8]) -> i64 {
        loop {}
    }

    pub async fn write(&mut self, buf: &[u8]) -> i64 {
        loop {}
    }

    pub async fn shutdown(&mut self) -> () {
        loop {}
    }
}

#[unimplemented]
pub struct TcpListener {}

impl TcpListener {
    pub async fn bind(addr: SocketAddr) -> TcpListener {
        loop {}
    }

    pub async fn accept(&mut self) -> TcpStream {
        loop {}
    }
}
