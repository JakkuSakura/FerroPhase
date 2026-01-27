use std::net::tcp::TcpStream;

#[unimplemented]
pub struct TlsConnector {}

impl TlsConnector {
    pub async fn connect(&self, domain: &str, stream: TcpStream) -> TlsStream {
        loop {}
    }
}

#[unimplemented]
pub struct TlsAcceptor {}

impl TlsAcceptor {
    pub async fn accept(&self, stream: TcpStream) -> TlsStream {
        loop {}
    }
}

#[unimplemented]
pub struct TlsStream {}

impl TlsStream {
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
