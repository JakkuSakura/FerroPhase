use std::net::tcp::TcpStream;

#[unimplemented]
pub struct TlsConnector {
    handle: i64,
}

impl TlsConnector {
    pub async fn connect(&self, domain: &str, stream: TcpStream) -> TlsStream {
        loop {}
    }
}

#[unimplemented]
pub struct TlsAcceptor {
    handle: i64,
}

impl TlsAcceptor {
    pub async fn accept(&self, stream: TcpStream) -> TlsStream {
        loop {}
    }
}

#[unimplemented]
pub struct TlsStream {
    handle: i64,
}

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
