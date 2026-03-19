use std::net::addr::SocketAddr;

#[unimplemented]
pub struct QuicConnection {
    handle: i64,
}

impl QuicConnection {
    pub async fn connect(addr: SocketAddr, server_name: &str) -> QuicConnection {
        loop {}
    }

    pub async fn open_bi(&mut self) -> QuicStream {
        loop {}
    }
}

#[unimplemented]
pub struct QuicListener {
    handle: i64,
}

impl QuicListener {
    pub async fn bind(addr: SocketAddr) -> QuicListener {
        loop {}
    }

    pub async fn accept(&mut self) -> QuicConnection {
        loop {}
    }
}

#[unimplemented]
pub struct QuicStream {
    handle: i64,
}

impl QuicStream {
    pub async fn read(&mut self, buf: &mut [u8]) -> i64 {
        loop {}
    }

    pub async fn write(&mut self, buf: &[u8]) -> i64 {
        loop {}
    }

    pub async fn finish(&mut self) -> () {
        loop {}
    }
}
