use std::net::addr::SocketAddr;

#[unimplemented]
pub struct UdpSocket {}

impl UdpSocket {
    pub async fn bind(addr: SocketAddr) -> UdpSocket {
        loop {}
    }

    pub async fn send_to(&mut self, buf: &[u8], addr: SocketAddr) -> i64 {
        loop {}
    }

    pub async fn recv_from(&mut self, buf: &mut [u8]) -> (i64, SocketAddr) {
        loop {}
    }
}
