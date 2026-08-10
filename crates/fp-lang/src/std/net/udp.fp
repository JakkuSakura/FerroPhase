
#[unimplemented]
pub struct UdpSocket {}

impl UdpSocket {
    pub async fn bind(addr: ::std::net::addr::SocketAddr) -> UdpSocket {
        loop {}
    }

    pub async fn send_to(&mut self, buf: &[u8], addr: ::std::net::addr::SocketAddr) -> i64 {
        loop {}
    }

    pub async fn recv_from(&mut self, buf: &mut [u8]) -> (i64, ::std::net::addr::SocketAddr) {
        loop {}
    }
}
