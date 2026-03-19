#[unimplemented]
pub struct WsStream {
    handle: i64,
}

impl WsStream {
    pub async fn connect(url: &str) -> WsStream {
        loop {}
    }

    pub async fn send(&mut self, message: WsMessage) -> () {
        loop {}
    }

    pub async fn recv(&mut self) -> WsMessage {
        loop {}
    }
}

#[unimplemented]
pub struct WsMessage {
    handle: i64,
}

impl WsMessage {
    pub fn text(value: &str) -> WsMessage {
        loop {}
    }

    pub fn binary(value: &[u8]) -> WsMessage {
        loop {}
    }
}
