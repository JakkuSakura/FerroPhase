#[unimplemented]
pub struct HttpClient {
    handle: i64,
}

impl HttpClient {
    pub async fn send(&self, request: HttpRequest) -> HttpResponse {
        loop {}
    }
}

#[unimplemented]
pub struct HttpRequest {
    handle: i64,
}

impl HttpRequest {
    pub fn get(url: &str) -> HttpRequest {
        loop {}
    }

    pub fn post(url: &str, body: &[u8]) -> HttpRequest {
        loop {}
    }
}

#[unimplemented]
pub struct HttpResponse {
    handle: i64,
}

impl HttpResponse {
    pub fn status(&self) -> i64 {
        loop {}
    }

    pub fn body(&self) -> &[u8] {
        loop {}
    }
}
