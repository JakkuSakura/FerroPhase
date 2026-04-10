use serde::Deserialize;

#[derive(Debug, Clone)]
pub struct OpenAiErrorDetails {
    pub message: String,
    pub code: Option<String>,
    pub error_type: Option<String>,
}

#[derive(Debug, Deserialize)]
struct OpenAiErrorResponse {
    error: OpenAiErrorBody,
}

#[derive(Debug, Deserialize)]
struct OpenAiErrorBody {
    message: String,
    #[serde(rename = "type")]
    error_type: Option<String>,
    code: Option<OpenAiErrorCode>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum OpenAiErrorCode {
    String(String),
    Number(i64),
}

impl OpenAiErrorCode {
    fn to_string(&self) -> String {
        match self {
            OpenAiErrorCode::String(value) => value.clone(),
            OpenAiErrorCode::Number(value) => value.to_string(),
        }
    }
}

pub fn parse_openai_error(bytes: &[u8]) -> OpenAiErrorDetails {
    let parsed: Result<OpenAiErrorResponse, _> = serde_json::from_slice(bytes);
    match parsed {
        Ok(resp) => OpenAiErrorDetails {
            message: resp.error.message,
            code: resp.error.code.map(|code| code.to_string()),
            error_type: resp.error.error_type,
        },
        Err(_) => OpenAiErrorDetails {
            message: String::from_utf8_lossy(bytes).to_string(),
            code: None,
            error_type: None,
        },
    }
}
