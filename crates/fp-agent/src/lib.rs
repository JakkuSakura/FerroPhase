use async_trait::async_trait;
use reqwest::Client;
use serde::{Deserialize, Serialize};
use serde_json::Value;

// ---------------------------------------------------------------------------
// Error
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub struct AgentError {
    pub message: String,
}

impl AgentError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl std::fmt::Display for AgentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for AgentError {}

// ---------------------------------------------------------------------------
// Messages & tool types
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AgentMessage {
    pub role: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub tool_calls: Vec<ToolCall>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_call_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
}

impl AgentMessage {
    pub fn system(content: String) -> Self {
        Self {
            role: "system".to_owned(),
            content: Some(content),
            tool_calls: Vec::new(),
            tool_call_id: None,
            name: None,
        }
    }

    pub fn user(content: String) -> Self {
        Self {
            role: "user".to_owned(),
            content: Some(content),
            tool_calls: Vec::new(),
            tool_call_id: None,
            name: None,
        }
    }

    pub fn tool(call_id: String, name: String, content: String) -> Self {
        Self {
            role: "tool".to_owned(),
            content: Some(content),
            tool_calls: Vec::new(),
            tool_call_id: Some(call_id),
            name: Some(name),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolCall {
    pub id: String,
    pub function: ToolCallFunction,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolCallFunction {
    pub name: String,
    pub arguments: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolDefinition {
    pub r#type: String,
    pub function: ToolFunctionDefinition,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolFunctionDefinition {
    pub name: String,
    pub description: String,
    pub parameters: Value,
}

// ---------------------------------------------------------------------------
// Request / Response
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AgentRequest {
    pub model: String,
    pub messages: Vec<AgentMessage>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub tools: Vec<ToolDefinition>,
    #[serde(default = "default_tool_choice")]
    pub tool_choice: String,
}

fn default_tool_choice() -> String {
    "auto".to_owned()
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AgentResponse {
    pub message: AgentMessage,
}

// ---------------------------------------------------------------------------
// Traits
// ---------------------------------------------------------------------------

#[async_trait]
pub trait ModelClient: Send + Sync {
    async fn chat(&self, request: AgentRequest) -> Result<AgentResponse, AgentError>;
}

#[async_trait]
pub trait ToolExecutor: Send + Sync {
    async fn execute(&self, call: ToolCall) -> Result<String, AgentError>;
}

// ---------------------------------------------------------------------------
// Agent runner
// ---------------------------------------------------------------------------

pub struct AgentRunner<C, T> {
    pub client: C,
    pub tools: T,
    pub max_rounds: usize,
}

impl<C, T> AgentRunner<C, T>
where
    C: ModelClient,
    T: ToolExecutor,
{
    pub async fn run(
        &self,
        request: AgentRequest,
    ) -> Result<(String, Vec<AgentMessage>), AgentError> {
        let mut messages = request.messages;
        let mut final_answer = String::new();

        for _ in 0..self.max_rounds {
            let response = self.client.chat(AgentRequest {
                model: request.model.clone(),
                messages: messages.clone(),
                tools: request.tools.clone(),
                tool_choice: request.tool_choice.clone(),
            }).await?;

            let msg = response.message;
            if msg.tool_calls.is_empty() {
                final_answer = msg.content.clone().unwrap_or_default();
                messages.push(msg);
                break;
            }

            messages.push(msg.clone());
            for call in msg.tool_calls {
                let result = self.tools.execute(call.clone()).await?;
                messages.push(AgentMessage::tool(call.id, call.function.name, result));
            }
        }

        Ok((final_answer, messages))
    }
}

// ---------------------------------------------------------------------------
// OpenRouter client (OpenAI-compatible chat completions)
// ---------------------------------------------------------------------------

/// A [`ModelClient`] that speaks the OpenAI-compatible chat completions protocol.
/// Works with OpenRouter, and any provider that exposes the same API surface.
#[derive(Clone)]
pub struct OpenRouterClient {
    client: Client,
    base_url: String,
    api_key: String,
}

impl OpenRouterClient {
    pub fn new(base_url: String, api_key: String) -> Self {
        Self {
            client: Client::new(),
            base_url,
            api_key,
        }
    }
}

#[async_trait]
impl ModelClient for OpenRouterClient {
    async fn chat(&self, request: AgentRequest) -> Result<AgentResponse, AgentError> {
        let response = self
            .client
            .post(format!("{}/chat/completions", self.base_url))
            .bearer_auth(&self.api_key)
            .json(&OpenRouterPayload {
                model: request.model,
                messages: request.messages,
                tools: request.tools,
                tool_choice: request.tool_choice,
            })
            .send()
            .await
            .map_err(|err| AgentError::new(err.to_string()))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(AgentError::new(format!("openrouter {status}: {body}")));
        }

        let payload: OpenRouterResponseBody = response
            .json()
            .await
            .map_err(|err| AgentError::new(err.to_string()))?;
        let Some(choice) = payload.choices.into_iter().next() else {
            return Err(AgentError::new("openrouter empty response"));
        };

        Ok(AgentResponse {
            message: choice.message,
        })
    }
}

// Wire-format types for the OpenAI / OpenRouter chat completions API.

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct OpenRouterPayload {
    model: String,
    messages: Vec<AgentMessage>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    tools: Vec<ToolDefinition>,
    tool_choice: String,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct OpenRouterResponseBody {
    choices: Vec<OpenRouterChoice>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct OpenRouterChoice {
    message: AgentMessage,
}

// ---------------------------------------------------------------------------
// Convenience: single-shot helper (no tools)
// ---------------------------------------------------------------------------

/// Send a single user message and return the assistant text.
/// Useful for simple tasks like translation or summarization
/// that don't need multi-round tool calling.
pub async fn simple_chat(
    client: &dyn ModelClient,
    model: &str,
    system: Option<&str>,
    user: &str,
) -> Result<String, AgentError> {
    let mut messages = Vec::new();
    if let Some(sys) = system {
        messages.push(AgentMessage::system(sys.to_owned()));
    }
    messages.push(AgentMessage::user(user.to_owned()));

    let response = client.chat(AgentRequest {
        model: model.to_owned(),
        messages,
        tools: Vec::new(),
        tool_choice: "none".to_owned(),
    }).await?;

    Ok(response.message.content.unwrap_or_default())
}
