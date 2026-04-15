use async_trait::async_trait;
#[cfg(feature = "net")]
use reqwest::Client;
use serde::{Deserialize, Serialize};
use serde_json::Value;

pub mod common;
pub mod normalize;
pub mod providers;
pub mod schema;
pub mod validate;

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
    #[serde(default)]
    pub tool_choice: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_tokens: Option<u32>,
    #[serde(default)]
    pub json_mode: bool,
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
            let response = self
                .client
                .chat(AgentRequest {
                    model: request.model.clone(),
                    messages: messages.clone(),
                    tools: request.tools.clone(),
                    tool_choice: request.tool_choice.clone(),
                    max_tokens: request.max_tokens,
                    json_mode: false,
                })
                .await?;

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

        // If json_mode is requested and the answer is not valid JSON,
        // send a follow-up request with response_format to force JSON output.
        if request.json_mode {
            let is_json = serde_json::from_str::<serde_json::Value>(&final_answer).is_ok();
            if !is_json {
                let mut followup_messages = messages.clone();
                followup_messages.push(AgentMessage::user(
                    "Convert your previous answer into a valid JSON object. Return ONLY the JSON, no explanation.".to_owned(),
                ));
                let response = self
                    .client
                    .chat(AgentRequest {
                        model: request.model.clone(),
                        messages: followup_messages,
                        tools: Vec::new(),
                        tool_choice: "none".to_owned(),
                        max_tokens: request.max_tokens,
                        json_mode: true,
                    })
                    .await?;
                if let Some(content) = response.message.content {
                    if serde_json::from_str::<serde_json::Value>(&content).is_ok() {
                        final_answer = content;
                    } else if let Some(start) = content.find('{') {
                        if let Some(end) = content.rfind('}') {
                            if start < end {
                                final_answer = content[start..=end].to_owned();
                            }
                        }
                    }
                }
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
#[cfg(feature = "net")]
#[derive(Clone)]
pub struct OpenRouterClient {
    client: Client,
    base_url: String,
    api_key: String,
}

#[cfg(feature = "net")]
impl OpenRouterClient {
    pub fn new(base_url: String, api_key: String) -> Self {
        Self {
            client: Client::new(),
            base_url,
            api_key,
        }
    }
}

#[cfg(feature = "net")]
#[async_trait]
impl ModelClient for OpenRouterClient {
    async fn chat(&self, request: AgentRequest) -> Result<AgentResponse, AgentError> {
        let response_format = if request.json_mode && request.tools.is_empty() {
            Some(serde_json::json!({"type": "json_object"}))
        } else {
            None
        };
        let response = self
            .client
            .post(format!("{}/chat/completions", self.base_url))
            .bearer_auth(&self.api_key)
            .json(&OpenRouterPayload {
                model: request.model,
                messages: request.messages,
                tools: request.tools,
                tool_choice: request.tool_choice,
                max_tokens: request.max_tokens,
                response_format,
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

#[cfg(not(feature = "net"))]
#[derive(Clone)]
pub struct OpenRouterClient;

#[cfg(not(feature = "net"))]
impl OpenRouterClient {
    pub fn new(_base_url: String, _api_key: String) -> Self {
        Self
    }
}

#[cfg(not(feature = "net"))]
#[async_trait]
impl ModelClient for OpenRouterClient {
    async fn chat(&self, _request: AgentRequest) -> Result<AgentResponse, AgentError> {
        Err(AgentError::new(
            "Feature 'net' is disabled; OpenRouter client is unavailable.",
        ))
    }
}

// Wire-format types for the OpenAI / OpenRouter chat completions API.

#[cfg(feature = "net")]
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct OpenRouterPayload {
    model: String,
    messages: Vec<AgentMessage>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    tools: Vec<ToolDefinition>,
    tool_choice: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    max_tokens: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    response_format: Option<Value>,
}

#[cfg(feature = "net")]
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct OpenRouterResponseBody {
    choices: Vec<OpenRouterChoice>,
}

#[cfg(feature = "net")]
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

    let response = client
        .chat(AgentRequest {
            model: model.to_owned(),
            messages,
            tools: Vec::new(),
            tool_choice: "none".to_owned(),
            max_tokens: None,
            json_mode: false,
        })
        .await?;

    Ok(response.message.content.unwrap_or_default())
}

// ---------------------------------------------------------------------------
// Truth-Based Evidence Tracking
// ---------------------------------------------------------------------------

use chrono::{DateTime, Utc};

/// Evidence recorded from a tool execution
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Evidence {
    pub tool_name: String,
    pub parameters: serde_json::Value,
    pub result_sample: serde_json::Value,
    pub row_count: usize,
    pub timestamp: DateTime<Utc>,
}

impl Evidence {
    pub fn new(
        tool_name: impl Into<String>,
        parameters: serde_json::Value,
        result_sample: serde_json::Value,
        row_count: usize,
    ) -> Self {
        Self {
            tool_name: tool_name.into(),
            parameters,
            result_sample,
            row_count,
            timestamp: Utc::now(),
        }
    }
}

/// Trait for recording evidence - implemented by the application
/// to store evidence in database, file, or any storage backend.
#[async_trait]
pub trait EvidenceRecorder: Send + Sync {
    async fn record(&self, evidence: Evidence) -> Result<(), AgentError>;
}

/// A tool executor that tracks evidence for every call
#[derive(Clone)]
pub struct EvidenceTrackingExecutor<T, E> {
    inner: T,
    recorder: E,
}

impl<T, E> EvidenceTrackingExecutor<T, E>
where
    T: ToolExecutor,
    E: EvidenceRecorder,
{
    pub fn new(inner: T, recorder: E) -> Self {
        Self { inner, recorder }
    }
}

#[async_trait]
impl<T, E> ToolExecutor for EvidenceTrackingExecutor<T, E>
where
    T: ToolExecutor + Clone,
    E: EvidenceRecorder,
{
    async fn execute(&self, call: ToolCall) -> Result<String, AgentError> {
        let tool_name = call.function.name.clone();
        let params: serde_json::Value =
            serde_json::from_str(&call.function.arguments).unwrap_or(serde_json::Value::Null);

        let result = self.inner.execute(call).await?;

        // Try to extract sample from result for evidence
        let result_sample: serde_json::Value = serde_json::from_str::<serde_json::Value>(&result)
            .map(|v: serde_json::Value| {
                // Extract first row if result is {"rows": [...]}
                v.get("rows")
                    .and_then(|r: &serde_json::Value| r.as_array())
                    .and_then(|arr: &Vec<serde_json::Value>| arr.first())
                    .cloned()
                    .unwrap_or(v)
            })
            .unwrap_or(serde_json::Value::Null);

        let row_count = serde_json::from_str::<serde_json::Value>(&result)
            .ok()
            .and_then(|v| v.get("rowCount").and_then(|c| c.as_i64()))
            .map(|c| c as usize)
            .unwrap_or(0);

        let evidence = Evidence::new(&tool_name, params, result_sample, row_count);
        if let Err(e) = self.recorder.record(evidence).await {
            tracing::warn!("failed to record evidence: {}", e);
        }

        Ok(result)
    }
}
