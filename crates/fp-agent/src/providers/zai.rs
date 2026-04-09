use serde::Serialize;

use crate::schema::openai::{ChatContent, ChatMessage, ChatRequest, Tool, ToolCall};

#[derive(Clone, Debug, Serialize)]
pub struct ZaiChatRequest {
    pub model: String,
    pub messages: Vec<ZaiMessage>,
    pub stream: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tools: Option<Vec<Tool>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_choice: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub temperature: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub top_p: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_tokens: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub thinking: Option<ZaiThinkingConfig>,
}

#[derive(Clone, Debug, Serialize)]
pub struct ZaiMessage {
    pub role: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_calls: Option<Vec<ToolCall>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_call_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
}

#[derive(Clone, Debug, Serialize)]
pub struct ZaiThinkingConfig {
    #[serde(rename = "type")]
    pub thinking_type: String,
}

pub fn build_zai_chat_request(
    request: &ChatRequest,
    model: &str,
    thinking_enabled: Option<bool>,
) -> ZaiChatRequest {
    let tools = if request.tools.is_empty() {
        None
    } else {
        Some(request.tools.iter().cloned().map(transform_tool).collect())
    };
    ZaiChatRequest {
        model: model.to_string(),
        messages: request.messages.iter().map(to_zai_message).collect(),
        stream: request.stream,
        tools,
        tool_choice: request.tool_choice.clone(),
        temperature: request.temperature,
        top_p: request.top_p,
        max_tokens: request.max_tokens,
        thinking: thinking_enabled.map(zai_thinking_config),
    }
}

fn zai_thinking_config(enabled: bool) -> ZaiThinkingConfig {
    ZaiThinkingConfig {
        thinking_type: if enabled {
            "enabled".into()
        } else {
            "disabled".into()
        },
    }
}

fn to_zai_message(message: &ChatMessage) -> ZaiMessage {
    let role = if message.role == "developer" {
        "system".to_string()
    } else {
        message.role.clone()
    };
    let content = message.content.as_ref().map(chat_content_to_string);
    let tool_calls = if message.tool_calls.is_empty() {
        None
    } else {
        Some(message.tool_calls.clone())
    };
    ZaiMessage {
        role,
        content,
        tool_calls,
        tool_call_id: message.tool_call_id.clone(),
        name: message.name.clone(),
    }
}

fn chat_content_to_string(content: &ChatContent) -> String {
    match content {
        ChatContent::Text(text) => text.clone(),
        ChatContent::Parts(parts) => parts
            .iter()
            .map(|part| part.text.clone().unwrap_or_default())
            .collect::<Vec<_>>()
            .join(""),
    }
}

fn transform_tool(mut tool: Tool) -> Tool {
    if tool.tool_type == "function" {
        tool.strict = None;
    }
    if tool.tool_type == "web_search" {
        tool.web_search = Some(crate::schema::openai::WebSearchConfig {
            enable: Some(true),
            search_engine: Some("search_pro_jina".into()),
        });
    }
    tool
}
