use serde::Serialize;

use crate::schema::openai::{
    CompactRequest, Instructions, ResponsesInput, ResponsesRequest, Tool, input_items_to_text,
    instructions_to_text, messages_to_input_items,
};

#[derive(Clone, Debug, Serialize)]
pub struct OpenAiReasoning {
    pub effort: &'static str,
}

#[derive(Clone, Debug, Serialize)]
pub struct OpenAiResponsesPayload {
    pub model: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub input: Option<ResponsesInput>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub instructions: Option<Instructions>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub previous_response_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub store: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<std::collections::BTreeMap<String, serde_json::Value>>,
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
    pub max_output_tokens: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stream: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub include: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reasoning: Option<OpenAiReasoning>,
}

#[derive(Clone, Debug, Serialize)]
pub struct OpenAiCompactPayload {
    pub model: String,
    pub input: ResponsesInput,
    pub instructions: Instructions,
    pub store: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub temperature: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_tokens: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_output_tokens: Option<u64>,
    pub stream: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reasoning: Option<OpenAiReasoning>,
}

pub fn build_openai_payload(
    request: &ResponsesRequest,
    model: &str,
    reasoning: Option<OpenAiReasoning>,
    default_max_output_tokens: Option<u64>,
) -> OpenAiResponsesPayload {
    let input = resolve_input_for_payload(request);
    let mut payload = OpenAiResponsesPayload {
        model: model.to_string(),
        input,
        instructions: request.instructions.clone(),
        previous_response_id: request.previous_response_id.clone(),
        store: request.store,
        metadata: request.metadata.clone(),
        tools: request.tools.clone(),
        tool_choice: request.tool_choice.clone(),
        temperature: request.temperature,
        top_p: request.top_p,
        max_tokens: request.max_tokens,
        max_output_tokens: request
            .max_output_tokens
            .or(request.max_tokens)
            .or(default_max_output_tokens),
        stream: request.stream,
        include: request.include.clone(),
        reasoning,
    };
    if payload.max_output_tokens.is_none() {
        payload.max_output_tokens = payload.max_tokens;
    }
    payload
}

pub fn build_openai_compact_payload(
    request: &CompactRequest,
    model: &str,
    reasoning: Option<OpenAiReasoning>,
    max_tokens: Option<u64>,
    max_output_tokens: Option<u64>,
    temperature: Option<f64>,
) -> OpenAiCompactPayload {
    OpenAiCompactPayload {
        model: model.to_string(),
        input: request.input.clone(),
        instructions: request.instructions.clone(),
        store: false,
        temperature,
        max_tokens,
        max_output_tokens,
        stream: false,
        reasoning,
    }
}

pub fn resolve_input_for_payload(request: &ResponsesRequest) -> Option<ResponsesInput> {
    if let Some(input) = &request.input {
        return Some(input.clone());
    }
    request
        .messages
        .as_ref()
        .map(|messages| ResponsesInput::Items(messages_to_input_items(messages)))
}

pub fn coerce_items_input_to_text(payload: &mut OpenAiResponsesPayload) {
    let Some(input) = payload.input.take() else {
        return;
    };
    payload.input = Some(match input {
        ResponsesInput::Items(items) => ResponsesInput::Text(input_items_to_text(&items)),
        other => other,
    });
}

pub fn coerce_instructions_to_text(payload: &mut OpenAiResponsesPayload) {
    let Some(instructions) = payload.instructions.take() else {
        return;
    };
    payload.instructions = Some(match instructions {
        Instructions::Text(text) => Instructions::Text(text),
        Instructions::Parts(parts) => Instructions::Text(instructions_to_text(&parts)),
    });
}

pub fn coerce_input_and_instructions_to_text(payload: &mut OpenAiResponsesPayload) {
    coerce_items_input_to_text(payload);
    coerce_instructions_to_text(payload);
}
