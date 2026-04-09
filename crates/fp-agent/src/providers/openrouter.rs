use crate::providers::openai::{
    OpenAiCompactPayload, OpenAiReasoning, OpenAiResponsesPayload, build_openai_compact_payload,
    build_openai_payload, coerce_input_and_instructions_to_text,
};
use crate::schema::openai::{
    CompactRequest, Instructions, ResponsesInput, ResponsesRequest, input_items_to_text,
    instructions_to_text,
};

pub fn build_openrouter_payload(
    request: &ResponsesRequest,
    model: &str,
    reasoning: Option<OpenAiReasoning>,
    default_max_output_tokens: u64,
) -> OpenAiResponsesPayload {
    let mut payload =
        build_openai_payload(request, model, reasoning, Some(default_max_output_tokens));
    coerce_input_and_instructions_to_text(&mut payload);
    if payload.max_tokens.is_none() {
        payload.max_tokens = Some(
            payload
                .max_output_tokens
                .unwrap_or(default_max_output_tokens),
        );
    }
    payload
}

pub fn build_openrouter_compact_payload(
    request: &CompactRequest,
    model: &str,
    reasoning: Option<OpenAiReasoning>,
    max_tokens: Option<u64>,
    max_output_tokens: Option<u64>,
    temperature: Option<f64>,
) -> OpenAiCompactPayload {
    let mut payload = build_openai_compact_payload(
        request,
        model,
        reasoning,
        max_tokens,
        max_output_tokens,
        temperature,
    );
    coerce_compact_payload_to_text(&mut payload);
    payload
}

fn coerce_compact_payload_to_text(payload: &mut OpenAiCompactPayload) {
    payload.input = match &payload.input {
        ResponsesInput::Items(items) => ResponsesInput::Text(input_items_to_text(items)),
        other => other.clone(),
    };
    payload.instructions = match &payload.instructions {
        Instructions::Parts(parts) => Instructions::Text(instructions_to_text(parts)),
        other => other.clone(),
    };
}
