use crate::providers::openai::{
    OpenAiCompactPayload, OpenAiReasoning, OpenAiResponsesPayload, build_openai_compact_payload,
    build_openai_payload, coerce_input_and_instructions_to_text,
};
use crate::schema::openai::{CompactRequest, ResponsesRequest};

pub fn build_tabcode_payload(
    request: &ResponsesRequest,
    model: &str,
    reasoning: Option<OpenAiReasoning>,
    default_max_output_tokens: Option<u64>,
) -> OpenAiResponsesPayload {
    let mut payload = build_openai_payload(request, model, reasoning, default_max_output_tokens);
    coerce_input_and_instructions_to_text(&mut payload);
    payload
}

pub fn build_tabcode_compact_payload(
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
    payload.input = match &payload.input {
        crate::schema::openai::ResponsesInput::Items(items) => {
            crate::schema::openai::ResponsesInput::Text(
                crate::schema::openai::input_items_to_text(items),
            )
        }
        other => other.clone(),
    };
    payload.instructions = match &payload.instructions {
        crate::schema::openai::Instructions::Parts(parts) => {
            crate::schema::openai::Instructions::Text(
                crate::schema::openai::instructions_to_text(parts),
            )
        }
        other => other.clone(),
    };
    payload
}
