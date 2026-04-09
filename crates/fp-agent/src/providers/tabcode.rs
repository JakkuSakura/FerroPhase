use crate::providers::openai::{
    OpenAiCompactPayload, OpenAiReasoning, OpenAiResponsesPayload, build_openai_compact_payload,
    build_openai_payload,
};
use crate::schema::openai::{CompactRequest, ResponsesRequest};

pub fn build_tabcode_payload(
    request: &ResponsesRequest,
    model: &str,
    reasoning: Option<OpenAiReasoning>,
    default_max_output_tokens: Option<u64>,
) -> OpenAiResponsesPayload {
    build_openai_payload(request, model, reasoning, default_max_output_tokens)
}

pub fn build_tabcode_compact_payload(
    request: &CompactRequest,
    model: &str,
    reasoning: Option<OpenAiReasoning>,
    max_tokens: Option<u64>,
    max_output_tokens: Option<u64>,
    temperature: Option<f64>,
) -> OpenAiCompactPayload {
    build_openai_compact_payload(
        request,
        model,
        reasoning,
        max_tokens,
        max_output_tokens,
        temperature,
    )
}
