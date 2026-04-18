use crate::AgentError;
use crate::providers::gemini::{GeminiRequestBody, build_gemini_request};
use crate::providers::openai::{
    OpenAiCompactPayload, OpenAiReasoning, OpenAiResponsesPayload, build_openai_compact_payload,
    build_openai_payload,
};
use crate::providers::openrouter::{build_openrouter_compact_payload, build_openrouter_payload};
use crate::providers::tabcode::{build_tabcode_compact_payload, build_tabcode_payload};
use crate::providers::zai::{ZaiChatRequest, build_zai_chat_request};
use crate::schema::openai::{ChatRequest, CompactRequest, ResponsesRequest};

#[derive(Debug, Clone, Copy)]
pub enum ProviderKind {
    OpenAi,
    OpenRouter,
    Tabcode,
    Gemini,
    Zai,
}

#[derive(Debug, Clone, Copy)]
pub enum ProviderRequest<'a> {
    Responses(&'a ResponsesRequest),
    Compact(&'a CompactRequest),
    Chat(&'a ChatRequest),
}

#[derive(Debug, Clone)]
pub enum ProviderPayload {
    OpenAiResponses(OpenAiResponsesPayload),
    OpenAiCompact(OpenAiCompactPayload),
    Gemini(GeminiRequestBody),
    Zai(ZaiChatRequest),
}

#[derive(Debug, Clone)]
pub struct ProviderBuildContext {
    pub model: String,
    pub reasoning: Option<OpenAiReasoning>,
    pub default_max_output_tokens: Option<u64>,
    pub compact_max_tokens: Option<u64>,
    pub compact_max_output_tokens: Option<u64>,
    pub compact_temperature: Option<f64>,
    pub thinking_budget: Option<u64>,
    pub thinking_enabled: Option<bool>,
}

impl ProviderBuildContext {
    pub fn new(model: impl Into<String>) -> Self {
        Self {
            model: model.into(),
            reasoning: None,
            default_max_output_tokens: None,
            compact_max_tokens: None,
            compact_max_output_tokens: None,
            compact_temperature: None,
            thinking_budget: None,
            thinking_enabled: None,
        }
    }
}

pub fn build_provider_payload(
    provider: ProviderKind,
    request: ProviderRequest<'_>,
    ctx: &ProviderBuildContext,
) -> Result<ProviderPayload, AgentError> {
    match (provider, request) {
        (ProviderKind::OpenAi, ProviderRequest::Responses(req)) => {
            Ok(ProviderPayload::OpenAiResponses(build_openai_payload(
                req,
                &ctx.model,
                ctx.reasoning.clone(),
                ctx.default_max_output_tokens,
            )))
        }
        (ProviderKind::OpenAi, ProviderRequest::Compact(req)) => Ok(
            ProviderPayload::OpenAiCompact(build_openai_compact_payload(
                req,
                &ctx.model,
                ctx.reasoning.clone(),
                ctx.compact_max_tokens,
                ctx.compact_max_output_tokens,
                ctx.compact_temperature,
            )),
        ),
        (ProviderKind::OpenRouter, ProviderRequest::Responses(req)) => {
            let default_max_output_tokens = ctx
                .default_max_output_tokens
                .ok_or_else(|| AgentError::new("missing default_max_output_tokens"))?;
            Ok(ProviderPayload::OpenAiResponses(build_openrouter_payload(
                req,
                &ctx.model,
                ctx.reasoning.clone(),
                default_max_output_tokens,
            )))
        }
        (ProviderKind::OpenRouter, ProviderRequest::Compact(req)) => Ok(
            ProviderPayload::OpenAiCompact(build_openrouter_compact_payload(
                req,
                &ctx.model,
                ctx.reasoning.clone(),
                ctx.compact_max_tokens,
                ctx.compact_max_output_tokens,
                ctx.compact_temperature,
            )),
        ),
        (ProviderKind::Tabcode, ProviderRequest::Responses(req)) => {
            Ok(ProviderPayload::OpenAiResponses(build_tabcode_payload(
                req,
                &ctx.model,
                ctx.reasoning.clone(),
                ctx.default_max_output_tokens,
            )))
        }
        (ProviderKind::Tabcode, ProviderRequest::Compact(req)) => Ok(
            ProviderPayload::OpenAiCompact(build_tabcode_compact_payload(
                req,
                &ctx.model,
                ctx.reasoning.clone(),
                ctx.compact_max_tokens,
                ctx.compact_max_output_tokens,
                ctx.compact_temperature,
            )),
        ),
        (ProviderKind::Gemini, ProviderRequest::Chat(req)) => Ok(ProviderPayload::Gemini(
            build_gemini_request(req, ctx.thinking_budget),
        )),
        (ProviderKind::Zai, ProviderRequest::Chat(req)) => Ok(ProviderPayload::Zai(
            build_zai_chat_request(req, &ctx.model, ctx.thinking_enabled),
        )),
        _ => Err(AgentError::new("Provider request type mismatch")),
    }
}
