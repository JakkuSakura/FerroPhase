use serde::Serialize;
use serde_json::Value;
use std::collections::HashMap;

use crate::schema::openai::{ChatContent, ChatMessage, ChatRequest};

pub fn sanitize_params(params: &Value) -> Value {
    match params {
        Value::Object(map) => {
            let filtered = map
                .iter()
                .filter(|(k, _)| {
                    ![
                        "additionalProperties",
                        "title",
                        "default",
                        "minItems",
                        "maxItems",
                        "uniqueItems",
                    ]
                    .contains(&k.as_str())
                })
                .map(|(k, v)| (k.clone(), sanitize_params(v)))
                .collect();
            Value::Object(filtered)
        }
        Value::Array(arr) => Value::Array(arr.iter().map(sanitize_params).collect()),
        other => other.clone(),
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct GeminiContent {
    pub role: String,
    pub parts: Vec<GeminiPart>,
}

#[derive(Clone, Debug, Serialize)]
pub struct GeminiSystemInstruction {
    pub parts: Vec<GeminiPart>,
}

#[derive(Clone, Debug, Serialize)]
pub struct GeminiPart {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub thought: Option<bool>,

    #[serde(rename = "functionCall", skip_serializing_if = "Option::is_none")]
    pub function_call: Option<GeminiFunctionCall>,

    #[serde(rename = "functionResponse", skip_serializing_if = "Option::is_none")]
    pub function_response: Option<GeminiFunctionResponse>,

    #[serde(rename = "thoughtSignature", skip_serializing_if = "Option::is_none")]
    pub thought_signature: Option<String>,
}

#[derive(Clone, Debug, Serialize)]
pub struct GeminiFunctionCall {
    pub name: String,
    pub args: Value,
}

#[derive(Clone, Debug, Serialize)]
pub struct GeminiFunctionResponse {
    pub name: String,
    pub response: GeminiFunctionResponseBody,
}

#[derive(Clone, Debug, Serialize)]
pub struct GeminiFunctionResponseBody {
    pub content: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct GeminiThinkingConfig {
    #[serde(rename = "thinkingBudget")]
    pub thinking_budget: u64,
    #[serde(rename = "includeThoughts")]
    pub include_thoughts: bool,
}

#[derive(Clone, Debug, Serialize)]
pub struct GeminiGenerationConfig {
    pub temperature: f64,
    #[serde(rename = "topP")]
    pub top_p: f64,
    #[serde(rename = "maxOutputTokens", skip_serializing_if = "Option::is_none")]
    pub max_output_tokens: Option<u64>,
    #[serde(rename = "thinkingConfig", skip_serializing_if = "Option::is_none")]
    pub thinking_config: Option<GeminiThinkingConfig>,
}

#[derive(Clone, Debug, Serialize)]
pub struct GeminiFunctionDeclaration {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub parameters: Value,
}

#[derive(Clone, Debug, Serialize)]
pub struct GeminiTool {
    #[serde(
        rename = "functionDeclarations",
        skip_serializing_if = "Option::is_none"
    )]
    pub function_declarations: Option<Vec<GeminiFunctionDeclaration>>,
    #[serde(rename = "googleSearch", skip_serializing_if = "Option::is_none")]
    pub google_search: Option<GeminiEmptyObject>,
}

#[derive(Clone, Debug, Serialize)]
pub struct GeminiEmptyObject {}

#[derive(Clone, Debug, Serialize)]
pub struct GeminiFunctionCallingConfig {
    pub mode: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct GeminiToolConfig {
    #[serde(rename = "functionCallingConfig")]
    pub function_calling_config: GeminiFunctionCallingConfig,
}

#[derive(Clone, Debug, Serialize)]
pub struct GeminiRequestBody {
    pub contents: Vec<GeminiContent>,
    #[serde(rename = "generationConfig")]
    pub generation_config: GeminiGenerationConfig,
    #[serde(rename = "systemInstruction", skip_serializing_if = "Option::is_none")]
    pub system_instruction: Option<GeminiSystemInstruction>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tools: Option<Vec<GeminiTool>>,
    #[serde(rename = "toolConfig", skip_serializing_if = "Option::is_none")]
    pub tool_config: Option<GeminiToolConfig>,
}

#[derive(Clone, Debug, Serialize)]
pub struct GeminiInternalBody {
    pub model: String,
    pub project: String,
    pub request: GeminiRequestBody,
}

pub fn build_gemini_request(
    request: &ChatRequest,
    thinking_budget: Option<u64>,
) -> GeminiRequestBody {
    let (contents, system_instruction) = map_messages(&request.messages);
    let (tools, tool_config) = apply_tools(request);
    let thinking_config = thinking_budget
        .filter(|b| *b > 0)
        .map(|budget| GeminiThinkingConfig {
            thinking_budget: budget,
            include_thoughts: true,
        });

    GeminiRequestBody {
        contents,
        generation_config: GeminiGenerationConfig {
            temperature: request.temperature.unwrap_or(1.0),
            top_p: request.top_p.unwrap_or(1.0),
            max_output_tokens: request.max_tokens,
            thinking_config,
        },
        system_instruction,
        tools,
        tool_config,
    }
}

pub fn map_messages(
    messages: &[ChatMessage],
) -> (Vec<GeminiContent>, Option<GeminiSystemInstruction>) {
    let mut contents: Vec<GeminiContent> = Vec::new();
    let mut system_parts: Vec<GeminiPart> = Vec::new();

    let mut tool_call_map: HashMap<String, String> = HashMap::new();
    for message in messages {
        for tool_call in &message.tool_calls {
            tool_call_map.insert(tool_call.id.clone(), tool_call.function.name.clone());
        }
    }

    for message in messages {
        let role = message.role.as_str();
        if role == "system" || role == "developer" {
            let text = chat_content_to_string(message.content.as_ref());
            if !text.is_empty() {
                system_parts.push(GeminiPart {
                    text: Some(text),
                    thought: None,
                    function_call: None,
                    function_response: None,
                    thought_signature: None,
                });
            }
            continue;
        }

        let mut parts: Vec<GeminiPart> = Vec::new();
        if let Some(reasoning) = message.reasoning_content.as_deref() {
            if !reasoning.is_empty() {
                parts.push(GeminiPart {
                    text: Some(reasoning.to_string()),
                    thought: Some(true),
                    function_call: None,
                    function_response: None,
                    thought_signature: None,
                });
            }
        }

        let text = chat_content_to_string(message.content.as_ref());
        if !text.is_empty() {
            parts.push(GeminiPart {
                text: Some(text),
                thought: None,
                function_call: None,
                function_response: None,
                thought_signature: None,
            });
        }

        for tool_call in &message.tool_calls {
            let args: Value = serde_json::from_str(&tool_call.function.arguments)
                .unwrap_or(Value::Object(Default::default()));
            let thought_sig = message
                .thought_signature
                .as_deref()
                .unwrap_or("skip_thought_signature_validator")
                .to_string();
            parts.push(GeminiPart {
                text: None,
                thought: None,
                function_call: Some(GeminiFunctionCall {
                    name: tool_call.function.name.clone(),
                    args,
                }),
                function_response: None,
                thought_signature: Some(thought_sig),
            });
        }

        let gemini_role = if role == "assistant" { "model" } else { "user" };

        if role == "tool" {
            let tool_call_id = message.tool_call_id.as_deref().unwrap_or("unknown");
            let function_name = tool_call_map
                .get(tool_call_id)
                .map(|s| s.as_str())
                .or_else(|| message.name.as_deref())
                .unwrap_or("unknown");
            let content = chat_content_to_string(message.content.as_ref());
            let resp_part = GeminiPart {
                text: None,
                thought: None,
                function_call: None,
                function_response: Some(GeminiFunctionResponse {
                    name: function_name.to_string(),
                    response: GeminiFunctionResponseBody { content },
                }),
                thought_signature: None,
            };

            let tool_role = "user".to_string();
            if let Some(last) = contents.last_mut() {
                if last.role == tool_role
                    && last
                        .parts
                        .iter()
                        .any(|part| part.function_response.is_some())
                {
                    last.parts.push(resp_part);
                    continue;
                }
            }

            contents.push(GeminiContent {
                role: tool_role,
                parts: vec![resp_part],
            });
            continue;
        }

        if parts.is_empty() {
            continue;
        }

        if let Some(last) = contents.last_mut() {
            if last.role == gemini_role {
                last.parts.extend(parts);
                continue;
            }
        }

        contents.push(GeminiContent {
            role: gemini_role.to_string(),
            parts,
        });
    }

    let system_instruction = if system_parts.is_empty() {
        None
    } else {
        Some(GeminiSystemInstruction {
            parts: system_parts,
        })
    };
    (contents, system_instruction)
}

pub fn apply_tools(request: &ChatRequest) -> (Option<Vec<GeminiTool>>, Option<GeminiToolConfig>) {
    let mut tool_decls: Vec<GeminiFunctionDeclaration> = Vec::new();
    for tool in &request.tools {
        if tool.tool_type != "function" {
            continue;
        }
        let func = match &tool.function {
            Some(f) => f,
            None => continue,
        };
        let params = func
            .parameters
            .as_ref()
            .cloned()
            .unwrap_or_else(|| Value::Object(Default::default()));
        tool_decls.push(GeminiFunctionDeclaration {
            name: func.name.clone(),
            description: func.description.clone(),
            parameters: sanitize_params(&params),
        });
    }

    let mut tools: Vec<GeminiTool> = Vec::new();
    if !tool_decls.is_empty() {
        tools.push(GeminiTool {
            function_declarations: Some(tool_decls),
            google_search: None,
        });
    }

    if request.include.iter().any(|v| v == "search") {
        tools.push(GeminiTool {
            function_declarations: None,
            google_search: Some(GeminiEmptyObject {}),
        });
    }

    let tools_opt = if tools.is_empty() { None } else { Some(tools) };

    let tool_choice = request.tool_choice.as_deref().unwrap_or("auto");
    let mode = match tool_choice {
        "none" => "NONE",
        _ => "ANY",
    };
    let tool_config = if tools_opt.is_some() {
        Some(GeminiToolConfig {
            function_calling_config: GeminiFunctionCallingConfig {
                mode: mode.to_string(),
            },
        })
    } else {
        None
    };

    (tools_opt, tool_config)
}

fn chat_content_to_string(content: Option<&ChatContent>) -> String {
    match content {
        None => String::new(),
        Some(ChatContent::Text(text)) => text.clone(),
        Some(ChatContent::Parts(parts)) => parts
            .iter()
            .filter_map(|part| part.text.clone())
            .collect::<Vec<_>>()
            .join(""),
    }
}
