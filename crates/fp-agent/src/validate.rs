use crate::AgentError;
use crate::schema::openai::{CompactRequest, ResponsesInput, ResponsesRequest, Tool};

pub fn validate_responses_request(req: &ResponsesRequest) -> Result<(), AgentError> {
    validate_model(&req.model)?;

    if let Some(t) = req.temperature {
        if !(0.0..=2.0).contains(&t) {
            return Err(AgentError::new(format!(
                "temperature must be between 0 and 2, got: {t}"
            )));
        }
    }

    if let Some(mt) = req.max_tokens {
        if !(1..=128_000).contains(&mt) {
            return Err(AgentError::new(format!(
                "max_tokens must be between 1 and 128000, got: {mt}"
            )));
        }
    }
    if let Some(mo) = req.max_output_tokens {
        if !(1..=128_000).contains(&mo) {
            return Err(AgentError::new(format!(
                "max_output_tokens must be between 1 and 128000, got: {mo}"
            )));
        }
    }

    if let Some(input) = &req.input {
        validate_input(input)?;
    }
    if req.input.is_none() && req.messages.is_none() {
        return Err(AgentError::new(
            "request.input or request.messages is required",
        ));
    }

    if let Some(tools) = &req.tools {
        validate_tools(tools)?;
    }

    Ok(())
}

pub fn validate_compact_request(_req: &CompactRequest) -> Result<(), AgentError> {
    Ok(())
}

fn validate_model(model: &str) -> Result<(), AgentError> {
    if model.len() > 100 {
        return Err(AgentError::new(format!(
            "Invalid model name (too long): {model}"
        )));
    }
    Ok(())
}

fn validate_input(input: &ResponsesInput) -> Result<(), AgentError> {
    match input {
        ResponsesInput::Text(_) => Ok(()),
        ResponsesInput::Items(_) => Ok(()),
    }
}

fn validate_tools(tools: &[Tool]) -> Result<(), AgentError> {
    for (i, tool) in tools.iter().enumerate() {
        match tool.tool_type.as_str() {
            "function" | "web_search" | "retrieval" | "custom" => {}
            other => {
                return Err(AgentError::new(format!(
                    "Tool {i} has invalid type: {other}"
                )));
            }
        }
        if tool.tool_type == "function" && tool.function.is_none() && tool.name.is_none() {
            return Err(AgentError::new(format!(
                "Tool {i} of type function must have 'function' or 'name'"
            )));
        }
        if tool.tool_type == "custom" && tool.function.is_none() && tool.name.is_none() {
            return Err(AgentError::new(format!(
                "Tool {i} of type custom must have 'function' or 'name'"
            )));
        }
    }
    Ok(())
}
