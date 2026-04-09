use crate::schema::openai::{
    ChatContent, ChatMessage, ChatRequest, Content, InputItem, Instructions, ResponsesInput,
    ResponsesRequest, TextPart, Tool, ToolCall, ToolCallFunction,
};

pub fn normalize_responses_request(req: &ResponsesRequest) -> ChatRequest {
    let mut messages: Vec<ChatMessage> = Vec::new();

    if let Some(instructions) = &req.instructions {
        let content = instructions_text(instructions);
        if !content.is_empty() {
            messages.push(ChatMessage {
                role: "system".into(),
                content: Some(ChatContent::Text(content)),
                reasoning_content: None,
                thought_signature: None,
                tool_calls: Vec::new(),
                tool_call_id: None,
                name: None,
            });
        }
    }

    if let Some(input) = &req.input {
        match input {
            ResponsesInput::Text(text) => {
                messages.push(ChatMessage {
                    role: "user".into(),
                    content: Some(ChatContent::Text(text.clone())),
                    reasoning_content: None,
                    thought_signature: None,
                    tool_calls: Vec::new(),
                    tool_call_id: None,
                    name: None,
                });
            }
            ResponsesInput::Items(items) => {
                for item in items {
                    process_input_item(item, &mut messages);
                }
            }
        }
    } else if let Some(messages_input) = &req.messages {
        messages.extend(messages_input.clone());
    }

    let tools = req
        .tools
        .as_ref()
        .map(|tools| tools.iter().cloned().map(normalize_tool).collect())
        .unwrap_or_default();

    ChatRequest {
        model: req.model.clone(),
        messages,
        tools,
        tool_choice: req.tool_choice.clone(),
        temperature: req.temperature,
        top_p: req.top_p,
        max_tokens: req.max_tokens,
        stream: req.stream.unwrap_or(false),
        store: req.store.unwrap_or(false),
        metadata: req.metadata.clone().unwrap_or_default(),
        previous_response_id: req.previous_response_id.clone(),
        include: req.include.clone().unwrap_or_default(),
    }
}

fn instructions_text(instructions: &Instructions) -> String {
    match instructions {
        Instructions::Text(text) => text.clone(),
        Instructions::Parts(parts) => parts
            .iter()
            .map(|part| match part {
                TextPart::Text(text) => text.clone(),
                TextPart::Obj { text } => text.clone(),
            })
            .collect::<Vec<_>>()
            .join(""),
    }
}

fn process_input_item(item: &InputItem, messages: &mut Vec<ChatMessage>) {
    let item_type = if item.item_type.is_empty() {
        "message"
    } else {
        item.item_type.as_str()
    };

    match item_type {
        "message" | "agentMessage" => process_message(item, messages),
        "reasoning" => process_reasoning(item, messages),
        "function_call"
        | "commandExecution"
        | "local_shell_call"
        | "fileChange"
        | "custom_tool_call"
        | "web_search_call" => process_tool_call(item, messages),
        "function_call_output"
        | "commandExecutionOutput"
        | "fileChangeOutput"
        | "custom_tool_call_output" => process_tool_output(item, messages),
        _ => {}
    }
}

fn process_message(item: &InputItem, messages: &mut Vec<ChatMessage>) {
    let role = item.role.as_deref().unwrap_or("user");
    let role = if role == "developer" { "system" } else { role };
    let reasoning_content = item.reasoning_content.clone().filter(|s| !s.is_empty());
    let content = extract_content_text(item.content.as_ref());

    if role == "assistant" || role == "model" {
        let idx = ensure_last_assistant(messages);
        let msg = &mut messages[idx];
        let current_content = chat_content_to_string(msg.content.as_ref());
        let merged = format!("{current_content}{content}");
        msg.content = Some(ChatContent::Text(merged));

        if let Some(rc) = reasoning_content {
            let current_rc = msg.reasoning_content.clone().unwrap_or_default();
            msg.reasoning_content = Some(format!("{current_rc}{rc}"));
        }
        if let Some(sig) = item.thought_signature.clone() {
            msg.thought_signature = Some(sig);
        }
        return;
    }

    messages.push(ChatMessage {
        role: role.to_string(),
        content: Some(ChatContent::Text(content)),
        reasoning_content,
        thought_signature: item.thought_signature.clone(),
        tool_calls: Vec::new(),
        tool_call_id: None,
        name: None,
    });
}

fn process_reasoning(item: &InputItem, messages: &mut Vec<ChatMessage>) {
    let content = extract_content_text(item.content.as_ref());
    if content.is_empty() {
        return;
    }

    let idx = ensure_last_assistant(messages);
    let msg = &mut messages[idx];
    let current_rc = msg.reasoning_content.clone().unwrap_or_default();
    msg.reasoning_content = Some(format!("{current_rc}{content}"));
    if let Some(sig) = item.thought_signature.clone() {
        msg.thought_signature = Some(sig);
    }
}

fn process_tool_call(item: &InputItem, messages: &mut Vec<ChatMessage>) {
    let call_id = item
        .call_id
        .as_deref()
        .or(item.id.as_deref())
        .unwrap_or("call_unknown")
        .to_string();

    let item_type = if item.item_type.is_empty() {
        "function_call"
    } else {
        item.item_type.as_str()
    };

    let name = match item.name.as_deref() {
        Some(n) => match item_type {
            "commandExecution" => "run_shell_command",
            "local_shell_call" => "local_shell_command",
            "fileChange" => "write_file",
            "web_search_call" => "web_search",
            _ => n,
        }
        .to_string(),
        None => "unknown".to_string(),
    };

    let args = item
        .arguments
        .as_ref()
        .or(item.input.as_ref())
        .or(item.action.as_ref());

    let args_str = args_to_json_string(args, item_type, item);

    let idx = ensure_last_assistant(messages);
    let msg = &mut messages[idx];
    msg.tool_calls.push(ToolCall {
        id: call_id.clone(),
        call_type: "function".into(),
        function: ToolCallFunction {
            name,
            arguments: args_str,
        },
    });

    if msg.thought_signature.is_none() {
        msg.thought_signature = item.thought_signature.clone();
    }
    if let Some(thought) = item.thought.clone() {
        let current_rc = msg.reasoning_content.clone().unwrap_or_default();
        msg.reasoning_content = Some(format!("{current_rc}{thought}"));
    }
}

fn args_to_json_string(
    args: Option<&serde_json::Value>,
    item_type: &str,
    item: &InputItem,
) -> String {
    if let Some(serde_json::Value::String(s)) = args {
        return s.clone();
    }
    if let Some(v) = args {
        return serde_json::to_string(v).unwrap_or_default();
    }

    match item_type {
        "commandExecution" => {
            let command = item.command.as_deref().unwrap_or("");
            let dir_path = item.cwd.as_deref().unwrap_or(".");
            format!(
                "{{\"command\":{},\"dir_path\":{}}}",
                serde_json::to_string(command).unwrap_or_default(),
                serde_json::to_string(dir_path).unwrap_or_default()
            )
        }
        "local_shell_call" => {
            let command = item
                .action
                .as_ref()
                .and_then(|a| match a {
                    serde_json::Value::Object(m) => m.get("exec"),
                    _ => None,
                })
                .and_then(|exec| match exec {
                    serde_json::Value::Object(m) => m.get("command"),
                    _ => None,
                })
                .map(|v| serde_json::to_string(v).unwrap_or_default())
                .unwrap_or_else(|| "[]".into());
            format!("{{\"command\":{command}}}")
        }
        "fileChange" => {
            let path = item
                .changes
                .as_ref()
                .and_then(|c| c.first())
                .map(|c| c.path.as_str())
                .unwrap_or("unknown");
            format!(
                "{{\"file_path\":{}}}",
                serde_json::to_string(path).unwrap_or_default()
            )
        }
        _ => "{}".into(),
    }
}

fn process_tool_output(item: &InputItem, messages: &mut Vec<ChatMessage>) {
    let call_id = item
        .call_id
        .as_deref()
        .or(item.id.as_deref())
        .unwrap_or("call_unknown")
        .to_string();

    let output_raw = item
        .output
        .as_ref()
        .or(item.content.as_ref())
        .or(item.stdout.as_ref());

    let mut content = extract_content_text(output_raw);
    if content.is_empty() {
        if let Some(stderr) = item.stderr.as_deref() {
            content = format!("Error: {stderr}");
        }
    }

    messages.push(ChatMessage {
        role: "tool".into(),
        content: Some(ChatContent::Text(content)),
        reasoning_content: None,
        thought_signature: None,
        tool_calls: Vec::new(),
        tool_call_id: Some(call_id),
        name: None,
    });
}

fn extract_content_text(content: Option<&Content>) -> String {
    match content {
        None => String::new(),
        Some(Content::Text(text)) => text.clone(),
        Some(Content::Parts(parts)) => parts
            .iter()
            .map(|part| match part.part_type.as_str() {
                "input_text" | "text" | "output_text" => part.text.clone().unwrap_or_default(),
                _ => String::new(),
            })
            .collect::<Vec<_>>()
            .join(""),
        Some(Content::Json(_)) => String::new(),
    }
}

fn ensure_last_assistant(messages: &mut Vec<ChatMessage>) -> usize {
    if let Some(last) = messages.last() {
        if last.role == "assistant" {
            return messages.len() - 1;
        }
    }
    messages.push(ChatMessage {
        role: "assistant".into(),
        content: None,
        reasoning_content: None,
        thought_signature: None,
        tool_calls: Vec::new(),
        tool_call_id: None,
        name: None,
    });
    messages.len() - 1
}

fn chat_content_to_string(content: Option<&ChatContent>) -> String {
    match content {
        None => String::new(),
        Some(ChatContent::Text(text)) => text.clone(),
        Some(ChatContent::Parts(parts)) => parts
            .iter()
            .map(|part| match part.part_type.as_str() {
                "input_text" | "text" | "output_text" => part.text.clone().unwrap_or_default(),
                _ => String::new(),
            })
            .collect::<Vec<_>>()
            .join(""),
    }
}

fn normalize_tool(mut tool: Tool) -> Tool {
    if tool.tool_type == "function" && tool.function.is_none() {
        if let Some(name) = tool.name.clone() {
            tool.function = Some(crate::schema::openai::FunctionDef {
                name,
                description: tool.description.clone(),
                parameters: tool.parameters.clone(),
            });
        }
    }
    tool
}
