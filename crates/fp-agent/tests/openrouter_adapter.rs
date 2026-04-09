use fp_agent::providers::openai::OpenAiReasoning;
use fp_agent::providers::openrouter::{build_openrouter_compact_payload, build_openrouter_payload};
use fp_agent::schema::openai::{
    CompactRequest, Content, InputItem, Instructions, ResponsesInput, ResponsesRequest, TextPart,
};

#[test]
fn openrouter_payload_coerces_items_and_instructions_to_text() {
    let request = ResponsesRequest {
        model: "glm-5.2-turbo".into(),
        input: Some(ResponsesInput::Items(vec![InputItem {
            item_type: "message".into(),
            id: None,
            call_id: None,
            role: Some("user".into()),
            name: None,
            content: Some(Content::Text("hi".into())),
            reasoning_content: None,
            thought_signature: None,
            thought: None,
            arguments: None,
            input: None,
            action: None,
            command: None,
            cwd: None,
            working_directory: None,
            changes: None,
            output: None,
            stdout: None,
            stderr: None,
            encrypted_content: None,
        }])),
        messages: None,
        instructions: Some(Instructions::Parts(vec![TextPart::Text("sys".into())])),
        previous_response_id: None,
        store: None,
        metadata: None,
        tools: None,
        tool_choice: None,
        temperature: None,
        top_p: None,
        max_tokens: None,
        max_output_tokens: None,
        stream: None,
        include: None,
    };

    let payload = build_openrouter_payload(
        &request,
        "z-ai/glm-5.2-turbo",
        Some(OpenAiReasoning { effort: "low" }),
        4096,
    );

    match payload.input {
        Some(ResponsesInput::Text(_)) => {}
        other => panic!("expected input to be coerced to text, got {other:?}"),
    }

    match payload.instructions {
        Some(Instructions::Text(_)) => {}
        other => panic!("expected instructions to be coerced to text, got {other:?}"),
    }
}

#[test]
fn openrouter_compact_payload_coerces_items_and_instructions_to_text() {
    let request = CompactRequest {
        model: "glm-5.2-turbo".into(),
        input: ResponsesInput::Items(vec![InputItem {
            item_type: "message".into(),
            id: None,
            call_id: None,
            role: Some("user".into()),
            name: None,
            content: Some(Content::Text("hi".into())),
            reasoning_content: None,
            thought_signature: None,
            thought: None,
            arguments: None,
            input: None,
            action: None,
            command: None,
            cwd: None,
            working_directory: None,
            changes: None,
            output: None,
            stdout: None,
            stderr: None,
            encrypted_content: None,
        }]),
        instructions: Instructions::Parts(vec![TextPart::Text("sys".into())]),
    };

    let payload = build_openrouter_compact_payload(
        &request,
        "z-ai/glm-5.2-turbo",
        Some(OpenAiReasoning { effort: "low" }),
        Some(64),
        Some(64),
        None,
    );

    match payload.input {
        ResponsesInput::Text(_) => {}
        other => panic!("expected compact input to be coerced to text, got {other:?}"),
    }

    match payload.instructions {
        Instructions::Text(_) => {}
        other => panic!("expected compact instructions to be coerced to text, got {other:?}"),
    }
}
