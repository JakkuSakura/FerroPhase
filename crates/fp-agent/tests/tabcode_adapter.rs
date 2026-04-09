use fp_agent::providers::openai::OpenAiReasoning;
use fp_agent::providers::tabcode::{build_tabcode_compact_payload, build_tabcode_payload};
use fp_agent::schema::openai::{
    CompactRequest, Content, InputItem, Instructions, ResponsesInput, ResponsesRequest, TextPart,
};

#[test]
fn tabcode_payload_preserves_items_and_instructions() {
    let request = ResponsesRequest {
        model: "gpt-5.2".into(),
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

    let payload = build_tabcode_payload(
        &request,
        "gpt-5.2",
        Some(OpenAiReasoning { effort: "low" }),
        None,
    );

    match payload.input {
        Some(ResponsesInput::Items(_)) => {}
        other => panic!("expected tabcode input to preserve items, got {other:?}"),
    }

    match payload.instructions {
        Some(Instructions::Parts(_)) => {}
        other => panic!("expected tabcode instructions to preserve parts, got {other:?}"),
    }
}

#[test]
fn tabcode_compact_payload_preserves_items_and_instructions() {
    let request = CompactRequest {
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

    let payload = build_tabcode_compact_payload(
        &request,
        "gpt-5.2",
        Some(OpenAiReasoning { effort: "low" }),
        Some(64),
        Some(64),
        None,
    );

    match payload.input {
        ResponsesInput::Items(_) => {}
        other => panic!("expected tabcode compact input to preserve items, got {other:?}"),
    }

    match payload.instructions {
        Instructions::Parts(_) => {}
        other => panic!("expected tabcode compact instructions to preserve parts, got {other:?}"),
    }
}
