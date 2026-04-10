use fp_agent::schema::openai::{ResponsesRequest, Tool};
use fp_agent::validate::validate_responses_request;

#[test]
fn allows_custom_tool_type() {
    let request = ResponsesRequest {
        model: "gpt-5.2".into(),
        input: Some(fp_agent::schema::openai::ResponsesInput::Text("ping".into())),
        messages: None,
        instructions: None,
        previous_response_id: None,
        store: None,
        metadata: None,
        tools: Some(vec![Tool {
            tool_type: "custom".into(),
            function: None,
            name: Some("codex_tool".into()),
            description: Some("custom tool".into()),
            parameters: None,
            strict: None,
            web_search: None,
        }]),
        tool_choice: None,
        temperature: None,
        top_p: None,
        max_tokens: None,
        max_output_tokens: None,
        stream: None,
        include: None,
    };

    validate_responses_request(&request).expect("custom tool should be allowed");
}
