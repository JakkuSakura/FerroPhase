//! Minimal, permissive LSP server for FerroPhase (.fp) files: syntax diagnostics only, no type checking/completion/hover.

use std::path::PathBuf;

use lsp_server::{Connection, ExtractError, Message, Notification};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification as _,
    PublishDiagnostics,
};
use lsp_types::{
    Diagnostic as LspDiagnostic, DiagnosticSeverity, DidChangeTextDocumentParams,
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, InitializeParams, Position,
    PublishDiagnosticsParams, Range, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, Uri,
};

use fp_core::diagnostics::DiagnosticLevel;
use fp_lang::ast::FerroPhaseParser;

fn main() -> eyre::Result<()> {
    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        ..Default::default()
    };
    let initialize_params = connection.initialize(serde_json::to_value(server_capabilities)?)?;
    let _initialize_params: InitializeParams = serde_json::from_value(initialize_params)?;

    main_loop(&connection)?;

    // Drop the connection (and its message sender) before joining the io threads: the writer
    // thread only terminates once all `Sender<Message>` handles are gone, and `connection` still
    // holds one for as long as it's in scope.
    drop(connection);
    io_threads.join()?;
    Ok(())
}

fn main_loop(connection: &Connection) -> eyre::Result<()> {
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                // `handle_shutdown` responds to a `shutdown` request and then blocks until it
                // sees the follow-up `exit` notification, returning `true` once that happens.
                // Any other request is not implemented; this server is intentionally minimal.
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
            }
            Message::Notification(not) => {
                handle_notification(connection, not)?;
            }
            Message::Response(_) => {
                // We never issue requests to the client, so nothing to do here.
            }
        }
    }
    Ok(())
}

fn handle_notification(connection: &Connection, not: Notification) -> eyre::Result<()> {
    match not.method.as_str() {
        m if m == DidOpenTextDocument::METHOD => {
            let params: DidOpenTextDocumentParams = cast_notification(not)?;
            let uri = params.text_document.uri;
            let text = params.text_document.text;
            publish_syntax_diagnostics(connection, &uri, &text)?;
        }
        m if m == DidChangeTextDocument::METHOD => {
            let params: DidChangeTextDocumentParams = cast_notification(not)?;
            let uri = params.text_document.uri;
            // Full sync: the last content change carries the entire document text.
            if let Some(change) = params.content_changes.into_iter().last() {
                publish_syntax_diagnostics(connection, &uri, &change.text)?;
            }
        }
        m if m == DidCloseTextDocument::METHOD => {
            let params: DidCloseTextDocumentParams = cast_notification(not)?;
            let uri = params.text_document.uri;
            publish_diagnostics(connection, &uri, Vec::new())?;
        }
        _ => {
            // Ignore anything else; this server is intentionally minimal.
        }
    }
    Ok(())
}

fn cast_notification<P>(not: Notification) -> eyre::Result<P>
where
    P: serde::de::DeserializeOwned,
{
    let method = not.method.clone();
    match not.extract(method.as_str()) {
        Ok(params) => Ok(params),
        Err(ExtractError::JsonError { method, error }) => {
            eyre::bail!("invalid params for {method}: {error}")
        }
        Err(ExtractError::MethodMismatch(not)) => {
            eyre::bail!("method mismatch: {}", not.method)
        }
    }
}

/// Parse `text` permissively (catching panics defensively, since this server must never crash on
/// garbage `.fp` content) and publish any syntax diagnostics produced by the lexer/parser.
fn publish_syntax_diagnostics(connection: &Connection, uri: &Uri, text: &str) -> eyre::Result<()> {
    let diagnostics = collect_syntax_diagnostics(uri, text);
    publish_diagnostics(connection, uri, diagnostics)
}

fn collect_syntax_diagnostics(uri: &Uri, text: &str) -> Vec<LspDiagnostic> {
    // `Uri` (lsp-types 0.97) has no filesystem-path conversion; we only need a stable label for
    // the parser's diagnostics, so use the URI string as-is rather than resolving a real path.
    let path = PathBuf::from(uri.as_str());

    let result = std::panic::catch_unwind(|| {
        let parser = FerroPhaseParser::new();
        // file id 0 lets the parser register/resolve the file id itself from the path+source.
        let _ = parser.parse_file_ast_with_file(text, 0, Some(path.as_path()), path.clone());
        parser.diagnostics().get_diagnostics()
    });

    let raw_diagnostics = match result {
        Ok(diags) => diags,
        Err(_) => {
            // The parser panicked on malformed input; report a generic diagnostic instead of
            // propagating the panic (this server must stay alive regardless of input).
            return vec![LspDiagnostic {
                range: Range::new(Position::new(0, 0), Position::new(0, 1)),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("fp-lsp".to_string()),
                message: "internal parser error while parsing this document".to_string(),
                ..Default::default()
            }];
        }
    };

    let line_starts = compute_line_starts(text);

    raw_diagnostics
        .into_iter()
        .map(|diag| to_lsp_diagnostic(&diag, text, &line_starts))
        .collect()
}

fn to_lsp_diagnostic(
    diag: &fp_core::diagnostics::Diagnostic,
    text: &str,
    line_starts: &[usize],
) -> LspDiagnostic {
    let severity = match diag.level {
        DiagnosticLevel::Error => DiagnosticSeverity::ERROR,
        DiagnosticLevel::Warning => DiagnosticSeverity::WARNING,
        DiagnosticLevel::Info => DiagnosticSeverity::INFORMATION,
    };

    let range = match diag.span {
        Some(span) => {
            let lo = (span.lo as usize).min(text.len());
            let hi = (span.hi as usize).min(text.len()).max(lo);
            Range::new(
                offset_to_position(lo, line_starts),
                offset_to_position(hi, line_starts),
            )
        }
        None => Range::new(Position::new(0, 0), Position::new(0, 1)),
    };

    LspDiagnostic {
        range,
        severity: Some(severity),
        source: Some("fp-lsp".to_string()),
        message: diag.message.clone(),
        ..Default::default()
    }
}

fn compute_line_starts(text: &str) -> Vec<usize> {
    let mut starts = vec![0usize];
    for (idx, ch) in text.char_indices() {
        if ch == '\n' {
            starts.push(idx + 1);
        }
    }
    starts
}

/// Converts a byte offset into a zero-indexed LSP `Position`.
///
/// NOTE: LSP `character` is defined in UTF-16 code units; here we use a byte-offset
/// approximation instead, which is only correct for ASCII text. Good enough for this
/// intentionally minimal, permissive server.
fn offset_to_position(offset: usize, line_starts: &[usize]) -> Position {
    let line_idx = match line_starts.binary_search(&offset) {
        Ok(idx) => idx,
        Err(idx) => idx.saturating_sub(1),
    };
    let line_start = line_starts.get(line_idx).copied().unwrap_or(0);
    let character = offset.saturating_sub(line_start);
    Position::new(line_idx as u32, character as u32)
}

fn publish_diagnostics(
    connection: &Connection,
    uri: &Uri,
    diagnostics: Vec<LspDiagnostic>,
) -> eyre::Result<()> {
    let params = PublishDiagnosticsParams {
        uri: uri.clone(),
        diagnostics,
        version: None,
    };
    let notification = Notification::new(PublishDiagnostics::METHOD.to_string(), params);
    connection
        .sender
        .send(Message::Notification(notification))?;
    Ok(())
}
