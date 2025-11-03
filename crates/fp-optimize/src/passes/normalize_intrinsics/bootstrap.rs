use fp_core::ast::{
    BlockStmt, Expr, ExprBlock, ExprField, ExprInvoke, ExprInvokeTarget, ExprKind,
    ExprStruct, Ident, ItemDefFunction, Locator, Path, Value,
};

fn path_tail_matches(path: &Path, tail: &[&str]) -> bool {
    if tail.is_empty() {
        return false;
    }
    let n = path.segments.len();
    if n < tail.len() {
        return false;
    }
    for (i, want) in tail.iter().rev().enumerate() {
        let got = path.segments[n - 1 - i].as_str();
        if got != *want {
            return false;
        }
    }
    true
}

fn path_matches(locator: &Locator, tail: &[&str]) -> bool {
    match locator {
        Locator::Path(path) => path_tail_matches(path, tail),
        Locator::Ident(ident) => tail.len() == 1 && ident.as_str() == tail[0],
        Locator::ParameterPath(pp) => pp
            .last()
            .map(|seg| tail.len() == 1 && seg.ident.as_str() == tail[0])
            .unwrap_or(false),
    }
}

fn make_empty_string_expr() -> Expr {
    Expr::new(ExprKind::Value(Box::new(Value::string(String::new()))))
}

pub fn maybe_bootstrap_invoke_replacement(invoke: &ExprInvoke) -> Option<Expr> {
    if std::env::var_os("FERROPHASE_BOOTSTRAP").is_none() {
        return None;
    }
    match &invoke.target {
        // std::env::var(_) => ""
        ExprInvokeTarget::Function(locator)
            if path_matches(locator, &["std", "env", "var"]) ||
                path_matches(locator, &["env", "var"]) =>
        {
            return Some(make_empty_string_expr());
        }
        _ => {}
    }
    None
}

pub fn is_bootstrap_cli_side_effect_call(invoke: &ExprInvoke) -> bool {
    if std::env::var_os("FERROPHASE_BOOTSTRAP").is_none() {
        return false;
    }
    // Only rewrite main when explicitly requested. Default keeps the standard CLI path.
    if std::env::var_os("FP_BOOTSTRAP_REWRITE_MAIN").is_none() {
        return false;
    }
    match &invoke.target {
        ExprInvokeTarget::Function(func) => {
            let name = match func {
                Locator::Ident(ident) => ident.as_str(),
                Locator::Path(path) => path.last().as_str(),
                Locator::ParameterPath(pp) => pp.last().map(|seg| seg.ident.as_str()).unwrap_or(""),
            };
            matches!(
                name,
                // diagnostics/logging setup
                "setup_error_reporting"
                    | "setup_logging"
                    | "registry"
                    | "builder"
                    | "subscriber"
                    | "layer"
                    | "with"
                    | "with_level"
                    | "with_timer"
                    | "with_target"
                    | "init"
                    | "fmt"
            )
        }
        _ => false,
    }
}

// Attempt to rewrite fp-cli::bin main into a direct compile execution when bootstrapping.
// This avoids relying on Clap parsing and other side-effectful runtime setup.
pub fn maybe_rewrite_cli_main(function: &mut ItemDefFunction) -> bool {
    if std::env::var_os("FERROPHASE_BOOTSTRAP").is_none() {
        return false;
    }
    if function.name.as_str() != "main" {
        return false;
    }

    // Derive snapshot and output paths from environment at compile time.
    // These values are baked into the Stage 1 compiler so it can deterministically
    // emit Stage 2 LLVM without relying on external libraries (Clap/Std IO).
    let snapshot_path = match std::env::var("FP_BOOTSTRAP_SNAPSHOT") {
        Ok(s) if !s.trim().is_empty() => s,
        _ => return false,
    };
    let output_path = match std::env::var("FP_BOOTSTRAP_OUTPUT") {
        Ok(s) if !s.trim().is_empty() => s,
        _ => return false,
    };

    let mut stmts: Vec<BlockStmt> = Vec::new();

    // let mut pipeline = fp_cli::pipeline::Pipeline::new();
    let pipeline_new = Expr::new(ExprKind::Invoke(ExprInvoke {
        target: ExprInvokeTarget::Function(Locator::path(Path::new(vec![
            Ident::new("fp_cli"),
            Ident::new("pipeline"),
            Ident::new("Pipeline"),
            Ident::new("new"),
        ]))),
        args: vec![],
    }));
    let pipeline_ident = Ident::new("pipeline");
    let pipe_let = fp_core::ast::StmtLet::new_simple(pipeline_ident.clone(), pipeline_new.into());
    stmts.push(BlockStmt::Let(pipe_let));

    // Build PipelineOptions { target: Llvm, base_path: Some(output_path), bootstrap_mode: true, ..Default::default() }
    let options_expr = Expr::new(ExprKind::Struct(ExprStruct {
        name: Box::new(Expr::locator(Locator::path(Path::new(vec![
            Ident::new("fp_cli"),
            Ident::new("config"),
            Ident::new("PipelineOptions"),
        ])))),
        fields: vec![
            ExprField::new(
                Ident::new("target"),
                Expr::locator(Locator::path(Path::new(vec![
                    Ident::new("fp_cli"),
                    Ident::new("config"),
                    Ident::new("PipelineTarget"),
                    Ident::new("Llvm"),
                ]))),
            ),
            ExprField::new(
                Ident::new("base_path"),
                Expr::new(ExprKind::Invoke(ExprInvoke {
                    target: ExprInvokeTarget::Function(Locator::path(Path::new(vec![
                        Ident::new("Some"),
                    ]))),
                    args: vec![Expr::new(ExprKind::Value(Box::new(Value::string(
                        output_path.clone(),
                    ))))],
                })),
            ),
            ExprField::new(
                Ident::new("save_intermediates"),
                Expr::new(ExprKind::Value(Box::new(Value::bool(true)))),
            ),
            ExprField::new(
                Ident::new("bootstrap_mode"),
                Expr::new(ExprKind::Value(Box::new(Value::bool(true)))),
            ),
        ],
    }));

    // Call as an associated function to avoid method-call lowering pitfalls:
    // fp_cli::pipeline::Pipeline::execute_compilation_from_snapshot_blocking(pipeline, snapshot, options)
    let exec_call = Expr::new(ExprKind::Invoke(ExprInvoke {
        target: ExprInvokeTarget::Function(Locator::path(Path::new(vec![
            Ident::new("fp_cli"),
            Ident::new("pipeline"),
            Ident::new("Pipeline"),
            Ident::new("execute_compilation_from_snapshot_blocking"),
        ]))),
        args: vec![
            Expr::ident(pipeline_ident.clone()),
            Expr::new(ExprKind::Value(Box::new(Value::string(snapshot_path.clone())))),
            options_expr,
        ],
    }));

    // Write value to a dummy let to ensure expression is executed.
    let _result_ident = Ident::new("_result");
    let result_let = fp_core::ast::StmtLet::new_simple(_result_ident, exec_call.into());
    stmts.push(BlockStmt::Let(result_let));

    let body = ExprBlock::new_stmts(stmts);
    function.body = Expr::block(body).into();
    true
}
