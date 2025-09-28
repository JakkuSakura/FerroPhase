use crate::{
    Result,
    cli::CliConfig,
    pipeline::{Pipeline, PipelineConfig, PipelineInput},
};
use fp_core::pretty::{PrettyOptions, pretty};
use std::path::PathBuf;

#[derive(Debug)]
pub struct ParseArgs {
    /// Expression to parse
    pub expr: Option<String>,
    /// File containing code to parse
    pub file: Option<PathBuf>,
}

pub async fn parse_command(args: ParseArgs, _config: &CliConfig) -> Result<()> {
    let input = match (args.expr, args.file) {
        (Some(expr), None) => PipelineInput::Expression(expr),
        (None, Some(file)) => PipelineInput::File(file),
        (Some(_), Some(_)) => {
            return Err(crate::CliError::Compilation(
                "Cannot specify both --expr and --file".to_string(),
            ));
        }
        (None, None) => {
            return Err(crate::CliError::Compilation(
                "Must specify either --expr or --file".to_string(),
            ));
        }
    };

    let pipeline = Pipeline::new();
    let _config = PipelineConfig {
        optimization_level: 0,
        print_ast: false,
        print_passes: false,
        target: "rust".to_string(),
        runtime: "literal".to_string(),
    };

    // Parse the input and get the AST
    let source = match &input {
        PipelineInput::Expression(expr) => expr.clone(),
        PipelineInput::File(path) => std::fs::read_to_string(&path).map_err(|e| {
            crate::CliError::Io(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Failed to read file {}: {}", path.display(), e),
            ))
        })?,
    };

    let ast = pipeline.parse_source_public(&source)?;

    let mut pretty_opts = PrettyOptions::default();
    pretty_opts.show_types = false;
    pretty_opts.show_spans = false;

    println!("{}", pretty(ast.as_ref(), pretty_opts));

    Ok(())
}
