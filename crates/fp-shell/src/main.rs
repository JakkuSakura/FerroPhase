use clap::Parser;
use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::CompileArgs;
use fp_shell::{parse_target, ScriptTarget, ShellBackend};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(name = "fp-shell", version, about = "Compile .fp scripts into shell scripts")]
struct Cli {
    input: PathBuf,
    #[arg(short, long)] output: Option<PathBuf>,
    #[arg(long, default_value = "bash")] target: String,
    #[arg(long)] check: bool,
    #[arg(long)] inventory: Option<PathBuf>,
    #[arg(long)] dry_run: bool,
    #[arg(long)] exec: bool,
}

fn main() {
    if let Err(error) = run() { eprintln!("error: {error}"); std::process::exit(1); }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    let target = parse_target(&cli.target)?;
    for (name, target_kind) in [("bash", ScriptTarget::Bash), ("powershell", ScriptTarget::PowerShell), ("pwsh", ScriptTarget::PowerShell), ("ps", ScriptTarget::PowerShell)] {
        let dry_run = cli.dry_run;
        fp_cli::register_target_backend(name, move |config| {
            Ok(Box::new(ShellBackend::new(target_kind, config, None, dry_run)))
        });
    }
    let args = CompileArgs {
        input: cli.input.clone(), package: None, target: target_name(target).into(),
        target_triple: None, target_cpu: None, native_target: None, target_features: None,
        target_sysroot: None, linker: "clang".into(), target_linker: None, output: cli.output.clone(),
        opt_level: 2, debug: false, release: false, include: Vec::new(), define: Vec::new(),
        exec: cli.exec, link: false, save_intermediates: false, source_language: Some("fp".into()),
        type_defs: false, single_world: false,
    };
    let runtime = tokio::runtime::Runtime::new()?;
    runtime.block_on(fp_cli::commands::compile_command(args, &CliConfig::default()))?;
    if cli.check { println!("ok"); }
    Ok(())
}

fn target_name(target: ScriptTarget) -> &'static str {
    match target { ScriptTarget::Bash => "bash", ScriptTarget::PowerShell => "powershell" }
}
