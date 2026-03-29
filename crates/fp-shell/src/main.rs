use clap::{Args, Parser, Subcommand};
use fp_shell::{
    CompileOptions, InterpretOptions, compile_file_with_options, interpret_file_with_options,
    load_inventory, parse_target,
};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(
    name = "fp-shell",
    version,
    about = "Compile .fp scripts into shell scripts"
)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    Compile(CompileArgs),
    Interpret(InterpretArgs),
}

#[derive(Args, Debug)]
struct CompileArgs {
    #[arg(required = true)]
    input: PathBuf,

    #[arg(short, long)]
    output: Option<PathBuf>,

    #[arg(long, default_value = "bash")]
    target: String,

    #[arg(long)]
    check: bool,

    #[arg(long)]
    inventory: Option<PathBuf>,
}

#[derive(Args, Debug)]
struct InterpretArgs {
    #[arg(required = true)]
    input: PathBuf,

    #[arg(long, default_value = "bash")]
    target: String,

    #[arg(long)]
    inventory: Option<PathBuf>,
}

fn main() {
    if let Err(err) = run() {
        eprintln!("error: {}", err);
        std::process::exit(1);
    }
}

fn run() -> Result<(), fp_shell::ShellError> {
    let cli = Cli::parse();
    match cli.command {
        Command::Compile(args) => {
            let target = parse_target(&args.target)?;
            let inventory = if let Some(path) = args.inventory.as_deref() {
                Some(load_inventory(path)?)
            } else {
                None
            };
            let options = CompileOptions { inventory };
            let output =
                compile_file_with_options(&args.input, args.output.as_deref(), target, &options)?;
            if args.check {
                println!("ok: {}", output.display());
            } else {
                println!("generated: {}", output.display());
            }
            Ok(())
        }
        Command::Interpret(args) => {
            let target = parse_target(&args.target)?;
            let inventory = if let Some(path) = args.inventory.as_deref() {
                Some(load_inventory(path)?)
            } else {
                None
            };
            let options = InterpretOptions { inventory, target };
            let _ = interpret_file_with_options(&args.input, &options)?;
            Ok(())
        }
    }
}
