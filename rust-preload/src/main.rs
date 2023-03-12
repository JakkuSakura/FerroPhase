#![feature(exit_status_error)]

use clap::Parser;
use std::mem::forget;
use std::path::Path;

use common::*;
use rust_preload::write_preload_to_dir;
use tempdir::TempDir;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Path to the project src dir
    #[arg(short, long)]
    path: String,
}

fn main() -> Result<()> {
    // setup_logs(LogLevel::Trace)?;
    let args: Args = Args::parse();

    let tmpdir = TempDir::new("rust-reload")?;
    write_preload_to_dir(&Path::new(&args.path), tmpdir.path())?;

    println!("{}", tmpdir.path().display());

    // forget tmpdir not to remove generated files
    forget(tmpdir);
    Ok(())
}
