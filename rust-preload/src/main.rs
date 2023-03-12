#![feature(exit_status_error)]

use clap::Parser;

use common::*;
use common_lang::preloader::Preloader;
use std::fs::{read_to_string, File};
use std::io::{BufRead, BufReader, Cursor};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Path to the project src dir
    #[arg(short, long)]
    path: String,
}

fn main() -> Result<()> {
    setup_logs(LogLevel::Trace)?;
    let args: Args = Args::parse();

    let tmpdir = std::env::temp_dir();

    let output = Command::new("find")
        .arg(&args.path)
        .arg("-name")
        .arg("*.rs")
        .spawn()?
        .wait_with_output()?;
    output.status.exit_ok()?;
    for rel_path in BufReader::new(Cursor::new(output.stdout)).lines() {
        let rel_path = rel_path?;
        let mut src_path = PathBuf::from_str(&args.path)?;
        src_path.push(Path::new(&rel_path));
        let src = read_to_string(src_path)?;
        let mut pre: Preloader = Preloader::new();
        let file = syn::parse_file(&src)?;
        pre.preload_file(&rust_lang::parser::parse_file(file)?)?;
        let mut store_path = tmpdir.clone();
        store_path.push(Path::new(&rel_path));
        let store_file = File::create(store_path)?;
        serde_json::to_writer_pretty(store_file, &pre)?;
    }
    println!("{}", tmpdir.to_string_lossy());

    Ok(())
}
