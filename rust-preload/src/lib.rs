#![feature(exit_status_error)]

use common::{Result, *};
use common_lang::preloader::Preloader;
use std::fs::{read_to_string, File};
use std::io::{BufRead, BufReader, Cursor};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

pub const PRELOAD_DIR: &'static str = "PRELOAD_DIR";
pub fn collect_rs_files(path: &Path) -> Result<Vec<PathBuf>> {
    eprintln!("Collecting files in {}", path.display());
    let output = Command::new("find")
        .arg(path)
        .arg("-name")
        .arg("*.rs")
        .stdout(Stdio::piped())
        .spawn()?
        .wait_with_output()?;
    output.status.exit_ok()?;
    eprintln!("Collected files in {}", path.display());
    BufReader::new(Cursor::new(output.stdout))
        .lines()
        .map(|x| {
            let buf = PathBuf::from(&x?);
            let buf = buf.strip_prefix(path)?.to_owned();
            Ok(buf)
        })
        .try_collect()
}
pub fn write_preload_to_dir(path: &Path, tmpdir: &Path) -> Result<()> {
    let rs_files = collect_rs_files(path)?;
    eprintln!("files: {:?}", rs_files);
    for rel_path in rs_files {
        let src_path = path.join(&rel_path);
        eprintln!("file: {}", src_path.display());
        let src = read_to_string(src_path)?;
        let mut pre: Preloader = Preloader::new();
        eprintln!("syn parse");
        let file = syn::parse_file(&src)?;
        eprintln!("rust parse");
        pre.preload_file(&rust_lang::parser::parse_file(file)?)?;
        let store_path = tmpdir.join(&rel_path);

        let store_file = File::create(store_path)?;
        serde_json::to_writer_pretty(store_file, &pre)?;
    }
    eprintln!("files done");
    Ok(())
}
