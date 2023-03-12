use common::Result;
use rust_preload::{collect_rs_files, PRELOAD_DIR};
use std::env;
use std::fs::metadata;
use std::path::{Path, PathBuf};

fn main() -> Result<()> {
    let dir = env::var(PRELOAD_DIR)?;
    println!("{}", dir);
    let rs_files = collect_rs_files(&Path::new(&dir))?;
    for rs_file in rs_files {
        let mut buf = PathBuf::from(&dir);
        buf.push(rs_file);
        let meta = metadata(buf.as_path())?;
        println!("{} len {}", buf.display(), meta.len());
    }
    Ok(())
}
