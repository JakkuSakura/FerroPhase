use common::*;

use common_lang::context::ExecutionContext;
use common_lang::interpreter::Interpreter;
use common_lang::specializer::Specializer;
use common_lang::{Deserializer, Serializer};
use rust_lang::printer::RustPrinter;
use rust_lang::rustfmt::format_code;
use rust_lang::RustSerde;
use std::fs::File;
use std::io::{Seek, SeekFrom, Write};
use std::rc::Rc;

fn main() -> Result<()> {
    setup_logs(LogLevel::Trace)?;

    let base = std::path::Path::new("examples");
    let mut dirs: Vec<_> = std::fs::read_dir(&base)?
        .into_iter()
        .map(|x| Ok::<_, Error>(x?.path()))
        .try_collect()?;
    dirs.sort();

    for file_in in dirs {
        let file_str = file_in.file_name().unwrap().to_string_lossy().to_string();
        if !file_str.contains("main_") || file_str.contains("_gen.rs") {
            continue;
        }

        let file_out = file_in.with_file_name(file_str.replace(".rs", "_gen.rs"));

        info!("{} => {}", file_in.display(), file_out.display());
        let mut file_out = File::create(file_out)?;
        let file_content = std::fs::read_to_string(file_in)?;
        let node = RustSerde.deserialize(&file_content)?;
        let ctx = ExecutionContext::new();
        let node = Specializer::new(Rc::new(RustSerde))
            .specialize_tree(&node, &ctx)?
            .context("Failed to specialize tree")?;
        let code = RustSerde.serialize_tree(&node)?;
        writeln!(&mut file_out, "{}", code)?;
        let code = format_code(&code)?;
        file_out.set_len(0)?;
        file_out.seek(SeekFrom::Start(0))?;
        writeln!(&mut file_out, "{}", code)?;

        let inp = Interpreter::new(Rc::new(RustSerde));
        let ctx = ExecutionContext::new();
        let intp_result = inp.interpret_tree(&node, &ctx)?;
        for row in ctx.take_outputs() {
            writeln!(&mut file_out, "// stdout: {}", row)?;
        }
        writeln!(
            &mut file_out,
            "// result: {}",
            RustPrinter.print_value(&intp_result)?
        )?;
    }
    Ok(())
}
