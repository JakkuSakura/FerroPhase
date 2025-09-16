use common::*;

use itertools::Itertools;
use fp_core::ast::{AstDeserializer, AstSerializer};
use fp_core::context::SharedScopedContext;
use fp_optimize::orchestrators::InterpretationOrchestrator;
use fp_optimize::utils::load_optimizers;
use fp_rust_lang::parser::RustParser;
use fp_rust_lang::printer::rustfmt::format_code;
use fp_rust_lang::printer::RustPrinter;
use std::fs::File;
use std::io::{Seek, SeekFrom, Write};
use std::sync::Arc;

fn main() -> Result<()> {
    setup_logs(LogLevel::Trace)?;

    let base = std::path::Path::new("examples");
    let mut dirs: Vec<_> = std::fs::read_dir(&base)?
        .into_iter()
        .map(|x| Ok::<_, Error>(x?.path()))
        .try_collect()?;
    dirs.sort();
    let rust_printer = Arc::new(RustPrinter::new());
    let rust_parser = RustParser::new();
    for file_in in dirs {
        let file_str = file_in.file_name().unwrap().to_string_lossy().to_string();
        if !file_str.contains("main_") || file_str.contains("_gen.rs") {
            continue;
        }

        let file_out = file_in.with_file_name(file_str.replace(".rs", "_gen.rs"));

        info!("{} => {}", file_in.display(), file_out.display());
        let mut file_out = File::create(file_out)?;
        let file_content = std::fs::read_to_string(file_in)?;
        let mut node = rust_parser.deserialize_node(&file_content)?;
        let ctx = SharedScopedContext::new();
        let optimizers = load_optimizers(rust_printer.clone() as _);
        for optimizer in optimizers {
            node = optimizer.optimize_tree(node, &ctx)?;
        }
        let code = rust_printer.serialize_node(&node)?;
        writeln!(&mut file_out, "{}", code)?;
        let code = format_code(&code)?;
        file_out.set_len(0)?;
        file_out.seek(SeekFrom::Start(0))?;
        writeln!(&mut file_out, "{}", code)?;

        let inp = Interpreter::new(rust_printer.clone() as _);
        let ctx = SharedScopedContext::new();
        let intp_result = inp.interpret_tree(node, &ctx)?;
        for row in ctx.take_outputs() {
            writeln!(&mut file_out, "// stdout: {}", row)?;
        }
        writeln!(
            &mut file_out,
            "// result: {}",
            rust_printer.print_value(&intp_result)?
        )?;
    }
    Ok(())
}
