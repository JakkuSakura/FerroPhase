use barebone::interpreter::{Interpreter, InterpreterContext};
use barebone::specializer::Specializer;
use barebone::{Deserializer, Serializer};
use common::*;
use rust_lang::rustfmt::format_code;
use rust_lang::RustSerde;
use std::fs::File;
use std::io::{Seek, SeekFrom, Write};
use std::rc::Rc;

fn main() -> Result<()> {
    setup_logs(LogLevel::Trace)?;

    let mut base = std::path::Path::new(file!()).to_path_buf();
    base.pop();
    base.push("../examples");
    let dir = std::fs::read_dir(&base)?;
    for file in dir {
        let file = file?;
        let file = file.file_name().as_os_str().to_string_lossy().to_string();
        if !file.ends_with(".rs") || file.contains("_gen.rs") {
            continue;
        }
        let mut file_in = base.clone();
        file_in.push(&file);

        let mut file_out = base.clone();
        file_out.push(file.replace(".rs", "_gen.rs"));
        info!("{:?} => {:?}", file_in, file_out);
        let mut file_out = File::create(file_out)?;
        let file_content = std::fs::read_to_string(file_in)?;
        let node = RustSerde.deserialize(&file_content)?;
        let ctx = InterpreterContext::new();
        let node = Specializer::new(Rc::new(RustSerde)).specialize_expr(node, &ctx)?;
        let code = RustSerde.serialize(&node)?;
        writeln!(&mut file_out, "{}", code)?;
        let code = format_code(&code)?;
        file_out.set_len(0)?;
        file_out.seek(SeekFrom::Start(0))?;
        writeln!(&mut file_out, "{}", code)?;

        let inp = Interpreter::new(Rc::new(RustSerde));
        let ctx = InterpreterContext::new();
        let intp_result = inp.interprete(&node, &ctx)?;
        for row in ctx.take_outputs() {
            writeln!(&mut file_out, "// stdout: {}", row)?;
        }
        writeln!(
            &mut file_out,
            "// result: {}",
            RustSerde.serialize(&intp_result)?
        )?;
    }
    Ok(())
}
