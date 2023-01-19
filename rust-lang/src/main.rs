use barebone::interpreter::{Interpreter, InterpreterContext};
use barebone::{Deserializer, Serializer};
use common::*;
use rust_lang::rustfmt::format_code;
use rust_lang::RustSerde;
use std::fmt::Write;
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
        if file.ends_with(".rs") && !file.contains(".gen") {
            let mut file_in = base.clone();
            file_in.push(&file);

            let mut file_out = base.clone();
            file_out.push(file.replace(".rs", ".gen.rs"));
            info!("{:?} => {:?}", file_in, file_out);
            let file_content = std::fs::read_to_string(file_in)?;
            let node = RustSerde.deserialize(&file_content)?;
            let inp = Interpreter::new(Rc::new(RustSerde));
            let ctx = InterpreterContext::new();
            let intp_result = inp.interprete(&node, &ctx)?;
            // info!("Code: {:?}", code);
            let mut code = RustSerde.serialize(&node)?;
            writeln!(&mut code, "")?;
            for row in ctx.take_outputs() {
                writeln!(&mut code, "// stdout: {}", row)?;
            }
            writeln!(&mut code, "// result: {:?}", intp_result)?;
            // info!("Code: {}", code);
            let code = format_code(&code)?;
            // info!("Code: {}", code);
            std::fs::write(file_out, &code)?;
        }
    }
    Ok(())
}
