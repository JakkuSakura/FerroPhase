use barebone::{Deserializer, Serializer};
use common::*;
use rust_lang::rustfmt::format_code;
use rust_lang::RustSerde;

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
            let code = RustSerde.deserialize(&file_content)?;
            // info!("Code: {:?}", code);
            let code = RustSerde.serialize(&code)?;
            // info!("Code: {}", code);
            let code = format_code(&code)?;
            // info!("Code: {}", code);
            std::fs::write(file_out, &code)?;
        }
    }
    Ok(())
}
