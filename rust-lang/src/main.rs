use barebone::{Deserializer, Serializer};
use common::*;
use rust_lang::rustfmt::format_code;
use rust_lang::RustSerde;

fn main() -> Result<()> {
    setup_logs(LogLevel::Trace)?;
    let code = RustSerde.deserialize(
        r#"
    fn main() {
        print(1);
    } 
    "#,
    )?;
    info!("Code: {:?}", code);
    let code = RustSerde.serialize(&code)?;
    info!("Code: {}", code);
    let code = format_code(&code)?;
    info!("Code: {}", code);
    Ok(())
}
