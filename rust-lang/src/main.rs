use barebone::Deserializer;
use common::*;
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
    Ok(())
}
