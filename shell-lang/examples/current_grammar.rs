include!("defs/mod.rs");
use shell_lang::starter::TryStarter;
use shell_lang::*;

fn main() {
    let src = SourceProcess::spawn("src", 1);
    let adder = AddProcess::spawn("adder");
    let dest = SinkProcess::spawn("dest");
    let add_five = adder.add(5);
    pipe!(src | add_five | dest).start();
    pipe!(src | adder | dest).start();
}
