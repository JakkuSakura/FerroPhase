include!("defs/mod.rs");
use shell_lang::starter::TryStarter;
use shell_lang::*;

fn main() {
    let src = &SourceProcess::spawn("src", 1);
    let adder = AddProcess::spawn("adder");
    let dest = &SinkProcess::spawn("dest");

    // declarative macro can't create new idents
    pipe!(src | (adder.add(5) | dest, adder.add(6) | dest)).start();
}
