// Minimal semantic fixture: control flow (if/else, while, for, loop).
// Expected output:
// if: small
// while_sum: 6
// for_prod: 24
// loop_break: 5

fn main() {
    let value = 3;
    if value < 5 {
        println!("if: small");
    } else {
        println!("if: large");
    }

    let mut i = 0;
    let mut sum = 0;
    while i < 4 {
        sum += i;
        i += 1;
    }
    println!("while_sum: {}", sum);

    let mut prod = 1;
    for n in 1..5 {
        prod *= n;
    }
    println!("for_prod: {}", prod);

    let mut counter = 0;
    let result = loop {
        counter += 1;
        if counter == 5 {
            break counter;
        }
    };
    println!("loop_break: {}", result);
}
