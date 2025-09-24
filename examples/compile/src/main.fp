const API_NAME: str = "FerroPhase";

const HELPER_DEF = quote {
    fn helper(x: i32) -> i32 {
        x * 2
    }
};

const REGISTER = splice(HELPER_DEF);

fn main() {
    println!("Welcome to {}", API_NAME);
    println!("helper(21) = {}", helper(21));
}

