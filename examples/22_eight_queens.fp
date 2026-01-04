#!/usr/bin/env fp run
//! Classic 8-queens solver using recursive backtracking.

fn solve(
    row: i64,
    cols: &mut [i64; 8],
    diag1: &mut [i64; 15],
    diag2: &mut [i64; 15],
    positions: &mut [i64; 8],
    first_solution: &mut [i64; 8],
    found_first: &mut bool,
) -> i64 {
    if row == 8 {
        if !*found_first {
            for r in 0..8 {
                first_solution[r] = positions[r];
            }
            *found_first = true;
        }
        return 1;
    }

    let mut count = 0;
    for col in 0..8 {
        let d1 = row + col;
        let d2 = row - col + 7;
        if cols[col] == 0 && diag1[d1] == 0 && diag2[d2] == 0 {
            cols[col] = 1;
            diag1[d1] = 1;
            diag2[d2] = 1;
            positions[row] = col;
            count += solve(
                row + 1,
                cols,
                diag1,
                diag2,
                positions,
                first_solution,
                found_first,
            );
            cols[col] = 0;
            diag1[d1] = 0;
            diag2[d2] = 0;
            positions[row] = -1;
        }
    }
    count
}

fn print_board(positions: &[i64; 8]) {
    println!("First solution:");
    for r in 0..8 {
        for c in 0..8 {
            if positions[r] == c {
                print!("Q ");
            } else {
                print!(". ");
            }
        }
        println!("");
    }
}

fn main() {
    println!("📘 Tutorial: 22_eight_queens.fp");
    println!("🧭 Focus: Classic 8-queens solver using recursive backtracking.");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");
    let mut cols: [i64; 8] = [0; 8];
    let mut diag1: [i64; 15] = [0; 15];
    let mut diag2: [i64; 15] = [0; 15];
    let mut positions: [i64; 8] = [-1, -1, -1, -1, -1, -1, -1, -1];
    let mut first_solution: [i64; 8] = [-1, -1, -1, -1, -1, -1, -1, -1];
    let mut found_first = false;

    let total = solve(
        0,
        &mut cols,
        &mut diag1,
        &mut diag2,
        &mut positions,
        &mut first_solution,
        &mut found_first,
    );

    print_board(&first_solution);
    println!("Total solutions: {}", total);
}
