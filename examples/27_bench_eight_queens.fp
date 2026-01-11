#!/usr/bin/env fp run
//! Benchmark-style run for the 8-queens solver.

fn solve(
    row: i64,
    cols: &mut [i64; 8],
    diag1: &mut [i64; 15],
    diag2: &mut [i64; 15],
    positions: &mut [i64; 8],
) -> i64 {
    if row == 8 {
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
            count += solve(row + 1, cols, diag1, diag2, positions);
            cols[col] = 0;
            diag1[d1] = 0;
            diag2[d2] = 0;
            positions[row] = -1;
        }
    }
    count
}

fn run_solver() -> i64 {
    let mut cols: [i64; 8] = [0; 8];
    let mut diag1: [i64; 15] = [0; 15];
    let mut diag2: [i64; 15] = [0; 15];
    let mut positions: [i64; 8] = [-1, -1, -1, -1, -1, -1, -1, -1];
    solve(0, &mut cols, &mut diag1, &mut diag2, &mut positions)
}

#[bench]
fn bench_eight_queens() {
    let mut idx = 0;
    while idx < 5 {
        let total = run_solver();
        assert_eq!(total, 92);
        idx = idx + 1;
    }
}

fn main() {
    println!("Running 8-queens benchmark demo");
    let report = std::bench::run_benches();
    println!(
        "Summary: {} passed, {} failed, {} total",
        report.passed,
        report.failed,
        report.total
    );
}
