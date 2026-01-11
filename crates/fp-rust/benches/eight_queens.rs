use criterion::{Criterion, criterion_group, criterion_main};

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
        if cols[col as usize] == 0 && diag1[d1 as usize] == 0 && diag2[d2 as usize] == 0 {
            cols[col as usize] = 1;
            diag1[d1 as usize] = 1;
            diag2[d2 as usize] = 1;
            positions[row as usize] = col;
            count += solve(row + 1, cols, diag1, diag2, positions);
            cols[col as usize] = 0;
            diag1[d1 as usize] = 0;
            diag2[d2 as usize] = 0;
            positions[row as usize] = -1;
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

fn bench_eight_queens(c: &mut Criterion) {
    c.bench_function("eight_queens", |b| {
        b.iter(|| {
            let total = run_solver();
            assert_eq!(total, 92);
        })
    });
}

criterion_group!(benches, bench_eight_queens);
criterion_main!(benches);
