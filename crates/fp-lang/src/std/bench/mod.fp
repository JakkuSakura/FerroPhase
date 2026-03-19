struct BenchCase {
    name: str,
    run: fn(),
}

const mut REGISTRY: Vec<BenchCase> = Vec::new();

const fn bench(item: quote<item>) -> quote<item> {
    let name = item.name;
    REGISTRY.push(BenchCase { name, run: item.value });
    item
}

struct BenchReport {
    total: i64,
    passed: i64,
    failed: i64,
}

fn run_benches() -> BenchReport {
    let benches: Vec<BenchCase> = REGISTRY;
    let mut passed = 0;
    let mut failed = 0;
    let mut idx = 0;
    while idx < benches.len() {
        let bench: BenchCase = benches[idx];
        let mut ok = true;
        let warmup_secs = 5.0f64;
        let measure_secs = 15.0f64;

        let warmup_start = std::time::now();
        let warmup_deadline = warmup_start + warmup_secs;
        let mut warmup_iters = 0;
        while std::time::now() < warmup_deadline {
            let warm_ok = catch_unwind(bench.run);
            if !warm_ok {
                ok = false;
                break;
            }
            warmup_iters = warmup_iters + 1;
        }

        let measure_start = std::time::now();
        let measure_deadline = measure_start + measure_secs;
        let mut measure_iters = 0;
        if ok {
            while std::time::now() < measure_deadline || measure_iters == 0 {
                let run_ok = catch_unwind(bench.run);
                if !run_ok {
                    ok = false;
                    break;
                }
                measure_iters = measure_iters + 1;
            }
        }
        let measure_end = std::time::now();
        let elapsed = measure_end - measure_start;
        if ok {
            passed = passed + 1;
            let iters_f = measure_iters as f64;
            let ns_per_iter = if iters_f > 0.0 {
                (elapsed / iters_f) * 1000000000.0
            } else {
                0.0
            };
            println(
                "  {} ... ok (iters: {}, time: {:.6}s, ns/iter: {:.2})",
                bench.name,
                measure_iters,
                elapsed,
                ns_per_iter
            );
        } else {
            failed = failed + 1;
            println("  {} ... FAILED", bench.name);
        }
        idx = idx + 1;
    }
    let total = passed + failed;
    println(
        "bench result: {} passed; {} failed; {} total",
        passed,
        failed,
        total
    );
    BenchReport {
        total,
        passed,
        failed,
    }
}
