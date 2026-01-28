#!/usr/bin/env fp run
//! Async runtime basics: sleep, spawn, join, and select

async fn worker(label: string, delay: f64, value: i64) -> i64 {
    println!("worker {label} start");
    std::future::sleep(delay).await;
    println!("worker {label} done");
    value
}

async fn main() {
    println!("Example: 35_async_await.fp");
    println!("Focus: async fn + await with spawn/join/select/sleep");

    let fast = worker("fast", 0.02, 10);
    let slow = worker("slow", 0.05, 20);
    let winner = std::task::select! {
        a = fast => {
            println!("select fast={a}");
            "fast"
        },
        b = slow => {
            println!("select slow={b}");
            "slow"
        },
    };
    println!("select winner={winner}");

    let left_future = worker("join-left", 0.01, 3);
    let right_future = worker("join-right", 0.02, 4);
    let (left, right) = std::task::join!(left_future, right_future);
    println!("join left={left} right={right}");

    let task = std::task::spawn(worker("spawned", 0.01, 5));
    let spawned = task.await;
    println!("spawned={spawned}");

    let total = worker("direct", 0.0, left + right + spawned).await;
    println!("total={total}");
}
