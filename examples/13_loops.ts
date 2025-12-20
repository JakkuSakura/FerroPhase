function factorial(n: number): number {
  let result = 1;
  let i = 1;
  while ((i <= n)) {
    result = (result * i);
    i = (i + 1);
  }
  return result;
}

function sum_range(start: number, end: number): number {
  let sum = 0;
  for (let i = start; i < end; i++) {
    sum = (sum + i);
  }
  return sum;
}

function find_first_divisor(n: number): number {
  let i = 2;
  while (true) {
    if (((i * i) > n)) {
      break;
    }
    if (((n % i) == 0)) {
      break;
    }
    i = (i + 1);
  }
}

function sum_even_numbers(limit: number): number {
  let sum = 0;
  let i = 0;
  while ((i < limit)) {
    i = (i + 1);
    if (((i % 2) != 0)) {
      continue;
    }
    sum = (sum + i);
  }
  return sum;
}

function main(): void {
  console.log("=== Loop Constructs ===\n");
  console.log("1. While loop - factorial:");
  console.log(`  5! = ${factorial(5)}`);
  console.log(`  7! = ${factorial(7)}`);
  console.log("\n2. For loop - sum range:");
  console.log(`  sum(1..10) = ${sum_range(1, 10)}`);
  console.log(`  sum(5..15) = ${sum_range(5, 15)}`);
  console.log("\n3. Loop with break expression:");
  console.log(`  First divisor of 24: ${find_first_divisor(24)}`);
  console.log(`  First divisor of 17: ${find_first_divisor(17)}`);
  console.log("\n4. Loop with continue:");
  console.log(`  Sum of even numbers < 10: ${sum_even_numbers(10)}`);
  console.log("\n5. Nested loops:");
  let count = 0;
  for (let i = 1; i < 4; i++) {
    for (let j = 1; j < 4; j++) {
      count = (count + 1);
      if ((i == j)) {
        console.log(`[${i}] `);
      }
    }
  }
  console.log(`
  Iterations: ${count}`);
  console.log("\n6. Compile-time iteration (simulated):");

  const FACTORIAL_CONST: number = 120;
  console.log(`  const 5! = ${FACTORIAL_CONST}`);
  console.log("\n✓ Loop constructs demonstrated!");
}

main();
