const PRIME_COUNT = 6;

const ZERO_BUFFER_CAPACITY = 16;

const HTTP_STATUS_COUNT = 4;
function main() {
  console.log("=== Compile-time Collections ===");
  console.log("Vec literals:");
  console.log(`  primes: ${PRIME_COUNT} elements`);
  console.log(`  zero buffer: ${ZERO_BUFFER_CAPACITY} elements`);
  console.log("\nHashMap literal via HashMap::from:");
  console.log(`  tracked HTTP statuses: ${HTTP_STATUS_COUNT} entries`);
}

main();
