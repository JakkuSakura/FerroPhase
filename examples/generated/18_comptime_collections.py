PRIME_COUNT: int = 6

ZERO_BUFFER_CAPACITY: int = 16

HTTP_STATUS_COUNT: int = 4

def main() -> None:
    print(f"=== Compile-time Collections ===")
    print(f"Vec literals:")
    print(f"  primes: {PRIME_COUNT} elements")
    print(f"  zero buffer: {ZERO_BUFFER_CAPACITY} elements")
    print(f"\nHashMap literal via HashMap::from:")
    print(f"  tracked HTTP statuses: {HTTP_STATUS_COUNT} entries")

if __name__ == "__main__":
    main()
