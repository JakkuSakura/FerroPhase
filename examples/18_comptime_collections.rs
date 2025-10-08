{
    use std :: collections :: HashMap ;
    const PRIME_COUNT : i64 = 6 ;
    const ZERO_BUFFER_CAPACITY : i64 = 16 ;
    const HTTP_STATUS_COUNT : i64 = 4 ;
    {
        println ! ("=== Compile-time Collections ===") ;
        println ! ("Vec literals:") ;
        println ! ("  primes: {} elements" , PRIME_COUNT) ;
        println ! ("  zero buffer: {} elements" , ZERO_BUFFER_CAPACITY) ;
        println ! ("\nHashMap literal via HashMap::from:") ;
        println ! ("  tracked HTTP statuses: {} entries" , HTTP_STATUS_COUNT) ;
    }
}
