interface Config {
    buffer_size: number;
    max_connections: number;
    timeout_ms: number;
    debug_level: number;
}

function main(): void {
  const COLOR_SIZE: number = 24;
  const POINT_SIZE: number = 16;
  const TOTAL_SIZE: number = 40;
  
  // Example struct instantiation
  const config_instance: Config = {
    buffer_size: 0,
    max_connections: 0,
    timeout_ms: 0,
    debug_level: 0,
  };
  
  // Generated output
  console.log('Transpilation Example');
  console.log(`COLOR_SIZE: ${COLOR_SIZE}`);
  console.log(`POINT_SIZE: ${POINT_SIZE}`);
  console.log(`TOTAL_SIZE: ${TOTAL_SIZE}`);
}

// Run main function
main();
