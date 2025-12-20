function main(): void {

  const BUFFER_SIZE: number = 4096;

  const MAX_CONNECTIONS: number = 150;

  const FACTORIAL_5: number = 120;

  const IS_LARGE: boolean = true;
  console.log(`Buffer: ${(BUFFER_SIZE / 1024)}KB, factorial(5)=${FACTORIAL_5}, large=${IS_LARGE}`);

interface Config {
  buffer_size: number;
  max_connections: number;
}

  function createConfig(buffer_size: number, max_connections: number): Config {
    return {
      buffer_size: buffer_size,
      max_connections: max_connections
    };
  }

  const CONFIG_SIZE: number = 2;

  const DEFAULT_CONFIG: Config = {buffer_size: 4096, max_connections: 150};
  console.log(`Config: ${(DEFAULT_CONFIG.buffer_size / 1024)}KB buffer, ${DEFAULT_CONFIG.max_connections} connections`);
  let runtime_multiplier = 3;
  let optimized_size = (4096 * 2);
  let cache_strategy = ((4096 > 2048) ? "large" : "small");
  let total_memory = (runtime_multiplier * (BUFFER_SIZE * MAX_CONNECTIONS));
  console.log(`Const blocks: size=${optimized_size}, strategy=${cache_strategy}, memory=${total_memory}`);
}

main();
