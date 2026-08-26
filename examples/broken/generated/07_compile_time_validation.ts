function main(): void {

interface Data {
  a: number;
  b: number;
  c: any;
}

  function createData(a: number, b: number, c: any): Data {
    return {
      a: a,
      b: b,
      c: c
    };
  }

  const DATA_SIZE: number = 3;

  const SIZE: number = 24;

  const FIELDS: number = 3;

  const HAS_A: boolean = true;

  const HAS_X: boolean = false;
  console.log(`sizeof=${SIZE}, fields=${FIELDS}`);
  console.log(`has_a=${HAS_A}, has_x=${HAS_X}`);

  const MAX_SIZE: number = 64;

  const SIZE_OK: boolean = true;

  const IS_ALIGNED: boolean = true;
  console.log(`size_ok=${SIZE_OK}, aligned=${IS_ALIGNED}`);

  const MODE: string = "optimized";
  console.log(`mode: ${MODE}`);
}

main();
