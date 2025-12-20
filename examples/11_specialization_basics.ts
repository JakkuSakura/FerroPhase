function add(a: number, b: number): number {
  return (a + b);
}

function double(x: number): number {
  return (x * 2);
}

function compose(x: number): number {
  return double(add(x, 1));
}

function main(): void {
  console.log(`${add(2, 3)}`);
  console.log(`${double(5)}`);
  console.log(`${compose(10)}`);

  const RESULT: number = 30;
  console.log(`const: ${RESULT}`);
}

main();
