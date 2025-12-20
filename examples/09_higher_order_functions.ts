function apply_if(cond: boolean, a: number, b: number, op: any): number {
  if (cond) {
    return op(a, b);
  }
  else {
    return 0;
  }
}

function make_adder(n: number): any {
  return (x) => (x + n);
}

function main(): void {
  console.log("Generic operations:");
  apply(10, 20, add);
  apply(1.5, 2.5, add);
  console.log("\nConditional:");
  console.log(`${apply_if(true, 5, 3, add__spec0)}`);
  console.log(`${apply_if(false, 5, 3, add__spec0)}`);
  console.log("\nClosure factory:");

  const add_10: any = make_adder(10);
  console.log(`add_10(5) = ${add_10(5)}`);
  let double = (x) => (x * 2);
  console.log(`double(7) = ${double(7)}`);
}

function add__spec0(a: number, b: number): number {
  return (a + b);
}

main();
