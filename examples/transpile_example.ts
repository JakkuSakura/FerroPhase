interface Point {
    x: number;
    y: number;
}
interface Color {
    r: number;
    g: number;
    b: number;
}

function main(): void {
  const COLOR_SIZE: number = 24;
  const POINT_SIZE: number = 16;
  const TOTAL_SIZE: number = 40;
  
  // Example struct instantiation
  const point_instance: Point = {
    x: 0,
    y: 0,
  };
  const color_instance: Color = {
    r: 0,
    g: 0,
    b: 0,
  };
  
  // Generated output
  console.log('Transpilation Example');
  console.log(`COLOR_SIZE: ${COLOR_SIZE}`);
  console.log(`POINT_SIZE: ${POINT_SIZE}`);
  console.log(`TOTAL_SIZE: ${TOTAL_SIZE}`);
}

// Run main function
main();
