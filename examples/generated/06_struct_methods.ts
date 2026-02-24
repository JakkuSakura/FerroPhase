interface Point {
  x: number;
  y: number;
}

function createPoint(x: number, y: number): Point {
  return {
    x: x,
    y: y
  };
}

const POINT_SIZE: number = 2;

interface Rectangle {
  width: number;
  height: number;
}

function createRectangle(width: number, height: number): Rectangle {
  return {
    width: width,
    height: height
  };
}

const RECTANGLE_SIZE: number = 2;

function main(): void {
  console.log("=== Struct Operations ===");
  let p1 = Point.new(10, 20);
  let p2 = Point.new(5, 15);
  console.log(`p1 = (${p1.x}, ${p1.y})`);
  console.log(`p2 = (${p2.x}, ${p2.y})`);
  p1.translate(3, (-4));
  console.log(`p1 after translate = (${p1.x}, ${p1.y})`);
  console.log(`Distance²(p1, p2) = ${p1.distance2(p2)}`);
  let rect = Rectangle.new(10, 5);
  console.log(`Rectangle: ${rect.width}×${rect.height}`);
  console.log(`  area = ${rect.area()}`);
  console.log(`  perimeter = ${rect.perimeter()}`);
  console.log(`  is_square = ${rect.is_square()}`);
}

main();
