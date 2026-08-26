function main(): void {

  const FIELD_COUNT: number = 3;

  const TYPE_NAME: string = "Point3D";

interface Point3D {
  x: number;
  y: number;
  z: number;
}

  function createPoint3D(x: number, y: number, z: number): Point3D {
    return {
      x: x,
      y: y,
      z: z
    };
  }

  const POINT3_D_SIZE: number = 3;

  console.log(`${Point3D.type_name()} has ${Point3D.field_count()} fields`);

  const VARIANT_A: number = 1;

  const VARIANT_B: number = 2;

  enum Tag {
    A,
    B,
  }
  let tag = Tag.A;
  console.log(`tag discriminant: ${tag}`);
}

main();
