from dataclasses import dataclass

@dataclass
class Point:
    x: int
    y: int

@dataclass
class Rectangle:
    width: int
    height: int

def main() -> None:
    print(f"=== Struct Operations ===")
    p1 = Point.new(10, 20)
    p2 = Point.new(5, 15)
    print(f"p1 = ({p1.x}, {p1.y})")
    print(f"p2 = ({p2.x}, {p2.y})")
    p1.translate(3, -(4))
    print(f"p1 after translate = ({p1.x}, {p1.y})")
    print(f"Distance²(p1, p2) = {p1.distance2(p2)}")
    rect = Rectangle.new(10, 5)
    print(f"Rectangle: {rect.width}×{rect.height}")
    print(f"  area = {rect.area()}")
    print(f"  perimeter = {rect.perimeter()}")
    print(f"  is_square = {rect.is_square()}")

if __name__ == "__main__":
    main()
