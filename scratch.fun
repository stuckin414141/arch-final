type Point = {x: int; y: int};

let make_adder: (Point)->(int)->(int) = ftmlk(p: Point) {
  // p should escape since its fields are accessed in the inner function
  ftmlk(z: int) {
    p.x + p.y + z
  }
};

let point: Point = {x = 5; y = 10};
let adder: (int)->(int) = make_adder(point);
print(adder(15))  // Should output 30 (5 + 10 + 15)