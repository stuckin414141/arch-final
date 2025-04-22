// Test function definitions
let add: int -> int := ftmlk(x: int) { x + 1 };
print(add(5));

// Test higher-order functions
let apply: (int -> int) -> int := ftmlk(f: int -> int) { f(10) };
print(apply(add));

// Test multi-parameter functions
let add_mul: int -> int -> int := ftmlk(x: int, y: int) { x + y * 2 };
print(add_mul(5, 3));