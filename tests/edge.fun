// Test empty function body
let identity: int -> int = ftmlk(x: int) { x };

// Test function with multiple parameters
let complex: int -> int -> int = ftmlk(x: int, y: int) {
  let z: int = x + y in
    z * 2
};
print(complex(3, 4));

// Test deeply nested expressions
print(1 + 2 + (3 * 4) + (if 5 > 6 then 7 else 8 + 9) * 10);