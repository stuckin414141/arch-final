// Test complex nested expressions
let result: int = 5 + (if 3 > 2 then 10 else 20) * 2;
print(result);

// Test nested function applications
let inc: int -> int = ftmlk(x: int) { x + 1 };
let double: int -> int = ftmlk(x: int) { x * 2 };
print(inc(double(inc(5))));

// Test complex statement sequence
let x: int = 1;
let y: int = 2;
x := x + 1;
y := y * 2;
print(x + y);
print(x * y)