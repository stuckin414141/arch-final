// Boolean literals and expressions test

// Boolean variable declarations
let t : bool := true;
let f : bool := false;

// Test logical operators
print(t && t);  // true
print(t && f);  // false
print(f && t);  // false
print(f && f);  // false

print(t || t);  // true
print(t || f);  // true
print(f || t);  // true
print(f || f);  // false

// Test comparison operators with booleans
print(t == t);  // true
print(t == f);  // false
print(t != f);  // true

// Test boolean expressions in if statements
if t { 
    print(1)
};

if f then {
    print(0)
};

// Test complex boolean expressions
let complex1 : bool := (t && f) || (t && t);
print(complex1);  // true

let complex2 : bool := (t && f) || (f && f);
print(complex2);  // false

// Test short-circuit evaluation (if supported)
let result : bool := f && (1 / 0 == 0);  // Should not cause division by zero if short-circuiting works
print(result);  // false

// Test while loop with boolean condition
let counter : int := 0;
while counter < 3 {
    print(counter);
    counter := counter + 1
};

// Test boolean function (if supported)
let not : bool -> bool := ftmlk(x : bool) {
    if x then false else true
};

print(not(t));  // false
print(not(f));  // true