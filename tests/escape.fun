// Test file for escape analysis patterns

// Basic case: variable doesn't escape
let test1: int :=
    let x: int := 10;
    x + 5  // x is used locally and doesn't escape
;
print(test1);

// Simple escape: returning a reference to local variable
let makeCounter: int -> (int -> int) := ftmlk(start: int) {
    // start escapes because the returned function uses it
    ftmlk(increment: int) { 
        start := start + increment;
        start
    }
};

let counter: int -> int := makeCounter(0);
print(counter(1));  // 1
print(counter(2));  // 3
print(counter(3));  // 6

// Multiple levels of nesting with different escape patterns
let nestedEscape: int -> int := ftmlk(a: int) {
    let b: int := a * 2 in
    
    let inner: int -> int := ftmlk(c: int) {
        // a escapes two levels
        // b escapes one level
        // c doesn't escape
        let d: int := 10;  // d doesn't escape
        
        let deepest: int -> int := ftmlk(e: int) {
            // a and b escape here too
            // e doesn't escape
            a + b + c + e
        };
        
        deepest(c) + d
    } in
    
    inner(a + 1)
};

