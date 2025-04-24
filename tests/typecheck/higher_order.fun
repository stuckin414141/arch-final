// Create a curried function
let add: int -> int -> int = ftmlk(x: int, y: int) { x + y };
let add5: int -> int = add(5);
print(add5(10));  // 15

// HOF that transforms a function
let twice: (int -> int) -> int -> int = ftmlk(f: int -> int, x: int) { f(f(x)) };
let inc: int -> int = ftmlk(x: int) { x + 1 };
print(twice(inc, 7));  // 9

// Function that returns a function
let makeAdder: int -> int -> int = ftmlk(x: int) { 
  ftmlk(y: int) { x + y } 
};

let add3: int -> int = makeAdder(3);
print(add3(10));  // 13

// Complex composition
let composeMany: (int -> int) -> int -> (int -> int) = 
  ftmlk(f: int -> int, n: int) {
    let result: int -> int := ftmlk(x: int) { x } in
    let i: int := 0 in
      while i < n {
        result := ftmlk(x: int) { f(result(x)) };
        i := i + 1
      };
      result
  };

let double: int -> int = ftmlk(x: int) { x * 2 };
let double3Times: int -> int = composeMany(double, 3);
print(double3Times(2));  // 16

//this should error
let wrong_bool : int -> int -> bool = 
    ftmlk(x: int, y: int) { x - y }