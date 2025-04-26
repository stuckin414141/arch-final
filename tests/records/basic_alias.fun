type Point = {x: int; y: int};
let p1: Point = {x = 5; y = 10};
let p2: Point = {x = 3; y = 7};
print(p1.x + p2.y);

type Person = {age: int; active: bool};
let employee: Person = {
  age = 30;
  active = true
};
print(employee.age)