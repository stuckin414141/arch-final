type Address = {American : bool; zipcode: int};
type Contact = {number: int; address: Address};

let contact: Contact = {
  number = 4444444;
  address = {American = true; zipcode = 12345}
};

print(contact.address.zipcode);

type Vector = {x: int; y: int};

let v1: Vector = {x = 3; y = 4};
let v2: Vector = {x = 2; y = 6};

let addVectors: (Vector)->(Vector)->(Vector) = 
  ftmlk(v1: Vector, v2: Vector) {
    {x = v1.x + v2.x; y = v1.y + v2.y}
  };

let result: Vector = addVectors(v1, v2);
print(result.x);
print(result.y);

type Score = {value: int; passing: bool};

let checkScore: (int)->(Score) = ftmlk(points: int) {
  if points >= 60 then
    {value = points; passing = true}
  else
    {value = points; passing = false}
};

let student1: Score = checkScore(75);
let student2: Score = checkScore(45);

print(student1.passing);
print(student2.passing);

type Calculator = {
  add: int->int->int;
  multiply: int->int->int
};

let makeCalculator: bool->(Calculator) = ftmlk(j : bool) {
  {
    add = ftmlk(a: int, b: int) { a + b };
    multiply = ftmlk(a: int, b: int) { a * b }
  }
};

let calc: Calculator = makeCalculator(true);
print(calc.add(5, 3));
print(calc.multiply(5, 3))