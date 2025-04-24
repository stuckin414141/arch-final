// Test if-then
if 5 > 3 { 
    print(1)
};

// Test if-then-else
let z : int = 
    if 5 > 10 then 
        10
    else 
        20;

// Test nested if expressions
let x: int = 
    if 5 > 3 then 
        if 2 < 1 then 
            100 
        else 
            200
    else 
        300;
print(x);

// Test while loops
let count: int = 0;
while count < 5 {
  print(count);
  count := count + 1
};

// Test break
let i: int = 0;
while i < 10 {
  if i > 5 { break };
  print(i);
  i := i + 1
}