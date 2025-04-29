// Closures + functions means that you can somewhat do OOP

type car = {
    inc_year : bool -> bool;
    price : int; year : int; age : int; broken : bool};



let constructor : int -> int -> car =
    ftmlk (price : int, age : int) {
        let ret : car = {
            inc_year = ftmlk(b : bool) {b};
            price = price;
            year = 2025 - age;
            age = age;
            broken = false
        }
        in
        let inc_year : bool -> bool = 
            ftmlk (b : bool) {
                ret.price := ret.price - 1000;
                ret.age := ret.age + 1;
                true
            }
        in
        ret.inc_year := inc_year;
        ret
    };
let honda : car = constructor(10000, 5);
let z : bool = honda.inc_year(true);
print(honda.price)