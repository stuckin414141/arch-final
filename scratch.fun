let add_three_numbers : int -> int -> int -> int = 
    ftmlk (x : int) {
        ftmlk (z : int) {
            ftmlk (y : int) {
                x + y + z
            }
        }
    };
let adc : int -> int -> int = add_three_numbers (1);
print(adc (8) (9));
let add_leet_sixty : int -> int = add_three_numbers (1337) (60);
print(add_leet_sixty(100));