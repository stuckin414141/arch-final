type t = int
let regCount = ref 0
let new_temp () = 
    let r = !regCount in
    regCount := r + 1;
    r

let to_string (reg : t) = 
  "t" ^ (string_of_int reg)