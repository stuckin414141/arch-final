type t = int
let regCount = ref 0
let new_temp () = 
    let r = !regCount in
    regCount := r + 1;
    r
