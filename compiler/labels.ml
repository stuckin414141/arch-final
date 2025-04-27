type t = string
let tempCount = ref 0
let new_label () = 
  let l = !tempCount in
  tempCount := l + 1;
  "L" ^ string_of_int l
let named_label name = name

let to_string t = t