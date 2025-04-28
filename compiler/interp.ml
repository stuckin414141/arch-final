

(*Least significant byte at index 0 will be the largest*)
(*This more-or-less does 255-byte base conversions*)
module ByteInteger = struct
  type t = int list
  let word_size = 8

  let num_to_word (num : int) : t = 
    let rec logic (num : int) (place : int) : t = 
      if place < 0 then
        []
      else
        let remainder = num mod 255 in
        remainder :: (logic (num / 255) (place - 1))
    in
    logic num 7

  let word_to_num (word : t) : int =
    let rec logic (word : t) (place : int) : int =
        let place_val = 255. ** (float_of_int place) |> int_of_float in
        match word with
        | byt :: rest ->
          byt * place_val + (logic rest (place + 1))
        | [] -> 0
    in
    logic word 0
    
end

(* We will be storing where all the labels point to*)
(* since our labels are just strings this works quite well*)
type jump_table = Mir.stmt list Symbols.SymbolTable.t 

