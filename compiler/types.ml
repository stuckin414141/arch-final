type t = 
  Bool
  | Int
  | Nullptr
  | Ftmlk of t * t
  | Unit
  | Record of (string * t) list
  | Placeholder

type env = t Symbols.SymbolTable.t

let u (typ : string) (real : t) : (env ->env) = 
  Symbols.SymbolTable.add typ real
let env = Symbols.SymbolTable.empty |>
  u "bool" Bool |>
  u "int" Int |>
  u "unit" Unit 

