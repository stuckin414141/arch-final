type lval = 
  | Var of string
  | Temp of Regs.t

type value = 
  | Lval of lval
  | Const of int
  | Address of Labels.t

type rval = 
  | Operation of value * Ast.ast_binop * value
  | Deref of lval
  | Val of value


type stmt = 
  (*Only works with bool --> don't need types*)
  | If of rval * stmt
  | Goto of Labels.t
  | Assign of lval * rval
  (*Stores at the memory location referred to by lval*)
  | Store of lval * value 
  (*Thing to store the value in, location of function, arguments*)
  | Call of lval option * rval * rval list
  | MakeLabel of Labels.t
  | Return of rval