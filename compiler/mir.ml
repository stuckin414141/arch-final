type lval =
  | Var of string
  | Temp of Regs.t

type value = 
  | Lval of lval
  | Const of int
  | Address of Labels.t

type expr = 
  | Operation of value * Ast.ast_binop * value
  | Not of value
  | Value of value


(*Int fields at the end are size values*)
type stmt = 
  (*We assign a variable name to be an argument*)
  | Arg of string
  (*The conditional jump*)
  | Goto of Labels.t * expr option
  | Assign of lval * expr * int
  (*
    lval = *<expr>
  *)
  | AssignDeref of lval * expr * int
  (*
    *<expr> = <expr>
  *)
  | Store of expr * expr * int
  (*Thing to store the value in, location of function, arguments*)
  | Call of lval option * value * value list
  | MakeLabel of Labels.t
  | Return of value