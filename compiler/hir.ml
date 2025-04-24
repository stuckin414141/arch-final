type stmt = 
  | Move of Regs.t * expr
  | Store of expr * expr
  | Call of expr * expr list
  | Branch of expr * expr
  | Jump of Labels.t
  | Label of Labels.t
  | Seq of stmt * stmt
and expr = 
  | Binop of expr * Ast.ast_binop * expr
  | Const of int
  | Address of Labels.t
  | Reg of Regs.t
  (*Produces the value located at the memory address specified at expr*)
  | Deref of expr
  | ESeq of stmt * expr