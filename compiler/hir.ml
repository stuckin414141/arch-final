type stmt = 
  | Load of expr * expr
  | Store of expr * expr
  | Call of expr * expr list
  | Branch of expr * expr
  | Label of Lower.Labels.t
and expr = 
  | Binop of expr * Ast.ast_binop * expr
  | Imm of int
  | Address of Lower.Labels.t
  | Reg of Lower.Regs.t