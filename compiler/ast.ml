
type ast_binop = 
  | Plus
  | Minus
  | Times
  | Div
  | Mod
  | And
  | Or
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
  | Shl
  | Shr
  | BAnd
  | BOr
  | BXor

(*Second-to-last bool ref refers to whether an enclosed function refers to it*)

type ast_stmt = 
  | While of ast_expr * ast_stmt
  | LetStmt of string * Types.t * ast_expr * bool ref
  | Print of ast_expr
  | Assign of string * ast_expr
  | IfUnit of ast_expr * ast_stmt
  | Seq of ast_stmt * ast_stmt
  | Break
and
  ast_expr = 
  | Var of string
  | Num of int
  | Bool of bool
  | If of ast_expr * ast_expr * ast_expr
  | Let of string * Types.t * ast_expr * ast_expr * bool ref
  | BinOp of ast_expr * ast_binop * ast_expr
  | ESeq of ast_stmt * ast_expr
  | Ftmlk of (string * Types.t * bool ref) list * ast_expr
  | FtmlkApp of ast_expr * ast_expr list