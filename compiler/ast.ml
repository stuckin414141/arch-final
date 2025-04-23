
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

type stmt = 
  | While of expr * stmt
  | LetStmt of string * Types.t * expr * bool ref
  | Print of expr
  | Assign of string * expr
  | IfUnit of expr * stmt
  | Seq of stmt * stmt
  | Break
and
  expr = 
  | Var of string
  | Num of int
  | Bool of bool
  | If of expr * expr * expr
  | Let of string * Types.t * expr * expr * bool ref
  | BinOp of expr * ast_binop * expr
  | ESeq of stmt * expr
  | Ftmlk of (string * Types.t * bool ref) list * expr
  | FtmlkApp of expr * expr list