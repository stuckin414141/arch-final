
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
  | LetStmt of string * Types.t * expr * bool ref * bool
  | Print of expr
  | Assign of expr * expr * Types.t
  | IfUnit of expr * stmt
  | Seq of stmt * stmt
  | Break
  (*Stopgap *)
  | Nothing
and
  expr = 
  (*Type is the expected type of expr*)
  | MemberOf of expr * string * Types.t
  | Var of string
  | Num of int
  | Nullptr
  | Bool of bool
  | RecordExp of (string * expr * Types.t) list
  | If of expr * expr * expr * Types.t
  | Let of string * Types.t * expr * expr * bool ref * bool
  | BinOp of expr * Binops.t * expr * Types.t
  | ESeq of stmt * expr
  | Ftmlk of (string * Types.t * bool ref) list * expr * bool ref
  | FtmlkApp of expr * expr list * Types.t