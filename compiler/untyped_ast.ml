type type_placeholder = 
  | RecordType of (string * type_placeholder) list
  | ArrowType of (type_placeholder * type_placeholder)
  | Typ of string


(*Second-to-last bool ref refers to whether an enclosed function refers to it*)
(*Second string always refers to the type*)

type stmt = 
  | While of expr * stmt
  | LetStmt of string * type_placeholder * expr * bool ref * bool
  | Print of expr
  | Assign of expr * expr
  | IfUnit of expr * stmt
  | Seq of stmt * stmt
  | TypeDecl of string * type_placeholder
  | Break
and
  expr = 
  (*Reference the member at field of string from struct
  of expr*)
  | MemberOf of expr * string
  | Var of string
  | Num of int
  | Bool of bool
  | RecordExp of (string * expr) list
  | If of expr * expr * expr
  | Let of string * type_placeholder * expr * expr * bool ref * bool
  | BinOp of expr * Binops.t * expr
  | ESeq of stmt * expr
  | Ftmlk of (string * type_placeholder * bool ref) list * expr
  | FtmlkApp of expr * expr list