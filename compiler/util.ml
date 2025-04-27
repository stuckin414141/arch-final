(*All written by Claude*)

(* Helper function for indentation *)
let indent_str n = String.make n ' '

(* Binary operator string conversion *)
let string_of_binop : Ast.ast_binop -> string = function 
  | Ast.Plus  -> "+"
  | Ast.Minus -> "-"
  | Ast.Times -> "*"
  | Ast.Div   -> "/"
  | Ast.Mod   -> "%"
  | Ast.And   -> "&&"
  | Ast.Or    -> "||"
  | Ast.Eq    -> "=="
  | Ast.Neq   -> "!="
  | Ast.Lt    -> "<"
  | Ast.Gt    -> ">"
  | Ast.Leq   -> "<="
  | Ast.Geq   -> ">="
  | Ast.Shl   -> "<<"
  | Ast.Shr   -> ">>"
  | Ast.BAnd  -> "&"
  | Ast.BOr   -> "|"
  | Ast.BXor  -> "^"

(* Type to string conversion *)
let rec string_of_type : Types.t -> string = function
  | Types.Int -> "int"
  | Types.Bool -> "bool"
  | Types.Ftmlk (t1, t2) ->
      "(" ^ (string_of_type t1) ^ ")->(" ^ (string_of_type t2) ^ ")"
  | Types.Unit -> "unit"
  | Types.Record fields ->
      let field_strs = List.map (fun (name, typ) -> name ^ ": " ^ string_of_type typ) fields in
      "{" ^ String.concat ", " field_strs ^ "}"
  | Types.Placeholder -> "Placeholder"

(*
 * Untyped AST string conversion
 *)

(* Type placeholder to string *)
let rec string_of_type_placeholder depth = function
  | Untyped_ast.RecordType fields ->
      let indentation = indent_str depth in
      let field_strings = List.map (fun (name, typ) ->
        indentation ^ "  Field " ^ name ^ "\n" ^
        string_of_type_placeholder (depth + 4) typ
      ) fields in
      indentation ^ "RecordType\n" ^ String.concat "" field_strings
  | Untyped_ast.ArrowType (t1, t2) ->
      let indentation = indent_str depth in
      indentation ^ "ArrowType\n" ^
      indentation ^ "  From\n" ^
      string_of_type_placeholder (depth + 4) t1 ^
      indentation ^ "  To\n" ^
      string_of_type_placeholder (depth + 4) t2
  | Untyped_ast.Typ name ->
      indent_str depth ^ "Typ " ^ name ^ "\n"

(* Untyped statements and expressions to string *)
let rec string_of_untyped_stmt depth = function 
  | Untyped_ast.While (cond, body) ->
      let indentation = indent_str depth in
      indentation ^ "While\n" ^
      string_of_untyped_expr (depth + 2) cond ^
      string_of_untyped_stmt (depth + 2) body
  | Untyped_ast.Seq (first, rest) ->
      let indentation = indent_str depth in
      indentation ^ "Seq\n" ^
      string_of_untyped_stmt (depth + 2) first ^
      string_of_untyped_stmt (depth + 2) rest
  | Untyped_ast.LetStmt (name, typ, expr, ref, is_recursive) ->
      let indentation = indent_str depth in
      indentation ^ "LetStmt " ^ name ^ "\n" ^
      indentation ^ "  Type\n" ^
      string_of_type_placeholder (depth + 4) typ ^
      indentation ^ "  Expr\n" ^
      string_of_untyped_expr (depth + 4) expr ^
      indentation ^ "  Ref: " ^ string_of_bool !ref ^ "\n" ^
      indentation ^ "  Is recursive: " ^ string_of_bool is_recursive ^ "\n"
  | Untyped_ast.Print expr ->
      let indentation = indent_str depth in
      indentation ^ "Print\n" ^
      string_of_untyped_expr (depth + 2) expr
  | Untyped_ast.Assign (name, expr) ->
      let indentation = indent_str depth in
      indentation ^ "Assign " ^ name ^ "\n" ^
      indentation ^ "  Expr\n" ^
      string_of_untyped_expr (depth + 4) expr
  | Untyped_ast.IfUnit (cond, body) ->
      let indentation = indent_str depth in
      indentation ^ "IfUnit\n" ^
      indentation ^ "  Cond\n" ^
      string_of_untyped_expr (depth + 4) cond ^
      indentation ^ "  Body\n" ^
      string_of_untyped_stmt (depth + 4) body
  | Untyped_ast.TypeDecl (name, typ) ->
      let indentation = indent_str depth in
      indentation ^ "TypeDecl " ^ name ^ "\n" ^
      string_of_type_placeholder (depth + 2) typ
  | Untyped_ast.Break ->
      indent_str depth ^ "Break\n"

and string_of_untyped_expr depth = function
  | Untyped_ast.Var name ->
      indent_str depth ^ "Var " ^ name ^ "\n"
  | Untyped_ast.Num n ->
      indent_str depth ^ "Num " ^ string_of_int n ^ "\n"
  | Untyped_ast.Bool b ->
      indent_str depth ^ "Bool " ^ string_of_bool b ^ "\n"
  | Untyped_ast.If (cond, then_branch, else_branch) ->
      let indentation = indent_str depth in
      indentation ^ "If\n" ^
      indentation ^ "  Cond\n" ^
      string_of_untyped_expr (depth + 4) cond ^
      indentation ^ "  Then\n" ^
      string_of_untyped_expr (depth + 4) then_branch ^
      indentation ^ "  Else\n" ^
      string_of_untyped_expr (depth + 4) else_branch
  | Untyped_ast.Let (name, typ, expr, body, ref, is_recursive) ->
      let indentation = indent_str depth in
      indentation ^ "Let " ^ name ^ "\n" ^
      indentation ^ "  Type\n" ^
      string_of_type_placeholder (depth + 4) typ ^
      indentation ^ "  Expr\n" ^
      string_of_untyped_expr (depth + 4) expr ^
      indentation ^ "  Body\n" ^
      string_of_untyped_expr (depth + 4) body ^
      indentation ^ "  Ref: " ^ string_of_bool !ref ^ "\n" ^
      indentation ^ "  Recursive: " ^ string_of_bool is_recursive ^ "\n"
  | Untyped_ast.BinOp (left, op, right) ->
      let indentation = indent_str depth in
      indentation ^ "BinOp " ^ string_of_binop op ^ "\n" ^
      indentation ^ "  Left\n" ^
      string_of_untyped_expr (depth + 4) left ^
      indentation ^ "  Right\n" ^
      string_of_untyped_expr (depth + 4) right
  | Untyped_ast.ESeq (stmt, expr) ->
      let indentation = indent_str depth in
      indentation ^ "ESeq\n" ^
      indentation ^ "  Stmt\n" ^
      string_of_untyped_stmt (depth + 4) stmt ^
      indentation ^ "  Expr\n" ^
      string_of_untyped_expr (depth + 4) expr
  | Untyped_ast.Ftmlk (args, body) ->
      let indentation = indent_str depth in
      let args_strs = List.map (fun (name, typ, ref) ->
        indentation ^ "    " ^ name ^ "\n" ^
        indentation ^ "      Type\n" ^
        string_of_type_placeholder (depth + 8) typ ^
        indentation ^ "      Ref: " ^ string_of_bool !ref ^ "\n"
      ) args in
      indentation ^ "Ftmlk\n" ^
      indentation ^ "  Args\n" ^
      String.concat "" args_strs ^
      indentation ^ "  Body\n" ^
      string_of_untyped_expr (depth + 4) body
  | Untyped_ast.FtmlkApp (func, args) ->
      let indentation = indent_str depth in
      let args_strs = List.map (fun arg ->
        string_of_untyped_expr (depth + 4) arg
      ) args in
      indentation ^ "FtmlkApp\n" ^
      indentation ^ "  Func\n" ^
      string_of_untyped_expr (depth + 4) func ^
      indentation ^ "  Args\n" ^
      String.concat "" args_strs
  | Untyped_ast.RecordExp fields ->
      let indentation = indent_str depth in
      let fields_strs = List.map (fun (name, expr) ->
        indentation ^ "  Field " ^ name ^ "\n" ^
        indentation ^ "    Expr\n" ^
        string_of_untyped_expr (depth + 6) expr
      ) fields in
      indentation ^ "RecordExp\n" ^
      String.concat "" fields_strs
  | Untyped_ast.MemberOf (expr, field) ->
      let indentation = indent_str depth in
      indentation ^ "MemberOf " ^ field ^ "\n" ^
      indentation ^ "  Expr\n" ^
      string_of_untyped_expr (depth + 4) expr

(* Entry point function for untyped AST *)
let string_of_untyped_ast stmt =
  string_of_untyped_stmt 0 stmt

(*
 * Typed AST string conversion
 *)

let rec string_of_ast_stmt depth = function 
  | Ast.While (cond, body) ->
      let indentation = indent_str depth in
      indentation ^ "While\n" ^
      string_of_ast_expr (depth + 2) cond ^
      string_of_ast_stmt (depth + 2) body
  | Ast.Seq (first, rest) ->
      let indentation = indent_str depth in
      indentation ^ "Seq\n" ^
      string_of_ast_stmt (depth + 2) first ^
      string_of_ast_stmt (depth + 2) rest
  | Ast.LetStmt (name, typ, expr, ref, is_recursive) ->
      let indentation = indent_str depth in
      indentation ^ "LetStmt " ^ name ^ "\n" ^
      indentation ^ "  type: " ^ string_of_type typ ^ "\n" ^
      indentation ^ "  Expr\n" ^
      string_of_ast_expr (depth + 4) expr ^
      indentation ^ "  Ref: " ^ string_of_bool !ref ^ "\n" ^
      indentation ^ "  Is recursive: " ^ string_of_bool is_recursive ^ "\n"
  | Ast.Print expr ->
      let indentation = indent_str depth in
      indentation ^ "Print\n" ^
      string_of_ast_expr (depth + 2) expr
  | Ast.Assign (name, expr, typ) ->
      let indentation = indent_str depth in
      indentation ^ "Assign " ^ name ^ "\n" ^
      indentation ^ "  Expr\n" ^
      string_of_ast_expr (depth + 4) expr ^
      indentation ^ "  Type: " ^ string_of_type typ ^ "\n"
  | Ast.IfUnit (cond, body) ->
      let indentation = indent_str depth in
      indentation ^ "IfUnit\n" ^
      indentation ^ "  Cond\n" ^
      string_of_ast_expr (depth + 4) cond ^
      indentation ^ "  Body\n" ^
      string_of_ast_stmt (depth + 4) body
  | Ast.Break ->
      indent_str depth ^ "Break\n"
  | Ast.Nothing ->
      indent_str depth ^ "Nothing\n"

and string_of_ast_expr depth = function
  | Ast.Var name ->
      indent_str depth ^ "Var " ^ name ^ "\n"
  | Ast.Num n ->
      indent_str depth ^ "Num " ^ string_of_int n ^ "\n"
  | Ast.Bool b ->
      indent_str depth ^ "Bool " ^ string_of_bool b ^ "\n"
  | Ast.If (cond, then_branch, else_branch, typ) ->
      let indentation = indent_str depth in
      indentation ^ "If\n" ^
      indentation ^ "  Cond\n" ^
      string_of_ast_expr (depth + 4) cond ^
      indentation ^ "  Then\n" ^
      string_of_ast_expr (depth + 4) then_branch ^
      indentation ^ "  Else\n" ^
      string_of_ast_expr (depth + 4) else_branch ^
      indentation ^ "  Type: " ^ string_of_type typ ^ "\n"
  | Ast.Let (name, typ, expr, body, ref, is_recursive) ->
      let indentation = indent_str depth in
      indentation ^ "Let " ^ name ^ "\n" ^
      indentation ^ "  Expr\n" ^
      string_of_ast_expr (depth + 4) expr ^
      indentation ^ "  Body\n" ^
      string_of_ast_expr (depth + 4) body ^
      indentation ^ "  Ref: " ^ string_of_bool !ref ^ "\n" ^
      indentation ^ "  Recursive: " ^ string_of_bool is_recursive ^ "\n" ^
      indentation ^ "  Type: " ^ string_of_type typ ^ "\n"
  | Ast.BinOp (left, op, right, typ) ->
      let indentation = indent_str depth in
      indentation ^ "BinOp " ^ string_of_binop op ^ "\n" ^
      indentation ^ "  Left\n" ^
      string_of_ast_expr (depth + 4) left ^
      indentation ^ "  Right\n" ^
      string_of_ast_expr (depth + 4) right ^
      indentation ^ "  Type: " ^ string_of_type typ ^ "\n"
  | Ast.ESeq (stmt, expr) ->
      let indentation = indent_str depth in
      indentation ^ "ESeq\n" ^
      indentation ^ "  Stmt\n" ^
      string_of_ast_stmt (depth + 4) stmt ^
      indentation ^ "  Expr\n" ^
      string_of_ast_expr (depth + 4) expr
  | Ast.Ftmlk (args, body) ->
      let indentation = indent_str depth in
      let args_strs = List.map (fun (name, typ, ref) ->
        indentation ^ "    " ^ name ^ " : " ^ string_of_type typ ^ "\n" ^
        indentation ^ "      Ref: " ^ string_of_bool !ref ^ "\n"
      ) args in
      indentation ^ "Ftmlk\n" ^
      indentation ^ "  Args\n" ^
      String.concat "" args_strs ^
      indentation ^ "  Body\n" ^
      string_of_ast_expr (depth + 4) body
  | Ast.FtmlkApp (func, args) ->
      let indentation = indent_str depth in
      let args_strs = List.map (fun arg ->
        string_of_ast_expr (depth + 4) arg
      ) args in
      indentation ^ "FtmlkApp\n" ^
      indentation ^ "  Func\n" ^
      string_of_ast_expr (depth + 4) func ^
      indentation ^ "  Args\n" ^
      String.concat "" args_strs
  | Ast.RecordExp fields ->
      let indentation = indent_str depth in
      let fields_strs = List.map (fun (name, expr, typ) ->
        indentation ^ "  Field " ^ name ^ "\n" ^
        indentation ^ "    Expr\n" ^
        string_of_ast_expr (depth + 6) expr ^
        indentation ^ "    Type: " ^ string_of_type typ ^ "\n"
      ) fields in
      indentation ^ "RecordExp\n" ^
      String.concat "" fields_strs
  | Ast.MemberOf (expr, field, typ) ->
      let indentation = indent_str depth in
      indentation ^ "MemberOf " ^ field ^ "\n" ^
      indentation ^ "  Expr\n" ^
      string_of_ast_expr (depth + 4) expr ^
      indentation ^ "  Type: " ^ string_of_type typ ^ "\n"

(* Entry point function for typed AST *)
let string_of_ast stmt =
  string_of_ast_stmt 0 stmt

(*
 * MIR string conversion
 *)

let string_of_lval = function
  | Mir.Var name -> "Var(" ^ name ^ ")"
  | Mir.Temp reg -> "Temp(" ^ Regs.to_string reg ^ ")"

let string_of_value = function
  | Mir.Lval lval -> string_of_lval lval
  | Mir.Const n -> "Const(" ^ string_of_int n ^ ")"
  | Mir.Address label -> "Address(" ^ Labels.to_string label ^ ")"

let string_of_expr = function
  | Mir.Operation (v1, op, v2) -> 
      "Operation(" ^ string_of_value v1 ^ ", " ^ 
      string_of_binop op ^ ", " ^ 
      string_of_value v2 ^ ")"
  | Mir.Value v -> "Value(" ^ string_of_value v ^ ")"

let string_of_mir_stmt depth stmt =
  let indentation = indent_str depth in
  match stmt with
  | Mir.Arg name ->
      indentation ^ "Arg " ^ name ^ "\n"
  | Mir.Goto (label, cond_opt) ->
      let cond_str = match cond_opt with
        | None -> ""
        | Some expr -> " if " ^ string_of_expr expr
      in
      indentation ^ "Goto " ^ Labels.to_string label ^ cond_str ^ "\n"
  | Mir.Assign (lval, expr, size) ->
      indentation ^ "Assign " ^ string_of_lval lval ^ " := " ^
      string_of_expr expr ^ " (size: " ^ string_of_int size ^ ")\n"
  | Mir.AssignDeref (lval, expr, size) ->
      indentation ^ "AssignDeref " ^ string_of_lval lval ^ " := *" ^
      string_of_expr expr ^ " (size: " ^ string_of_int size ^ ")\n"
  | Mir.Store (dest, src, size) ->
      indentation ^ "Store *" ^ string_of_expr dest ^ " := " ^
      string_of_expr src ^ " (size: " ^ string_of_int size ^ ")\n"
  | Mir.Call (dest_opt, func, args) ->
      let dest_str = match dest_opt with
        | None -> ""
        | Some lval -> string_of_lval lval ^ " := "
      in
      let args_str = String.concat ", " (List.map string_of_value args) in
      indentation ^ "Call " ^ dest_str ^ 
      string_of_value func ^ "(" ^ args_str ^ ")\n"
  | Mir.MakeLabel label ->
      indentation ^ "Label " ^ Labels.to_string label ^ "\n"
  | Mir.Return value ->
      indentation ^ "Return " ^ string_of_value value ^ "\n"

(* Entry point function for MIR *)
let string_of_mir stmts =
  String.concat "" (List.map (string_of_mir_stmt 0) stmts)