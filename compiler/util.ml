(*written by claude*)

let token_to_string (t : Grammar.token) : string =
    match t with
    | Grammar.EOF -> "EOF"
    | Grammar.ID id -> "ID(" ^ id ^ ")"
    | Grammar.NUM n -> "NUM(" ^ string_of_int n ^ ")"
    | Grammar.COMMA -> "COMMA"
    | Grammar.COLON -> "COLON"
    | Grammar.SEMICOLON -> "SEMICOLON"
    | Grammar.LPAREN -> "LPAREN"
    | Grammar.RPAREN -> "RPAREN"
    | Grammar.LBRACE -> "LBRACE"
    | Grammar.RBRACE -> "RBRACE"
    | Grammar.PLUS -> "PLUS"
    | Grammar.MINUS -> "MINUS"
    | Grammar.TIMES -> "TIMES"
    | Grammar.DIVIDE -> "DIVIDE"
    | Grammar.MODULO -> "MODULO"
    | Grammar.EQ -> "EQ"
    | Grammar.NEQ -> "NEQ"
    | Grammar.LT -> "LT"
    | Grammar.LE -> "LE"
    | Grammar.GT -> "GT"
    | Grammar.GE -> "GE"
    | Grammar.BOR -> "BOR"
    | Grammar.BAND -> "BAND"
    | Grammar.BXOR -> "BXOR"
    | Grammar.SHL -> "SHL"
    | Grammar.SHR -> "SHR"
    | Grammar.AND -> "AND"
    | Grammar.OR -> "OR"
    | Grammar.ASSIGN -> "ASSIGN"
    | Grammar.IF -> "IF"
    | Grammar.THEN -> "THEN" 
    | Grammar.ELSE -> "ELSE"
    | Grammar.WHILE -> "WHILE"
    | Grammar.LET -> "LET"
    | Grammar.IN -> "IN"
    | Grammar.BREAK -> "BREAK"
    | Grammar.FTMLK -> "FTMLK"
    | Grammar.PRINT -> "PRINT"
    | Grammar.ARROW -> "ARROW"
    | Grammar.UNIT -> "UNIT"
    | Grammar.INT -> "INT"
    | Grammar.BOOL -> "BOOL"
    | Grammar.TRUE -> "TRUE"
    | Grammar.FALSE -> "FALSE"
    | Grammar.REC -> "REC"

let string_of_binop : Ast.ast_binop -> string = function 
    | Ast.Plus ->  "+"
    | Ast.Minus ->  "-"
    | Ast.Times ->  "*"
    | Ast.Div ->  "/"
    | Ast.Mod ->  "%"
    | Ast.And ->  "&&"
    | Ast.Or ->  "||"
    | Ast.Eq ->  "=="
    | Ast.Neq ->  "!="
    | Ast.Lt ->  "<"
    | Ast.Gt ->  ">"
    | Ast.Leq ->  "<="
    | Ast.Geq ->  ">="
    | Ast.Shl ->  "<<"
    | Ast.Shr ->  ">>"
    | Ast.BAnd ->  "&"
    | Ast.BOr ->  "|"
    | Ast.BXor ->  "^"
let rec string_of_type : Types.t -> string = function
    | Types.Int -> "int"
    | Types.Bool -> "bool"
    | Types.Ftmlk (t1, t2) ->
            "(" ^ (string_of_type t1) ^ ")->(" ^ (string_of_type t2) ^ ")"
    | Types.Unit -> "unit"
let indent n = 
    for _ = 0 to n - 1 do
        print_char ' '
    done
let rec print_ast_stmt depth = function 
    | Ast.While (cond, body) ->
        indent depth;
        print_endline "While";
        print_ast_expr (depth + 2) cond;
        print_ast_stmt (depth + 2) body
    | Ast.Seq (first, rest) ->
        indent depth;
        print_endline "Seq";
        print_ast_stmt (depth + 2) first;
        print_ast_stmt (depth + 2) rest
    | Ast.LetStmt (name, typ, expr, ref, is_recursive) ->
        indent depth;
        print_endline ("LetStmt " ^ name);
        indent (depth + 2);
        print_endline ("type: " ^ string_of_type typ);
        indent (depth + 2);
        print_endline "Expr";
        print_ast_expr (depth + 4) expr;
        indent (depth + 2);
        print_endline ("Ref: " ^ string_of_bool !ref);
    indent (depth + 2);
        print_endline ("Is recursive: " ^ string_of_bool is_recursive)
    | Ast.Print expr ->
        indent depth;
        print_endline "Print";
        print_ast_expr (depth + 2) expr
    | Ast.Assign (name, expr) ->
        indent depth;
        print_endline ("Assign " ^ name);
        indent (depth + 2);
        print_endline "Expr";
        print_ast_expr (depth + 4) expr
    | Ast.IfUnit (cond, body) ->
        indent depth;
        print_endline "IfUnit";
        indent (depth + 2);
        print_endline "Cond";
        print_ast_expr (depth + 4) cond;
        indent (depth + 2);
        print_endline "Body";
        print_ast_stmt (depth + 4) body
    | Ast.Break ->
        indent depth;
        print_endline "Break"
and print_ast_expr depth = function
    | Ast.Var name ->
        indent depth;
        print_endline ("Var " ^ name)
    | Ast.Num n ->
        indent depth;
        print_endline ("Num " ^ string_of_int n)
    | Ast.If (cond, then_branch, else_branch) ->
        indent depth;
        print_endline "If";
        indent (depth + 2);
        print_endline "Cond";
        print_ast_expr (depth + 4) cond;
        indent (depth + 2);
        print_endline "Then";
        print_ast_expr (depth + 4) then_branch;
        indent (depth + 2);
        print_endline "Else";
        print_ast_expr (depth + 4) else_branch
    | Ast.Let (name, _, expr, body, ref, is_recursive) ->
        indent depth;
        print_endline ("Let " ^ name);
        indent (depth + 2);
        print_endline "Expr";
        print_ast_expr (depth + 4) expr;
        indent (depth + 2);
        print_endline "Body";
        print_ast_expr (depth + 4) body;
        indent (depth + 2);
        print_endline ("Ref: " ^ string_of_bool !ref);
      indent (depth + 2);
      print_endline ("Recursive: " ^ string_of_bool is_recursive)
    | Ast.BinOp (left, op, right) ->
        indent depth;
        print_endline ("BinOp " ^ string_of_binop op);
        indent (depth + 2);
        print_endline "Left";
        print_ast_expr (depth + 4) left;
        indent (depth + 2);
        print_endline "Right";
        print_ast_expr (depth + 4) right
    | Ast.ESeq (stmt, expr) ->
        indent depth;
        print_endline "ESeq";
        indent (depth + 2);
        print_endline "Stmt";
        print_ast_stmt (depth + 4) stmt;
        indent (depth + 2);
        print_endline "Expr";
        print_ast_expr (depth + 4) expr
    | Ast.Ftmlk (args, body) ->
        indent depth;
        print_endline "Ftmlk";
        indent (depth + 2);
        print_endline "Args";
        List.iter (fun (name, typ, ref) ->
            indent (depth + 4);
            print_endline (name ^ " : " ^ string_of_type typ);
        indent (depth + 6);
            print_endline ("Ref: " ^ string_of_bool !ref)
        ) args;
        indent (depth + 2);
        print_endline "Body";
        print_ast_expr (depth + 4) body
    | Ast.FtmlkApp (func, args) ->
        indent depth;
        print_endline "FtmlkApp";
        indent (depth + 2);
        print_endline "Func";
        print_ast_expr (depth + 4) func;
        indent (depth + 2);
        print_endline "Args";
        List.iter (fun arg ->
            indent (depth + 4);
            print_ast_expr (depth + 4) arg
        ) args
    | Ast.Bool b ->
        indent depth;
        print_endline ("Bool " ^ string_of_bool b)