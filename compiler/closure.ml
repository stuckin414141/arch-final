(*
* The environment argument will be referred to as "0env"
* The corresponding backwards pointer will be "0prev"
* We stage closure conversion in three phases:
* 1] Mark variables that need to be stored in closure, convert accesses & 
*   writes in nested functions into references to the variable on closure
* 2] recurse over functions, mark which ones need an environment argument,
* come up with environment type
* 3] 
* TODO: this will fuck over recursive function calls
*)

type var_depth = (Types.t * bool ref * int) Symbols.SymbolTable.t

(*Generates the chain of dereferences back up to the desired static link*)
let rec get_outer_sl (cur_depth : int) (desired_depth : int) : Untyped_ast.expr = 
  if cur_depth = (desired_depth + 1) then 
    (Var "0env")
  else 
    MemberOf ((get_outer_sl (cur_depth - 1) desired_depth), "0prev")

let rec escape_analysis_stmt (venv : var_depth) depth (ast : Untyped_ast.stmt) = 
  match ast with
  | While (cond, body) ->
      let u_venv, u_cond = analysis_expr venv depth cond in
      let u_venv, u_body = analysis_stmt u_venv depth body in
      (u_venv, While (u_cond, u_body))
  | Seq (first, rest) ->
      let venv_after_first, u_first = analysis_stmt venv depth first in
      let venv_after_rest, u_rest = analysis_stmt venv_after_first depth rest in
      (venv_after_first, Seq (u_first, u_rest))
  | LetStmt (name, typ, expr, ref, is_recursive) ->
      let venv_with_decl = Symbols.SymbolTable.add name (depth, ref) venv in
      let (_, u_expr) = let analysis_expr venv depth expr in
      venv_with_decl, LetStmt(name, typ, u_expr, ref, is_recursive)
  | Print expr ->
      let _, expr_ast = analysis_expr venv depth expr in
      Print expr_ast
  | Assign (name, expr) ->
    let (_, u_expr) = analysis_expr venv depth expr in
    (match Symbols.SymbolTable.find_opt name venv with
    | Some (d, ref) ->
        if d < depth then
          ref := true;
          venv,
          (*TODO: pending support for record assignments*)
    | None -> analysis_expr venv depth expr)
  | IfUnit (cond, body) ->
      let updated_venv = analysis_expr venv depth cond in
      analysis_stmt updated_venv depth body
  | Ast.Nothing -> venv
  | Break -> venv
and 
   escape_analysis_expr 
      (venv : var_depth)
       depth 
      (ast : Untyped_ast.expr) 
      : var_depth * Untyped_ast.expr= 
    match ast with
    | UA.Var name ->
        (match Symbols.SymbolTable.find_opt name venv with
        | Some (typ, ref, var_depth) ->
            if var_depth < depth then
              ref := true;
              venv, MemberOf (get_outer_sl depth var_depth, name)
        | None -> venv, ast)
    | Num _ -> venv, ast
    | If (cond, then_branch, else_branch) ->
        let cond_venv, cond_ast = escape_analysis_expr venv vars_used depth cond in
        let _, then_ast = 
          escape_analysis_expr cond_venv cond_vu depth then_branch in
        let _, total_vu, else_ast = 
          escape_analysis_expr cond_venv then_vu depth else_branch in
        (venv, UA.If (cond_ast, then_ast, else_ast))
    | Let (name, typ, init, body, ref, is_recursive) ->
        let venv_with_decl = Symbols.SymbolTable.add name (depth, typ, ref) venv in
        let (_, init_vu, updated_init_ast) = 
        if is_recursive then
          escape_analysis_expr venv_with_decl depth init 
        else 
          escape_analysis_expr venv depth expr
        in
        let (_, updated_body_ast) = 
          escape_analysis_expr venv_with_decl depth body
        in
        (venv, 
        UA.Let (name, typ, updated_init_ast, updated_body_ast, ref, is_recursive))
    | BinOp (left, op, right) ->
        let (_, u_left) = escape_analysis_expr venv depth left in
        let (_, u_right) = escape_analysis_expr venv depth right in
        (venv, Binop(u_left, op, u_right))
    | ESeq (stmt, expr) ->
        let (stmt_venv, u_stmt) = 
          escape_analysis_stmt venv  depth stmt 
        in
        let (_, u_expr) = escape_analysis_expr stmt_venv depth expr in
        (venv, UA.Eseq (u_stmt, u_expr))
    | Ftmlk (args, body) ->
      let func_venv = List.fold_left (fun venv (name, _, ref) ->
        Symbols.SymbolTable.add name (depth + 1, ref) venv) venv args in
      let (_, body) = escape_analysis_expr func_venv (depth + 1) body in
        (venv, Ftmlk (args, body))
    | FtmlkApp (func, args) ->
      let (venv_after_args, args_ast) = 
        List.fold_left (fun (venv, args) arg -> 
          let (u_venv, arg_ast) = 
            escape_analysis_expr venv depth arg
          in
          (venv, arg_ast :: args)) venv args 
      in
      let (_, func_ast) = 
        escape_analysis_expr venv_after_args depth func
      in
      (venv, FtmlkApp (func_ast, args_ast))
    | Bool _ -> venv
    | RecordExp fields ->
        let process_field (venv, fields_ast) (name, expr) =
          let (u_venv, field_ast) = 
            escape_analysis_expr venv depth expr
          in
            (u_venv, (name, field_ast) :: fields_ast)
        in
        let (u_venv, fields_ast) = 
          List.fold_left process_field (venv, []) fields
        in
        (u_venv, RecordExp fields_ast)
    | MemberOf (expr, field_name) ->
        let updated_venv, expr_ast = escape_analysis_expr venv depth expr in
        (updated_venv, UA.MemberOf (expr_ast, field_name))

