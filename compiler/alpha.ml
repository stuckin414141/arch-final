(*
* Re-write function arguments and let bindings to their alpha-equivalents.
* This has to be done before lowering to MIR so that closure conversion isn't cancer
*)
module Renamings = struct
  type t = int Symbols.SymbolTable.t

  (*Gets current name, also returns updated naming envrionment *)
  let get_current_name (var_name : string) (renamings : t) =
    match (Symbols.SymbolTable.find_opt var_name renamings) with
    | Some counter ->
      var_name ^ (string_of_int counter), renamings
    | _ -> (*should never happen*)
      failwith ("used variable " ^ var_name ^ " before declaration")

  (*Used when creating a variable for a new scope. Returns the 
  new name (used within scope), along with symbol table*)
  let create_new_name (var_name : string) (renamings : t) = 
    match (Symbols.SymbolTable.find_opt var_name renamings) with
    | Some counter ->
      var_name ^ (string_of_int (counter + 1)), 
      renamings |> Symbols.SymbolTable.add var_name (counter + 1)
    | None ->
      var_name ^ (string_of_int (0)),
      renamings |> Symbols.SymbolTable.add var_name 0

  let empty = Symbols.SymbolTable.empty
end

let rec rewrite_stmt (renamings : Renamings.t) (ast : Untyped_ast.stmt) : 
  Untyped_ast.stmt * Renamings.t = 
  match ast with
  | While (cond_expr, body_stmt) ->
    let (cond_ast, cond_renamings) = rewrite_expr renamings cond_expr in
    let (body_ast, _) = rewrite_stmt cond_renamings body_stmt in
    (While(cond_ast, body_ast), renamings)
  | LetStmt (var_name, var_typ, init_expr, is_escaping, is_recursive) ->
    let updated_var_name, updated_renamings = Renamings.create_new_name var_name renamings in
    let (init_ast, _) = 
      if is_recursive then
        rewrite_expr updated_renamings init_expr
      else
        rewrite_expr renamings init_expr 
    in
    (LetStmt (updated_var_name, var_typ, init_ast, is_escaping, is_recursive),
    updated_renamings)
  | Print (expr) ->
    let (expr_ast, _) = rewrite_expr renamings expr in
    (Print (expr_ast), renamings)
  | Assign (target, value) ->
    let (target_ast, _) = rewrite_expr renamings target in
    let (value_ast, _) = rewrite_expr renamings value in
    (Assign (target_ast, value_ast), renamings)
  | IfUnit (cond, body) ->
    let (cond_ast, cond_renamings) = rewrite_expr renamings cond in
    let (body_ast, _) = rewrite_stmt cond_renamings body in
    (IfUnit (cond_ast, body_ast), renamings)
  | Seq (stmt1, stmt2) ->
    let (stmt1_ast, stmt1_renamings) = rewrite_stmt renamings stmt1 in
    let (stmt2_ast, stmt2_renamings) = rewrite_stmt stmt1_renamings stmt2 in
    (Seq (stmt1_ast, stmt2_ast), stmt2_renamings)
  | TypeDecl (_, _) -> ast, renamings
  | Break -> Break, renamings
and rewrite_expr (renamings : Renamings.t) (ast : Untyped_ast.expr) : Untyped_ast.expr * Renamings.t = 
  match ast with
  | MemberOf (records, field_name) ->
    let (records_ast, _) = rewrite_expr renamings records in
    (MemberOf(records_ast, field_name), renamings)
  | Var var_name ->
    let (alpha_var_name, _) = Renamings.get_current_name var_name renamings in
    (Var alpha_var_name, renamings)
  | Num i -> 
    (Num i, renamings)
  | Bool b ->
    (Bool b, renamings)
  | Nullptr ->
    (Nullptr, renamings)
  | RecordExp fields ->
    let alpha_fields = 
      let convert_field (name, expr) = 
        let (expr_ast, _) = rewrite_expr renamings expr in
        (name, expr_ast)
      in
      List.map convert_field fields
    in
    RecordExp alpha_fields, renamings
  | If (cond, then_expr, else_expr) ->
    let (cond_ast, cond_renamings) = rewrite_expr renamings cond in
    let (then_ast, _) = rewrite_expr cond_renamings then_expr in
    let (else_ast, _) = rewrite_expr cond_renamings else_expr in
    (If (cond_ast, then_ast, else_ast), renamings)
  | Let (var_name, var_typ, init, body, is_closure, is_recursive) ->
    let alpha_var_name, updated_renamings = 
      Renamings.create_new_name var_name renamings
    in
    let (init_ast, _) = 
      if is_recursive then 
        rewrite_expr updated_renamings init
      else
        rewrite_expr renamings init
    in
    let (body_ast, _) = rewrite_expr updated_renamings body in
    Let (alpha_var_name, var_typ, init_ast, body_ast, is_closure, is_recursive),
    renamings
  | BinOp (left, op, right) ->
    let (left_ast, _) = rewrite_expr renamings left in
    let (right_ast, _) = rewrite_expr renamings right in
    BinOp(left_ast, op, right_ast), renamings
  | ESeq (stmt, expr) ->
    let (stmt_ast, stmt_renamings) = rewrite_stmt renamings stmt in
    let (expr_ast, _) = rewrite_expr stmt_renamings expr in
    ESeq (stmt_ast, expr_ast), renamings
  | Ftmlk (args, body) ->
    let get_alpha_renaming (alpha_args, alpha_renamings) arg = 
      let (arg_name, typ, is_closure) = arg in 
      let alpha_name, updated_renamings = 
        Renamings.create_new_name arg_name alpha_renamings
      in
      ((alpha_name, typ, is_closure) :: alpha_args, updated_renamings)
    in
    let (alpha_args, func_renamings) = 
      List.fold_left get_alpha_renaming ([], renamings) args
    in
    let body_ast, _ = 
      rewrite_expr func_renamings body
    in
    Ftmlk (alpha_args |> List.rev, body_ast),
    renamings
  | FtmlkApp (func, args) ->
    let get_alpha_arg alpha_args arg = 
      let (arg_ast, _) = rewrite_expr renamings arg in
      arg_ast :: alpha_args
    in
    let alpha_args = List.fold_left get_alpha_arg [] args |> List.rev
    in
    let (func_ast, _) = rewrite_expr renamings func in
    FtmlkApp (func_ast, alpha_args), renamings

  let convert ast = 
    let (result, _) = rewrite_stmt (Renamings.empty) ast in
    result