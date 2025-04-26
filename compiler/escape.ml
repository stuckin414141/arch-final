(*
* Perform escape analysis to 
* 1] Determine which variables need to go on the heap
* 2] which variables can't be stored in registers
*)

type escape_table = (int * bool ref) Symbols.SymbolTable.t

let rec analysis_stmt (venv : escape_table) depth = function
  | Ast.While (cond, body) ->
      let updated_venv = analysis_expr venv depth cond in
      analysis_stmt updated_venv depth body
  | Ast.Seq (first, rest) ->
      let venv_after_first = analysis_stmt venv depth first in
      analysis_stmt venv_after_first depth rest
  | Ast.LetStmt (name, _, expr, ref, _) ->
      let venv_with_decl = Symbols.SymbolTable.add name (depth, ref) venv in
      analysis_expr venv_with_decl depth expr
  | Ast.Print expr ->
      let updated_venv = analysis_expr venv depth expr in
      updated_venv
  | Ast.Assign (name, expr) ->
    (match Symbols.SymbolTable.find_opt name venv with
    | Some (d, ref) ->
        if d < depth then
          ref := true;
        analysis_expr venv depth expr
    | None -> analysis_expr venv depth expr)
  | Ast.IfUnit (cond, body) ->
      let updated_venv = analysis_expr venv depth cond in
      analysis_stmt updated_venv depth body
  | Ast.Nothing -> venv
  | Break -> venv
and 
   analysis_expr (venv : escape_table) depth = function
    | Ast.Var name ->
        (match Symbols.SymbolTable.find_opt name venv with
        | Some (d, ref) ->
            if d < depth then
              ref := true;
            venv
        | None -> venv)
    | Ast.Num _ -> venv
    | Ast.If (cond, then_branch, else_branch) ->
        let updated_venv = analysis_expr venv depth cond in
        let updated_venv_then = analysis_expr updated_venv depth then_branch in
        analysis_expr updated_venv_then depth else_branch
    | Ast.Let (name, _, expr, body, ref, _) ->
        let venv_with_decl = Symbols.SymbolTable.add name (depth, ref) venv in
        let updated_venv = analysis_expr venv_with_decl depth expr in
        analysis_expr updated_venv depth body
    | Ast.BinOp (left, _, right) ->
        let updated_venv = analysis_expr venv depth left in
        analysis_expr updated_venv depth right
    | Ast.ESeq (stmt, expr) ->
        let updated_venv = analysis_stmt venv depth stmt in
        analysis_expr updated_venv depth expr
    | Ast.Ftmlk (args, body) ->
      let func_venv = List.fold_left (fun venv (name, _, ref) ->
        Symbols.SymbolTable.add name (depth + 1, ref) venv) venv args in
      analysis_expr func_venv (depth + 1) body
    | Ast.FtmlkApp (func, args) ->
      let venv_after_args = 
        List.fold_left (fun venv arg -> analysis_expr venv depth arg) venv args 
      in
      analysis_expr venv_after_args depth func
    | Ast.Bool _ -> venv
    | Ast.RecordExp fields ->
        List.fold_left (fun venv (_, expr) -> analysis_expr venv depth expr) venv fields
    | Ast.MemberOf (expr, _) ->
        let updated_venv = analysis_expr venv depth expr in
        updated_venv

let analyze ast = 
  analysis_stmt Symbols.SymbolTable.empty 0 ast