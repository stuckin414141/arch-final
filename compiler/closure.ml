(*
* We will implement closure as a set of re-writes
* Several changes need to be made:
  - modifications to variables accessed by nested functions
    need to be moved to closure
  - accesses of variables used by nested functions need to be moved to closure
  - we need to add an env arg
  - we need to initialize a static link for every function that includes
    nested functions which access outer variables
  - re-write function application to check if it's calling a closure or
    function address
  - re-write ftmlk/function expressions to either return a closure (with
    requisite masks) or a plain old address
*)

(*
It therefore makes sense to perform closure-conversion in three passes:
- The first is to create the environment argument/static links. 
  The principle piece of information that this requires is exactly
  what variables in a function are accessed by nested functions.
  We perform a first pass to mark this information in let statements.
  After let statements are populated, we will perform two passes on every
  function body:
  - the first pass refrains from entering any function body. It is soley
    to construct the type of the current 
    static link, absent the type of the prev pointer
  - we use the previous information to perform another pass that enters function
    bodies, passing the type of the previous static link/environment into the function.
    That type becomes of the type of the function's 0env argument.
  - for all types other than bool, we will treat them as integers and initialize
    them with 0
- Finally, when the static links are all created + their types are determined,
  we will go back through and re-write all variable accesses accordingly
- since MIR no longer does alpha-renaming we may need to keep an environment/static link
  counter to ensure distinct names/arguments
*)

(* Given a function definition, returns a list of the fields required in the
static link (excluding back pointer)*)
let get_sl_fields (args : (string * Types.t * bool ref) list) 
  (body : Ast.expr) : (string * Types.t) list = 
  let closure_args = 
    let is_arg_closure (_, _, is_closure) = !is_closure in
    List.filter is_arg_closure args |>
    List.map (fun (name, typ, _) -> (name, typ)) 
  in
    let rec process_expr (ast : Ast.expr) =
      let pe = process_expr in 
      match ast with
      | Let (var_name, var_typ, init_expr, body_expr, is_closure, _) ->
        let subexpr_sl_fields = 
          (process_expr init_expr) @ (process_expr body_expr)
        in
        if !is_closure then
          (var_name, var_typ) :: subexpr_sl_fields
        else
          subexpr_sl_fields
      | If (cond_expr, then_expr, else_expr, _) ->
        (pe cond_expr) @ (pe then_expr) @ (pe else_expr)
      | Var _ -> []
      | Num _ -> []
      | Nullptr -> []
      | Bool _ -> []
      | RecordExp fields ->
        List.fold_left (fun sl_fields (_, field_expr, _) -> 
          sl_fields @ (pe field_expr)) [] fields
      | BinOp (left, _, right, _) ->
        (pe left) @ (pe right)
      | ESeq (stmt, expr) ->
        (process_stmt stmt) @ (pe expr)
      | FtmlkApp (func, args) ->
        (pe func) @ 
        (List.fold_left (fun sl_fields arg -> sl_fields @ (pe arg))
          [] args)
      | Ast.Ftmlk (_, _) -> []
      | Ast.MemberOf (record_expr, _, _) ->  
        (process_expr record_expr)
    and process_stmt (ast : Ast.stmt) = 
      let pe = process_expr in
      let ps = process_stmt in 
      match ast with 
      | LetStmt (var_name, var_typ, init_expr, is_closure, _) ->
        let init_expr_sl = pe init_expr in
        if !is_closure then
          (var_name, var_typ) :: init_expr_sl
        else 
          init_expr_sl
      | While (cond, body) ->
        (pe cond) @ (ps body)
      | Print (expr) -> pe expr
      | Assign (target, src, _) -> (pe target) @ (pe src)
      | IfUnit (cond, body) -> (pe cond) @ (ps body)
      | Seq (stmt1, stmt2) -> (ps stmt1) @ (ps stmt2)
      | Break -> []
      | Nothing -> []
    in
  (process_expr body) @ closure_args

module Escape = struct 
  (*
* Perform escape analysis to 
* 1] Determine which variables need to go on the heap
* 2] which variables can't be stored in registers
*)

type escape_table = (int * bool ref) Symbols.SymbolTable.t

let rec analysis_stmt (venv : escape_table) depth = function
  | Ast.While (cond, body) ->
      let updated_venv = analysis_expr venv depth cond in
      let _ = analysis_stmt updated_venv depth body in
      venv
  | Ast.Seq (first, rest) ->
      let venv_after_first = analysis_stmt venv depth first in
      analysis_stmt venv_after_first depth rest
  | Ast.LetStmt (name, _, expr, ref, _) ->
      let venv_with_decl = Symbols.SymbolTable.add name (depth, ref) venv in
      analysis_expr venv_with_decl depth expr
  | Ast.Print expr ->
      let _ = analysis_expr venv depth expr in
      venv
  | Ast.Assign (target, expr, _) ->
    let _ = analysis_expr venv depth target in
    let _ = analysis_expr venv depth expr in
    venv
  | Ast.IfUnit (cond, body) ->
      let updated_venv = analysis_expr venv depth cond in
      let _ = analysis_stmt updated_venv depth body in 
      venv
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
    | Ast.Nullptr -> venv
    | Ast.If (cond, then_branch, else_branch, _) ->
        let updated_venv = analysis_expr venv depth cond in
        let _ = analysis_expr updated_venv depth then_branch in
        let _ = analysis_expr updated_venv depth else_branch in
        venv
    | Ast.Let (name, _, expr, body, ref, is_recursive) ->
        let venv_with_decl = Symbols.SymbolTable.add name (depth, ref) venv in
        let _ = 
          if is_recursive then
            analysis_expr venv_with_decl depth expr 
          else
            analysis_expr venv depth expr
        in
        let _ = analysis_expr venv_with_decl depth body in 
        venv
    | Ast.BinOp (left, _, right, _) ->
        let _ = analysis_expr venv depth left in
        let _ = analysis_expr venv depth right in
        venv
    | Ast.ESeq (stmt, expr) ->
        let updated_venv = analysis_stmt venv depth stmt in
        let _ = analysis_expr updated_venv depth expr in
        venv
    | Ast.Ftmlk (args, body) ->
      let func_venv = List.fold_left (fun venv (name, _, ref) ->
        Symbols.SymbolTable.add name (depth + 1, ref) venv) venv args in
      let _ = analysis_expr func_venv (depth + 1) body in
      venv
    | Ast.FtmlkApp (func, args) ->
      let _ = 
        List.fold_left (fun venv arg -> analysis_expr venv depth arg) venv args 
      in
      let _ = analysis_expr venv depth func in
      venv
    | Ast.Bool _ -> venv
    | Ast.RecordExp fields ->
        let _ = 
          List.fold_left (fun venv (_, expr, _) -> analysis_expr venv depth expr) venv fields
        in
        venv
    | Ast.MemberOf (expr, _, _) ->
        let _ = analysis_expr venv depth expr in
        venv
  let analyze ast = 
      let _ = analysis_stmt Symbols.SymbolTable.empty 0 ast in
      ast
end
(*
We now re-write every function that requires it to take in an environment
argument and produce a static link.
The static link record will be zero-initialized initially (int = 0, bool = false,
ptr types = nullptr)
*)