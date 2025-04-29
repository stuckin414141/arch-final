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

module Escape = struct 
  (*
* Perform escape analysis to 
* 1] Determine which variables need to go on the heap
* 2] which variables can't be stored in registers
*)

type escape_table = (int * bool ref) Symbols.SymbolTable.t

module Depths = Set.Make(Int)
  let rec analysis_stmt (venv : escape_table) depth = function
    | Ast.While (cond, body) ->
        let updated_venv, cond_req_env = analysis_expr venv depth cond in
        let _, body_req_env = analysis_stmt updated_venv depth body in
        venv, (Depths.union cond_req_env body_req_env)
    | Ast.Seq (first, rest) ->
        let venv_after_first, first_req = analysis_stmt venv depth first in
        let venv_after_second, second_req = 
          analysis_stmt venv_after_first depth rest 
        in
        venv_after_second, (Depths.union first_req second_req)
    | Ast.LetStmt (name, _, expr, ref, _) ->
        let venv_with_decl = Symbols.SymbolTable.add name (depth, ref) venv in
        analysis_expr venv_with_decl depth expr
    | Ast.Print expr ->
        let _, req_env = analysis_expr venv depth expr in
        venv, req_env
    | Ast.Assign (target, expr, _) ->
      let _, target_req = analysis_expr venv depth target in
      let _ , expr_req = analysis_expr venv depth expr in
      venv, (Depths.union target_req expr_req)
    | Ast.IfUnit (cond, body) ->
        let updated_venv, cond_req = analysis_expr venv depth cond in
        let _, body_req = analysis_stmt updated_venv depth body in 
        venv, (Depths.union cond_req body_req)
    | Ast.Nothing -> venv, Depths.empty
    | Break -> venv, Depths.empty
    and 
     analysis_expr (venv : escape_table) depth = function
      | Ast.Var name ->
          (match Symbols.SymbolTable.find_opt name venv with
          | Some (d, ref) ->
              if d < depth then
                (ref := true;
                venv, (Depths.empty |> Depths.add d))
              else
                venv, Depths.empty
          | None -> venv, Depths.empty)
      | Ast.Num _ -> venv, Depths.empty
      | Ast.Nullptr -> venv, Depths.empty
      | Ast.If (cond, then_branch, else_branch, _) ->
          let updated_venv, cond_req = analysis_expr venv depth cond in
          let _, then_req = analysis_expr updated_venv depth then_branch in
          let _, else_req = analysis_expr updated_venv depth else_branch in
          venv, (Depths.union cond_req then_req |> Depths.union else_req)
      | Ast.Let (name, _, expr, body, ref, is_recursive) ->
          let venv_with_decl = Symbols.SymbolTable.add name (depth, ref) venv in
          let _, init_req = 
            if is_recursive then
              analysis_expr venv_with_decl depth expr 
            else
              analysis_expr venv depth expr
          in
          let _, body_req = analysis_expr venv_with_decl depth body in 
          venv, (Depths.union init_req body_req)
      | Ast.BinOp (left, _, right, _) ->
          let _, left_req = analysis_expr venv depth left in
          let _, right_req = analysis_expr venv depth right in
          venv, (Depths.union left_req right_req)
      | Ast.ESeq (stmt, expr) ->
          let updated_venv, stmt_req = analysis_stmt venv depth stmt in
          let _, expr_req = analysis_expr updated_venv depth expr in
          venv, (Depths.union stmt_req expr_req)
      | Ast.Ftmlk (args, body, req) ->
        let func_venv = List.fold_left (fun venv (name, _, ref) ->
          Symbols.SymbolTable.add name (depth + 1, ref) venv) venv args in
        let _, func_req = analysis_expr func_venv (depth + 1) body in
        if Depths.is_empty func_req then 
          (req := false;
          venv, func_req)
        else
          (req := true;
          venv, (Depths.remove depth func_req))
      | Ast.FtmlkApp (func, args, _) ->
        let _, args_req = 
          List.fold_left (fun (venv, cur) arg -> 
            let (_, arg_req) = analysis_expr venv depth arg in
            (venv, (Depths.union cur arg_req))) 
          (venv, Depths.empty) args 
        in
        let _, func_req= analysis_expr venv depth func in
        venv, (Depths.union args_req func_req)
      | Ast.Bool _ -> venv, Depths.empty
      | Ast.RecordExp fields ->
          let _, req = 
            List.fold_left (fun (venv, 
            field_expr_reqs) (_, expr, _) ->
            let (_, field_req) = analysis_expr venv depth expr in 
            (venv, Depths.union field_expr_reqs field_req)) 
            (venv, Depths.empty) 
            fields
          in
          venv, req
      | Ast.MemberOf (expr, _, _) ->
          let _, req = analysis_expr venv depth expr in
          venv, req
    let analyze ast = 
        let _, _ = analysis_stmt Symbols.SymbolTable.empty 0 ast in
        ast
end

let default_val (typ : Types.t) : Ast.expr =
  match typ with
  | Bool -> Bool false
  | Int -> Num 0 
  | Nullptr -> Nullptr
  | Ftmlk (_, _) -> Nullptr
  | Record _ -> Nullptr
  | Self -> Nullptr
  | Unit -> failwith "unit indicates no value"

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
      | FtmlkApp (func, args, _) ->
        (pe func) @ 
        (List.fold_left (fun sl_fields arg -> sl_fields @ (pe arg))
          [] args)
      | Ast.Ftmlk (_, _, _) -> []
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

  (*wrapper to get the type of the static link + initializing expression
  given the static link of the enclosing function*)
  let get_sl_type_exp args body cur_sl = 
    let temp_sl_fields = 
      get_sl_fields args body
    in
    let temp_sl_record_exp = 
        List.map (fun (field_name, typ) ->
          (field_name, default_val typ, typ)) temp_sl_fields
    in
    let should_have_sl = List.length temp_sl_fields <> 0 in
      (match cur_sl with
      | Some cur_sl_typ ->
        ("0prev", cur_sl_typ) :: temp_sl_fields,
        ("0prev", Ast.Var ("0env"), cur_sl_typ) :: temp_sl_record_exp,
        should_have_sl
      | None -> temp_sl_fields, temp_sl_record_exp, should_have_sl)

(*
We now re-write every function that requires it to take in an environment
argument and produce a static link.
The static link record will be zero-initialized initially (int = 0, bool = false,
ptr types = nullptr)
The general algorithm:
- upon processing an ftmlk expression, if it requires an environment, then 
  we 
  1] get the type of the static link (function + passed in parent static link type)
  2] Use another function to re-write the body to refer to the static link instead
    of the plain variable
      - this does not actually require any knowledge of the static link
        thanks to our naming scheme. The link is still required to fill
        type information in the AST
  3] Re-write the return value of the ftmlk expression to be a record
    that gets OR'ed
*)

let get_prev_sl_type (sl : Types.t) = 
  match sl with
  | Record fields ->
    List.filter (fun (field_name, _) -> field_name = "0prev") fields |>
    List.hd |> snd
  | _ -> failwith "Not a static link"

let expr_of_outer_var (var_name : string) (distance : int) (cur_sl_type : Types.t) =
  if distance = 0 then 
    Ast.MemberOf(Var "0sl", var_name, cur_sl_type)
  else
  let rec walk_sl (steps : int) (sl_type : Types.t) = 
    if steps = 0 then 
      Ast.Var "0env"
    else
      let prev_sl_type = get_prev_sl_type sl_type in
      Ast.MemberOf(walk_sl (steps -1) prev_sl_type, "0prev", prev_sl_type)
  in
  let cur_env_type = get_prev_sl_type cur_sl_type in
  Ast.MemberOf (walk_sl (distance - 1) cur_env_type, var_name, cur_env_type)

(* Observe that we only need to do re-writes for variables that are closure/capture
* Therefore, regardless, we will check of a let/letstmt/assign is done on a variable
* in depths. If so, then it will be converted into a record assignment/dereference.
* the previous alpha-equivalence pass ensures that this works with let... in... expr
*)
(*Two boolean options are to handle recursive calls - first indicates
if it's a function, second indicates if it needs an environment*)
type depths = (int * bool * bool) Symbols.SymbolTable.t

let create_closure (func : Ast.expr) (env : Ast.expr) = 
  Ast.BinOp(
    Ast.RecordExp(
      ["code", func, Int;
      "env", env, Int]
    ), Binops.BOr,
    Ast.Num (0x100 lsl 44),
    Types.Int
  )
let rec create_env_arg (depths : depths) (depth : int)
  (cur_sl : Types.t option) (ast : Ast.expr) : 
  Ast.expr * depths =
  let cea_ignore_depth ast =
    create_env_arg depths depth cur_sl ast |> fst
  in
  match ast with
  | Ftmlk (args, body, req_env) ->
    (*TODO: write closure args to closure*)
    let process_arg depths (arg_name, _, is_closure) = 
      if !is_closure then 
        Symbols.SymbolTable.add arg_name (depth + 1, false, false) depths
      else
        depths 
    in
    let func_depths = List.fold_left process_arg depths args in
    let sl_type, sl_expr, should_have_sl = 
      get_sl_type_exp args body cur_sl 
    in
    if (should_have_sl) || !req_env then
      let arg_to_closure (stmts : Ast.stmt) ((arg_name, arg_typ, is_closure)) =
        if !is_closure then 
          Ast.Seq(stmts, 
          Assign(
            MemberOf(Var "0sl", arg_name, Record sl_type),
            Var arg_name,
            arg_typ)
          )
        else
          stmts
      in
      let move_arg_to_closure = List.fold_left (arg_to_closure) Ast.Nothing args in
      let updated_body = 
        create_env_arg func_depths (depth + 1) (Some (Types.Record sl_type)) body
        |> fst
      in
      let updated_body = Ast.ESeq(move_arg_to_closure, updated_body) in
      let updated_args, _ = 
        (match cur_sl with 
        | Some typ -> ("0env", typ, ref false) :: args, true
        | None -> args, false)
      in
      let hoisted_func = 
        Ast.Ftmlk (
          updated_args,
          Let("0sl",Record sl_type, RecordExp sl_expr, updated_body, ref false, false),
          req_env
        )
      in
      if !req_env then 
        create_closure hoisted_func (Var "0sl"), depths
      else
        hoisted_func, depths
    else
      Ftmlk (
        args,
        (*If it doesn't need an environment then it shouldn't have to access outer variables.
        Therefore we don't pass the deps*)
        create_env_arg depths (depth+1) None body |> fst,
        req_env
      ), depths
  | MemberOf (expr, field_name, typ) ->
    MemberOf (cea_ignore_depth expr, field_name, typ), depths
  | RecordExp fields ->
    RecordExp (List.map (fun (name, expr, typ) -> 
      (name, cea_ignore_depth expr, typ)) fields), depths
  | If (cond_exp, then_exp, else_exp, typ) ->
    let (cond_ast, cond_depths) = create_env_arg depths depth cur_sl cond_exp in
    If(cond_ast,
      create_env_arg cond_depths depth cur_sl then_exp |> fst,
      create_env_arg cond_depths depth cur_sl else_exp |> fst,
      typ), depths
  | Let (var_name, typ, init_expr, body_expr, is_closure, is_rec) ->
    if !is_closure then 
      let is_func_type = (match typ with
      | Ftmlk (_,_) -> true
      | _ -> false)
      in
      let updated_depths = 
        Symbols.SymbolTable.add var_name (depth, is_func_type, false) depths
      in
      let updated_init_expr,_ = 
        if is_rec then 
          create_env_arg updated_depths depth cur_sl init_expr
        else
          create_env_arg depths depth cur_sl init_expr
      in
      let updated_body_expr, _ = 
        create_env_arg updated_depths depth cur_sl body_expr
      in
      let cur_sl_type = (
        match cur_sl with 
        | Some typ -> typ
        | None -> failwith ("missing static link, couldn't store " ^ var_name)
      )
      in
      ESeq(
        Assign(
          MemberOf(Var "0sl", var_name, cur_sl_type),
          updated_init_expr,
          typ
        ),
        updated_body_expr
      ), depths
    else
      (*Note: we do not need to add the newly-declared/defined variable
      into depths since we have a guarantee that the nested function
      wont' try accessing it*)
      Let (
        var_name,
        typ,
        create_env_arg depths depth cur_sl init_expr |> fst,
        create_env_arg depths depth cur_sl init_expr|> fst,
        is_closure,
        is_rec
      ), depths
  | BinOp (left, op, right, typ) ->
    BinOp (
      cea_ignore_depth left,
      op,
      cea_ignore_depth right,
      typ
    ), depths
  | ESeq (stmt, expr) ->
    let (stmt_ast, stmt_depths) = create_env_arg_stmt depths depth cur_sl stmt in
    ESeq (
      stmt_ast,
      create_env_arg stmt_depths depth cur_sl expr |> fst
    ), depths
  | FtmlkApp (func, args, typ) ->
    let args_ast = List.map (fun e -> cea_ignore_depth e) args in
    let func_ast = cea_ignore_depth func in
    let is_func_normal = 
      Ast.BinOp(
        Ast.BinOp(
          func_ast,
          Binops.BAnd,
          Num (0xfff lsl 44),
          Int
        ), 
        Binops.Eq,
        Num 0,
        Bool
      )
    in
    let call_closure = 
      let closure_type = Types.Record [("code", Int); ("env", Int)] in
      let clean_closure = 
        Ast.BinOp(
          func_ast,
          Binops.BAnd,
          Num (0xffffffffffff),
          Int
        )
      in
      let code = Ast.MemberOf(clean_closure, "code", closure_type) in
      let env = Ast.MemberOf(clean_closure, "env", closure_type) in
        Ast.FtmlkApp (
          code,
          env :: args_ast,
          typ
        )
    in
    Ast.(
      If (
        is_func_normal,
        FtmlkApp (func_ast, args_ast, typ),
        call_closure,
        typ
      )
    ), depths
  | Var var_name ->
    (match Symbols.SymbolTable.find_opt var_name depths with
    | Some (desired, is_func, _) ->
      let sl_type = 
        (match cur_sl with
        | Some typ -> typ
        | None -> failwith "Error: static link not found")
      in
      if is_func then 
        (Var var_name)
      else
        (expr_of_outer_var var_name (depth - desired) sl_type)
    | None -> (Var var_name)
      ), depths
  | Num _ | Nullptr | Bool _ -> ast, depths
  and create_env_arg_stmt (depths : depths) (depth : int) cur_sl (ast : Ast.stmt) = 
    let cea = create_env_arg depths depth cur_sl in
    match ast with
    | While (cond, stmt) ->
      let cond_ast, cond_depths = create_env_arg depths depth cur_sl cond in
      While (cond_ast,
      create_env_arg_stmt cond_depths depth cur_sl stmt |> fst),
      depths
    | LetStmt (var_name, typ, init_expr, is_closure, is_rec) ->
      if not !is_closure then
        LetStmt(
          var_name,
          typ,
          cea init_expr |> fst,
          is_closure,
          is_rec
        ), depths
      else
        let is_func_type = 
        (match typ with
        | Ftmlk (_,_) -> true
        | _ -> false)
        in
        let updated_depths = 
          Symbols.SymbolTable.add var_name (depth, is_func_type, false) depths
        in
        let updated_init_expr,_ = 
          if is_rec then 
            create_env_arg updated_depths depth cur_sl init_expr
          else
            create_env_arg depths depth cur_sl init_expr
        in
        let cur_sl_type = (
          match cur_sl with 
          | Some typ -> typ
          | None -> failwith ("missing static link, couldn't store " ^ var_name)
        )
        in
          Assign(
            MemberOf(Var "0sl", var_name, cur_sl_type),
            updated_init_expr,
            typ
          ), updated_depths
    | Print (expr) ->
      Print (cea expr |> fst), depths
    | Assign (target, src, typ) ->
      Assign (
        cea target |> fst,
        cea src |> fst,
        typ
      ), depths
    | IfUnit (cond, stmt) ->
      let cond_ast,cond_depths = create_env_arg depths depth cur_sl cond in
      IfUnit (
        cond_ast,
        create_env_arg_stmt cond_depths depth cur_sl stmt |> fst
      ), depths
    | Seq (stmt1, stmt2) ->
      let stmt1_ast, stmt1_depths = create_env_arg_stmt depths depth cur_sl stmt1 in
      let stmt2_ast, stmt2_depths = create_env_arg_stmt stmt1_depths depth cur_sl stmt2 in
      Seq (
        stmt1_ast,
        stmt2_ast
      ), stmt2_depths
    | Break | Nothing -> ast,depths

let create_env (ast : Ast.stmt) = 
  (*Since the global is a function, we will also have to analyze main*)
  let main_sl_type, main_sl_expr, main_sl_maybe = 
    get_sl_type_exp [] (Ast.ESeq (ast, Num 0)) None 
  in
  if main_sl_maybe then
    Ast.Seq(Ast.LetStmt ("0sl", Record main_sl_type, RecordExp main_sl_expr, 
  ref false, false),
      create_env_arg_stmt (Symbols.SymbolTable.empty) 0 (Some (Record main_sl_type)) ast
      |> fst)
  else
    create_env_arg_stmt (Symbols.SymbolTable.empty) 0 None ast |> fst
