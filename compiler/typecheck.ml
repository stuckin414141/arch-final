(*The overall goal of this stage (aside from checking that types are valid)
* Is producing a typed AST
*)

type types = Types.env
let lookup = Symbols.SymbolTable.find_opt

module UA = Untyped_ast

let rec convert_to_type (typ : UA.type_placeholder) (type_env : types) : Types.t =
  match typ with
  | Typ name ->
    (match Symbols.SymbolTable.find_opt name type_env with 
    | Some real -> real
    | None -> failwith ("Type " ^ name ^ " used before declaration")
    )
  | ArrowType (arg, rest) ->
    Ftmlk (convert_to_type arg type_env, convert_to_type rest type_env)
  | RecordType fields ->
    let 
      convert_field (s, t) : string * Types.t= (s, convert_to_type t type_env) 
    in
      Record (List.map convert_field fields)

let is_valid_record_type fields = 
  List.fold_left (fun e (s, _) -> 
    List.length (List.filter (fun (so, _) -> so = s)
    fields) = 1 && e) true fields

let rec check_valid_type_decl (type_name : string) 
  (body : UA.type_placeholder) (type_env : types): bool = 
    match body with
    (*if it goes through a record, we check that there 
      aren't duplicate names*)
    | RecordType fields -> 
      is_valid_record_type fields
    | ArrowType (t1, t2) ->
      (check_valid_type_decl type_name t1 type_env) && 
      (check_valid_type_decl type_name t2 type_env)
    | Typ str ->
      if type_name = str then false
      else true



(*first part of types is for variables fyi*)
let rec typecheck_stmt ast (types : types * types) : 
(types * types) * Types.t * Ast.stmt = 
  let (var_types, type_env) = types in
  let err_if_declared var_name = 
    match (lookup var_name var_types) with
    | Some _ -> failwith (var_name ^ " is already declared")
    | None -> ()
  in
  match ast with
  | UA.Break -> (types, Types.Unit, Ast.Break)
  | UA.While (cond, body) ->
      let (cond_types, cond_type, cond_ast) = typecheck_expr cond types in
      if cond_type <> Types.Bool then
        failwith "Condition of while loop must be a boolean"
      else
        let (_, body_type, body_ast) = typecheck_stmt body cond_types in
        if body_type <> Types.Unit then
          failwith "Body of while loop must be a unit"
        else
          let while_ast = Ast.While (cond_ast, body_ast) in
          (types, Types.Unit, while_ast)
  | IfUnit (cond, body) ->
    let (cond_types, cond_type, cond_ast) = typecheck_expr cond types in
    if cond_type <> Types.Bool then
      failwith "Condition of if unit must be a boolean"
    else
      let (_, body_type, body_ast) = typecheck_stmt body cond_types in
      if body_type <> Types.Unit then
        failwith "Body of if unit must be a unit"
      else
        (types, Types.Unit, Ast.IfUnit(cond_ast, body_ast))
  | Print expr ->
    let (_, expr_type, expr_ast) = typecheck_expr expr types in
    if expr_type <> Types.Int && expr_type <> Types.Bool then
      failwith "we can only print ints :("
    else
      (types, Types.Unit, Ast.Print(expr_ast))
  | LetStmt (var_name, var_type, init_expr, is_closure, is_recursive) ->
    err_if_declared var_name;
    let real_type = convert_to_type var_type type_env in
    let updated_vars = Symbols.SymbolTable.add var_name real_type var_types in
    let (init_type, init_expr) = 
        if is_recursive then
          let (_, init_type, init_expr) = typecheck_expr init_expr (updated_vars, type_env) in
          (init_type, init_expr)
        else
          let (_, init_type, init_expr) = typecheck_expr init_expr types in
          (init_type, init_expr)
      in
      if init_type <> real_type then
        failwith "Type of initialization statement does not match variable type"
      else
        ((updated_vars, type_env), Types.Unit, 
        Ast.LetStmt (var_name, real_type, init_expr, is_closure, is_recursive))
  | Assign (target, expr) ->
    let (_, target_typ, target_ast) = typecheck_expr target types in
    let (_, expr_typ, expr_ast) = typecheck_expr expr types in
    if target_typ <> expr_typ then 
      failwith "Types in assign statement must match"
    else
      (types, Types.Unit, Assign (target_ast, expr_ast, expr_typ))
  | Seq (stmt1, stmt2) ->
    let check_unit_and_propogate stmt types = 
        let (types, typ, stmt_ast) = typecheck_stmt stmt types in
        if typ <> Types.Unit then
          failwith "Statement in statement sequence must be a unit"
        else
          (types, stmt_ast)
    in
    let (types, stmt1_ast) = check_unit_and_propogate stmt1 types in
    let (types, stmt2_ast) = check_unit_and_propogate stmt2 types in
    (types, Types.Unit, Ast.Seq(stmt1_ast, stmt2_ast))
  | TypeDecl (type_name, type_body) ->
    if not (check_valid_type_decl type_name type_body type_env) then
      failwith ("Ill-formed type declaration for " ^ type_name)
    else
      let type_env : types = 
        Symbols.SymbolTable.add type_name (Types.Placeholder) type_env 
      in
      let real_type : Types.t = convert_to_type type_body type_env in
      let type_env : types = Symbols.SymbolTable.add type_name real_type type_env in
      ((var_types, type_env), Types.Unit, Ast.Nothing)
and typecheck_expr ast types = 
  let (var_types, type_env) = types in
  match ast with
  | UA.Var var_name ->
    (match lookup var_name var_types with
    | Some typ -> (types, typ, Ast.Var var_name)
    | None -> failwith ("Variable " ^ var_name ^ " used before declaration"))
  | Num n -> (types, Types.Int, Ast.Num n)
  | Bool b -> (types, Types.Bool, Ast.Bool b)
  | If (cond, then_expr, else_expr) ->
    let (cond_types, cond_type, cond_ast) = typecheck_expr cond types in
    if cond_type <> Types.Bool then
      failwith "Condition of if expression must be a boolean"
    else
      let (_, then_type, then_ast) = typecheck_expr then_expr cond_types in
      let (_, else_type, else_ast) = typecheck_expr else_expr cond_types in
      if then_type <> else_type then
        failwith "Then and else branches of if expression must have the same type"
      else
        (types, then_type, Ast.If(cond_ast, then_ast, else_ast, then_type))
  | Let (var_name, var_type, init_expr, body_expr, is_closure, is_recursive) ->
      let real_var_type : Types.t = convert_to_type var_type type_env in
      let updated_var_types = Symbols.SymbolTable.add var_name real_var_type var_types in
      let updated_types = (updated_var_types, type_env) in
      let init_type, init_expr = 
        if is_recursive then
          let (_, init_type, init_expr) = 
            typecheck_expr init_expr updated_types 
          in
          init_type, init_expr
        else
          let (_, init_type, init_expr) = typecheck_expr init_expr types in
          init_type, init_expr
      in
      if init_type <> real_var_type then
        failwith "Type of initialization statement does not match variable type"
      else
        let (_, body_type, body_expr) = typecheck_expr body_expr updated_types in
        (types, body_type, 
        Ast.Let(var_name, real_var_type, init_expr, body_expr, 
        is_closure, is_recursive))
  | BinOp (left, op, right) ->
    let (_, left_type, left_expr) = typecheck_expr left types in
    let (_, right_type, right_expr) = typecheck_expr right types in
    (match op with
    | Binops.Plus | Binops.Minus | Binops.Times | Binops.Div | Binops.Mod
      | Binops.Shl | Binops.Shr | Binops.BAnd | Binops.BOr | Binops.BXor ->
      if left_type <> Types.Int || right_type <> Types.Int then
        failwith "Arithmetic operations require integer operands"
      else
        (types, Types.Int, Ast.BinOp (left_expr, op, right_expr, Types.Int))
    | Binops.Leq | Binops.Geq | Binops.Lt | Binops.Gt ->
      if left_type <> Types.Int || right_type <> Types.Int then
        failwith "Comparison other than eq or neq require integers"
      else
        (types, Types.Bool, Ast.BinOp (left_expr, op, right_expr, Types.Bool))
    | Binops.And | Binops.Or ->
      if left_type <> Types.Bool || right_type <> Types.Bool then
        failwith "Logical operations require boolean operands"
      else
        (types, Types.Bool, Ast.BinOp (left_expr, op, right_expr, Types.Bool))
    | Binops.Eq | Binops.Neq   ->
      if left_type <> right_type then
        failwith "Comparison operations require operands of the same type"
      else
        (types, Types.Bool, Ast.BinOp (left_expr, op, right_expr, Types.Bool)))
  | Ftmlk (params, body) ->
    let param_to_ftmlk params ret = 
      List.fold_right (fun param cur_typ -> Types.Ftmlk (param, cur_typ)) params ret
    in
    let rec build_func_env params var_types =
      match params with
      | [] -> var_types
      | (name, typ, _) :: rest ->
        let real_type = convert_to_type typ type_env in
        let updated_var_types = Symbols.SymbolTable.add name real_type var_types in
        build_func_env rest updated_var_types
    in
    let updated_var_types = build_func_env params var_types in
    let updated_types = (updated_var_types, type_env) in
    let (_, body_type, body_expr) = typecheck_expr body updated_types in
    let params_with_types = List.map (fun (name, typ, b) -> 
      (name, convert_to_type typ type_env, b)) params in
    let param_types = List.map (fun (_, typ, _) -> convert_to_type typ type_env) params in 
    let func_type = param_to_ftmlk param_types body_type in
    (types, func_type, Ast.Ftmlk (params_with_types, body_expr))
  | FtmlkApp (func, args) ->
    (*Any definitions make either in func or in any of the 
    args will not be available outside of them*)
    let check_outer_arg func_type arg = 
      let (_, arg_type, _) = typecheck_expr arg types in
      (match func_type with
      | Types.Ftmlk (outer, rest) ->
        if outer <> arg_type then 
          failwith "wrong types"
        else
          rest
      (*All arguments have been applied, so this is an error*)
      | _ -> failwith "too many arguments")
    in
    let arg_ast arg = 
      let (_, _, arg_ast) = typecheck_expr arg types in
      arg_ast
    in
    let args_ast = List.map arg_ast args in
    let (_, func_type, func_ast) = typecheck_expr func types in
    (types, List.fold_left check_outer_arg func_type args, Ast.FtmlkApp(func_ast, args_ast))
  | ESeq (stmt, expr) ->
    let (types, stmt_type, stmt_ast) = typecheck_stmt stmt types in
    if stmt_type <> Types.Unit then
      failwith "Statement in ESeq doesn't return Unit"
    else
      let (_, expr_type, expr_ast) = typecheck_expr expr types in
      (types, expr_type, Ast.ESeq (stmt_ast, expr_ast))
  | RecordExp fields -> 
      let convert_field (name, init_expr) = 
        let (_, typ, init_ast) = 
          (typecheck_expr init_expr types) 
        in
          (name, typ, init_ast)
      in
      let fields_info = 
        List.fold_right (fun field l -> (convert_field field) :: l) fields []
      in
      let fields_type = 
        List.map (fun (name, typ, _) -> (name, typ)) fields_info 
      in
      let field_ast = 
        Ast.RecordExp (List.map (fun (name, typ, ast) -> (name, ast, typ)) fields_info)
      in
      if not (is_valid_record_type fields_type) then
        failwith "Bad record type";
      (types, Record fields_type, field_ast)
  | MemberOf (record, field) ->
      let (_, record_type, record_ast) = typecheck_expr record types in
      (match record_type with
      | Record fields ->
        let get_field_type fields = 
            let matching_fields = 
                List.filter (fun (s, _) -> if s = field then true else false)
                fields 
              in
                (match matching_fields with
                | [] -> 
                  failwith ("Error: field " ^ field ^ " not found in 
                  record type " ^ (Util.string_of_type record_type))
                | (_, t) :: [] -> t
                | _ -> failwith "Too many records of same name"
                )
                in
        let field_type = get_field_type fields in
        (types, field_type, MemberOf (record_ast, field, record_type))
      | _ -> 
        failwith "Error: attempted to dereference field 
        on non-record type")


let analyze ast = 
  let initial_var_types = Symbols.SymbolTable.empty in
  typecheck_stmt ast (initial_var_types, Types.env)
