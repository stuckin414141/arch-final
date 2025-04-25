type types = Types.t Symbols.SymbolTable.t
let lookup = Symbols.SymbolTable.find_opt
let rec typecheck_stmt ast (types : types) = 
  let err_if_declared var_name = 
    match Symbols.SymbolTable.find_opt var_name types with
    | Some _ -> failwith ("Variable " ^ var_name ^ " already declared")
    | None -> ()
  in
  match ast with
  | Ast.Break -> (types, Types.Unit)
  | Ast.While (cond, body) ->
      let (cond_types, cond_type) = typecheck_expr cond types in
      if cond_type <> Types.Bool then
        failwith "Condition of while loop must be a boolean"
      else
        let (_, body_type) = typecheck_stmt body cond_types in
        if body_type <> Types.Unit then
          failwith "Body of while loop must be a unit"
        else
          (types, Types.Unit)
  | Ast.IfUnit (cond, body) ->
    let (cond_types, cond_type) = typecheck_expr cond types in
    if cond_type <> Types.Bool then
      failwith "Condition of if unit must be a boolean"
    else
      let (_, body_type) = typecheck_stmt body cond_types in
      if body_type <> Types.Unit then
        failwith "Body of if unit must be a unit"
      else
        (types, Types.Unit)
  | Ast.Print expr ->
    let (_, expr_type) = typecheck_expr expr types in
    if expr_type = Types.Unit then
      failwith "Print expression must not return unit"
    else
      (types, Types.Unit)
  | Ast.LetStmt (var_name, var_type, init_expr, _, is_recursive) ->
    err_if_declared var_name;
    let updated_types = Symbols.SymbolTable.add var_name var_type types in
    let init_type = 
        if is_recursive then
          let (_, init_type) = typecheck_expr init_expr updated_types in
          init_type
        else
          let (_, init_type) = typecheck_expr init_expr types in
          init_type
      in
      if init_type <> var_type then
        failwith "Type of initialization statement does not match variable type"
      else
        (updated_types, Types.Unit)
  | Ast.Assign (var_name, expr) ->
    (match lookup var_name types with
    | Some typ ->
      let (_, expr_type) = typecheck_expr expr types in
      if expr_type <> typ then
        failwith "Type of assignment does not match variable type"
      else
        (types, Types.Unit)
    | None ->
      failwith ("Variable " ^ var_name ^ " assigned before declaration")
    )
  | Ast.Seq (stmt1, stmt2) ->
    let check_unit_and_propogate stmt types = 
        let (types_env, typ) = typecheck_stmt stmt types in
        if typ <> Types.Unit then
          failwith "Statement in statement sequence must be a unit"
        else
          types_env
    in
    let updated_types = 
    check_unit_and_propogate stmt1 types |> 
    check_unit_and_propogate stmt2
    in
    (updated_types, Types.Unit)
and typecheck_expr ast types = 
  match ast with
  | Ast.Var var_name ->
    (match lookup var_name types with
    | Some typ -> (types, typ)
    | None -> failwith ("Variable " ^ var_name ^ " used before declaration"))
  | Ast.Num _ -> (types, Types.Int)
  | Ast.Bool _ -> (types, Types.Bool)
  | Ast.If (cond, then_expr, else_expr) ->
    let (_, cond_type) = typecheck_expr cond types in
    if cond_type <> Types.Bool then
      failwith "Condition of if expression must be a boolean"
    else
      let (_, then_type) = typecheck_expr then_expr types in
      let (_, else_type) = typecheck_expr else_expr types in
      if then_type <> else_type then
        failwith "Then and else branches of if expression must have the same type"
      else
        (types, then_type)
  | Ast.Let (var_name, var_type, init_expr, body_expr, _, is_recursive) ->
      let updated_types = Symbols.SymbolTable.add var_name var_type types in
      let init_type = 
        if is_recursive then
          let (_, init_type) = typecheck_expr init_expr updated_types in
          init_type
        else
          let (_, init_type) = typecheck_expr init_expr types in
          init_type
      in
      if init_type <> var_type then
        failwith "Type of initialization statement does not match variable type"
      else
        let (_, body_type) = typecheck_expr body_expr updated_types in
        (types, body_type)
  | Ast.BinOp (left, op, right) ->
    let (_, left_type) = typecheck_expr left types in
    let (_, right_type) = typecheck_expr right types in
    (match op with
    | Ast.Plus | Ast.Minus | Ast.Times | Ast.Div | Ast.Mod
      | Ast.Shl | Ast.Shr | Ast.BAnd | Ast.BOr | Ast.BXor ->
      if left_type <> Types.Int || right_type <> Types.Int then
        failwith "Arithmetic operations require integer operands"
      else
        (types, Types.Int)
    | Ast.And | Ast.Or ->
      if left_type <> Types.Bool || right_type <> Types.Bool then
        failwith "Logical operations require boolean operands"
      else
        (types, Types.Bool)
    | Ast.Eq | Ast.Neq | Ast.Lt | Ast.Gt | Ast.Leq | Ast.Geq ->
      if left_type <> right_type then
        failwith "Comparison operations require operands of the same type"
      else
        (types, Types.Bool))
  | Ast.Ftmlk (params, body) ->
    let param_to_ftmlk params ret = 
      List.fold_right (fun param cur_typ -> Types.Ftmlk (param, cur_typ)) params ret
    in
    let rec check_params params types =
      match params with
      | [] -> types
      | (name, typ, _) :: rest ->
        let updated_types = Symbols.SymbolTable.add name typ types in
        check_params rest updated_types
    in
    let updated_types = check_params params types in
    let (_, body_type) = typecheck_expr body updated_types in
    let param_types = List.map (fun (_, typ, _) -> typ) params in 
    let func_type = param_to_ftmlk param_types body_type in
    (types, func_type)
  | Ast.FtmlkApp (func, args) ->
    (*Any definitions make either in func or in any of the 
    args will not be available outside of them*)
    let check_outer_arg func_type arg = 
      let (_, arg_type) = typecheck_expr arg types in
      (match func_type with
      | Types.Ftmlk (outer, rest) ->
        if outer <> arg_type then 
          failwith "wrong types"
        else
          rest
      (*All arguments have been applied, so this is an error*)
      | _ -> failwith "too many arguments")
    in
    let (_, func_type) = typecheck_expr func types in
    (types, List.fold_left check_outer_arg func_type args)
  | Ast.ESeq (stmt, expr) ->
    let (types, stmt_type) = typecheck_stmt stmt types in
    if stmt_type <> Types.Unit then
      failwith "Statement in ESeq doesn't return Unit"
    else
      (types, typecheck_expr expr types |> snd)

let analyze ast = 
  typecheck_stmt ast Symbols.SymbolTable.empty
