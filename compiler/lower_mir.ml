(* this should've been a functor but we only depend on two variables *)

(* Generates a map from field name -> offset from start of record*)
let generate_struct_layout (word_sz : int) (must_be_aligned : bool) 
  (fields: (string * Types.t) list) : (int Symbols.SymbolTable.t * int)= 
  let rec logic cur_off fields : int Symbols.SymbolTable.t * int = 
    match fields with 
    | [] -> (Symbols.SymbolTable.empty, cur_off)
    | (name, typ) :: rest -> 
      if must_be_aligned then
        let rest_offsets, sz = logic (cur_off + word_sz) rest in
        Symbols.SymbolTable.add name (cur_off) rest_offsets,sz
      else
        let (next_slot, cur_slot) = 
        (match typ with
        | Types.Bool -> 
          (*This is the only case where we might not want to preserve alignment*)
          (cur_off + 1, cur_off)
        | __ ->
          (*Everything else is word-sized*)
          (*In case this is misaligned*)
          if (cur_off mod word_sz <> 0) then
            let cur_slot = 
              ((cur_off / word_sz) * word_sz) + word_sz 
            in
            let next_slot = 
              cur_slot + word_sz
            in
            (next_slot, cur_slot)
          else
            (cur_off + word_sz, cur_off))
        in
        let (offsets, size) = logic next_slot rest in
        offsets |> Symbols.SymbolTable.add name cur_slot,
        size
  in
  logic 0 fields

let get_field_type (field : string) (fields : (string * Types.t) list) = 
  List.find (fun (s, _) -> s = field) fields |> snd  

let all_mir : Mir.stmt list list ref = ref []

let size_of_type (word_size : int) (typ : Types.t) = 
  match typ with
  | Bool -> 1
  | _ -> word_size

type types = Types.env

module Renamings = struct
  type t = int Symbols.SymbolTable.t

  (*Gets current name, also returns updated naming envrionment *)
  let get_current_name (var_name : string) (renamings : t) =
    match (Symbols.SymbolTable.find_opt var_name renamings) with
    | Some counter ->
      var_name ^ (string_of_int counter), renamings
    | _ -> (*should never happen*)
      failwith "used variable before declaration"

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

let if_counter = ref 0
let while_counter = ref 0  
let func_counter = ref 0

let is_expr_ftmlk (expr : Ast.expr) = 
  match expr with
  | Ftmlk _ -> true
  | _ -> false 

let rec stat_to_mir 
  (must_be_aligned : bool) 
  (word_size : int) 
  (loop_end : Labels.t option)  
  (renamings : Renamings.t) 
  (ast : Ast.stmt)  = 
  let size_of_type = size_of_type word_size in 
  let expr_to_mir = expr_to_mir must_be_aligned word_size loop_end in
  let reg_stm = stat_to_mir must_be_aligned word_size loop_end in
  match ast with
  | While (cond, stmt) ->
    let (cond_insts, cond_val, cond_renamings) = expr_to_mir renamings cond in
    let while_num = !while_counter |> string_of_int in
    while_counter := !while_counter + 1;
    let begin_label = 
      Labels.named_label (while_num ^ "WhileBegin") 
    in
    let end_label = 
      Labels.named_label (while_num ^ "WhileEnd")
    in
    let body_label = 
      Labels.named_label (while_num ^ "WhileBody")
    in
    let stmt_insts, _ = 
      stat_to_mir must_be_aligned word_size (Some end_label) cond_renamings stmt 
    in
    (Mir.(
      MakeLabel begin_label ::
      cond_insts @
      [Goto (body_label, Some (Value cond_val));
      Goto (end_label, None);
      MakeLabel (body_label)] @
      stmt_insts @
      [ Goto (begin_label, None);
      MakeLabel end_label]
    ) : Mir.stmt list), renamings
  | LetStmt (var_name, var_typ, init_expr, _, is_recursive) ->
    let (init_insts, init_val, _) = 
      if is_expr_ftmlk init_expr then
        let func_addr = 
          ftmlk_to_mir must_be_aligned word_size renamings init_expr (Some var_name) is_recursive
        in
          [], func_addr, renamings
      else
        expr_to_mir renamings init_expr 
    in
    let val_size = 
      (match var_typ with 
        | Bool -> 1
        | _ -> word_size
      )
    in
    let new_var, renamings = Renamings.create_new_name var_name renamings in
    Mir.(
      init_insts @
      [Assign (Var new_var, Value init_val, val_size)]
    ), renamings
  | Print (expr) ->
    let (expr_insts, expr_val, _) = expr_to_mir renamings expr in
    Mir.(
      expr_insts @ 
      [Call (None, 
            (Address (Labels.named_label "print")), 
            [expr_val])
      ]
    ), renamings
  | Assign (var_name, expr, typ) ->
    let (expr_insts, expr_val, _) = expr_to_mir renamings expr in
    let move_sz = size_of_type typ in 
    let var_name, _ = Renamings.get_current_name var_name renamings in
    Mir.(
      expr_insts @
      [ Assign (Var var_name, Value expr_val, move_sz)]
    ), renamings
  | IfUnit (cond, stmt) ->
    let (cond_insts, cond_val, cond_renamings) = expr_to_mir renamings cond in
    let stmt_insts, _ = reg_stm cond_renamings stmt in
    let if_num = !if_counter |> string_of_int in
    if_counter := !if_counter + 1;
    let true_label = 
      Labels.named_label (if_num ^ "IfUnitTrue") 
    in
    let end_label = 
      Labels.named_label (if_num ^ "IfUnitEnd") in 
    Mir.(
      cond_insts @
      [Goto (true_label, Some (Value cond_val)); 
      Goto (end_label, None);
      MakeLabel true_label] @
      stmt_insts @ 
      [MakeLabel end_label]
    ), renamings
  | Seq (stmt1, stmt2) ->
    let (stmt1_insts, renamings) = reg_stm renamings stmt1 in
    let (stmt2_insts, renamings) = reg_stm renamings stmt2 in
      stmt1_insts @ stmt2_insts, renamings
  | Break -> 
    (match loop_end with
    | Some loop_end ->
      [Goto (loop_end, None)], renamings
    | None ->
      failwith "Attempt to break without being inside a loop")
  | Nothing -> [], renamings

and expr_to_mir 
  (must_be_aligned : bool) 
  (word_size : int) 
  (loop_end : Labels.t option) 
  (renamings : Renamings.t)
  (ast : Ast.expr) 
   = 
  let size_of_type = size_of_type word_size in
  let reg_etm = expr_to_mir must_be_aligned word_size loop_end in
  let lookup = Symbols.SymbolTable.find in
  match ast with
  | MemberOf (expr, var, typ) ->
    let (expr_insts, expr_val, _) = reg_etm renamings expr in
    let fields = 
      (match typ with
      | Record fields -> fields
      | _ -> failwith "Attempt to dereference non-types"
      ) 
    in
    let offset = 
      generate_struct_layout word_size must_be_aligned fields 
      |> fst
      |> Symbols.SymbolTable.find var
    in
    let res = Regs.new_temp () in
    let calculate_address = 
      Mir.(Operation (expr_val, Plus, Const offset))
    in
    let size_of_type = 
      get_field_type var fields |> size_of_type
    in
    Mir.(
      expr_insts @ 
      [AssignDeref (Temp res, calculate_address, size_of_type)]
    ), Lval (Temp res), renamings
  | Var var ->
    let var, _ = Renamings.get_current_name var renamings in
    [], (Lval (Var (var))), renamings
  | Num int -> 
    [], (Const int), renamings
  | Bool b -> 
    [], 
    (*May change in future*)
    (if b then 
      (Const 1)
    else
      (Const 0)),
    renamings
  | RecordExp fields ->
    let res = Regs.new_temp () in
    let offsets, sz = 
      fields
      |> List.map (fun (name, _, typ) -> (name, typ))
      |> generate_struct_layout word_size must_be_aligned 
    in
    let alloc_mem : Mir.stmt = 
      Call (
        Some (Temp res),
        (Address (Labels.named_label "gib_mem")),
        [(Const sz)]
        )
    in
    let initialize_fields = 
      let rec logic fields : Mir.stmt list= 
        (match fields with
        | (field_name, expr, typ) :: rest -> 
          let (expr_insts, expr_val, _) = reg_etm renamings expr in
          let field_offset = lookup field_name offsets in
          let field_addr = 
            Mir.Operation (Lval (Temp res), Plus, Const field_offset) 
          in
          expr_insts @
          [Mir.Store (field_addr, Value expr_val, size_of_type typ)] @
          logic rest
        | [] -> [])
      in
        logic fields
    in
    alloc_mem :: initialize_fields, Lval (Temp res), renamings
  | If (cond, then_expr, else_expr, typ) ->
    let result_reg = Regs.new_temp () in
    let (cond_insts, cond_val, cond_renamings) = reg_etm renamings cond in
    let (then_insts, then_val, _) = reg_etm cond_renamings then_expr in
    let (else_insts, else_val, _) = reg_etm cond_renamings else_expr in
    let cur_if_num = !if_counter |> string_of_int in
    if_counter := !if_counter + 1;
    let then_label = 
      Labels.named_label (cur_if_num ^ "Then") 
    in
    let else_label = 
      Labels.named_label (cur_if_num ^ "Else") 
    in
    let end_label = 
      Labels.named_label (cur_if_num ^ "IfEnd") 
    in
    cond_insts @
    Mir.([Goto (then_label, Some (Value (cond_val))); 
    Goto (else_label, None)] @ 
    MakeLabel (then_label) ::
    then_insts @
    [
      Assign (Temp result_reg, Value then_val, size_of_type typ);
      Goto (end_label, None)
    ] @
    MakeLabel (else_label) ::
    else_insts @
    [ Assign (Temp result_reg, Value else_val, size_of_type typ);
      MakeLabel end_label]), Lval (Temp result_reg), renamings
  | Let (var_name, var_typ, init_expr, body_expr, _, is_recursive) ->
    let var_name, body_renamings = 
      Renamings.create_new_name var_name renamings 
    in
    let (init_insts, (init_val : Mir.value), _) = 
      if is_expr_ftmlk init_expr then
        [], 
         
          (ftmlk_to_mir must_be_aligned word_size renamings init_expr (Some var_name) is_recursive),
        renamings
      else
        reg_etm renamings init_expr
    in
    let (body_insts, body_val, _) = 
      reg_etm body_renamings body_expr
    in
      init_insts @
      [Mir.Assign (Var var_name, (Value init_val), size_of_type var_typ)] @
      body_insts, body_val, renamings
  | BinOp (left_expr, op, right_expr, typ) ->
    let (left_insts, left_val, _) = reg_etm renamings left_expr in
    let (right_insts, right_val, _) = reg_etm renamings right_expr in
    let result = Regs.new_temp () in
    left_insts @
    right_insts @
    [Mir.Assign (Temp result, Operation (left_val, op, right_val), size_of_type typ)],
    (Lval (Temp result)),
    renamings
  | ESeq (stmt, expr) ->
    let (stmt_insts, stmt_renamings) = 
      stat_to_mir must_be_aligned word_size loop_end renamings stmt
    in
    let (expr_insts, expr_val, _) = reg_etm stmt_renamings expr in
    stmt_insts @ expr_insts, expr_val, renamings
  | Ftmlk _ ->
    let func_addr = 
      ftmlk_to_mir must_be_aligned word_size renamings ast None false
    in
    ([], func_addr, renamings)
  | FtmlkApp (func, args) ->
    let process_args (arg_insts, arg_vals) arg = 
      let (arg_inst, arg_val, _) = reg_etm renamings arg in
      (arg_insts @ arg_inst, arg_vals @ [arg_val])
    in
    let 
      (arg_insts, arg_vals) = List.fold_left process_args ([], []) args
    in
    let result = Regs.new_temp () in
    let (func_insts, func_val, _) = reg_etm renamings func in
    Mir.(
      arg_insts @
      func_insts @
      [Call ((Some (Temp result)), func_val, arg_vals)]
    ), Lval (Temp result), renamings

and ftmlk_to_mir 
  (must_be_aligned : bool) 
  (word_size : int) 
  (renamings : Renamings.t)
  (ftmlk_expr : Ast.expr)
  (func_name : string option)
  (is_recursive : bool) =
  (*At this point there should be no closures. All
  accesses to variables from enclosing functions will go through
  the static link*)
  let (args, body) = 
    match ftmlk_expr with
    | Ftmlk (a, b) -> (a, b)
    | _ -> failwith "Called ftmlk_to_mir on non-ftmlk"
  in
  let process_args ((insts : Mir.stmt list), (renamings : Renamings.t) )
    ((var_name, _, _) : string * Types.t * bool ref) = 
    let var_name, renamings = Renamings.create_new_name var_name renamings in
    (insts @ [Mir.Arg var_name], renamings)
  in
  let func_num = !func_counter |> string_of_int in 
  func_counter := !func_counter + 1;
  let func_label = Labels.named_label (
    match func_name with
    | Some func_name -> func_num ^ "func_" ^ func_name
    | None -> func_num ^ "F") in
  let (arg_insts, func_renamings) = 
    let (arg_insts, arg_renamings) = 
      List.fold_left process_args ([], renamings) args
    in
    if is_recursive then
      let (func_name, func_renamings) =
        Renamings.create_new_name (
          match func_name with 
          | Some f_n -> f_n
          | None -> failwith "Anonymous function can't be recursive"
        )
        arg_renamings
      in
        Mir.(Assign (Var func_name, Value (Address func_label), word_size)) ::
        arg_insts, func_renamings
    else
      (arg_insts, arg_renamings)
  in
  let (func_body, func_val, _) = 
    expr_to_mir must_be_aligned word_size None func_renamings body
  in
  let func_insts = 
    Mir.MakeLabel func_label ::
    arg_insts @ 
    func_body @
    [Mir.Return func_val]
  in
  all_mir := func_insts :: !all_mir;
  (Mir.Address func_label)

let lower (must_be_aligned : bool) (word_size : int) 
  (ast : Ast.stmt) : Mir.stmt list list = 
  let (main, _) = stat_to_mir must_be_aligned word_size None
  Renamings.empty ast in
  ((Mir.MakeLabel (Labels.named_label "main") :: main @ [Mir.Return (Const 0)]) :: !all_mir)
