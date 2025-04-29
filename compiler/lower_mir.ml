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

let get_offset_of_field  (word_sz : int) (must_be_aligned : bool) 
(record_type : Types.t) (field_name : string) : int = 
  match record_type with 
  | Record fields ->
    let layout, _ = 
      generate_struct_layout word_sz must_be_aligned fields
    in
    Symbols.SymbolTable.find field_name layout
  | _ -> failwith "Not a record type"


let all_mir : Mir.stmt list list ref = ref []

let size_of_type (word_size : int) (typ : Types.t) = 
  match typ with
  | Bool -> 1
  | _ -> word_size

type types = Types.env

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
  (ast : Ast.stmt)  = 
  let size_of_type = size_of_type word_size in 
  let expr_to_mir = expr_to_mir must_be_aligned word_size loop_end in
  let reg_stm = stat_to_mir must_be_aligned word_size loop_end in
  match ast with
  | While (cond, stmt) ->
    let (cond_insts, cond_val) = expr_to_mir cond in
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
    let stmt_insts = 
      stat_to_mir must_be_aligned word_size (Some end_label) stmt 
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
    ) : Mir.stmt list)
  | LetStmt (var_name, var_typ, init_expr, _, is_recursive) ->
    let (init_insts, init_val) = 
      if is_expr_ftmlk init_expr then
        let func_addr = 
          ftmlk_to_mir must_be_aligned word_size init_expr (Some var_name) is_recursive
        in
          [], func_addr
      else
        expr_to_mir init_expr 
    in
    let val_size = 
      (match var_typ with 
        | Bool -> 1
        | _ -> word_size
      )
    in
    Mir.(
      init_insts @
      [Assign (Var var_name, Value init_val, val_size)]
    )
  | Print (expr) ->
    let (expr_insts, expr_val) = expr_to_mir expr in
    Mir.(
      expr_insts @ 
      [Call (None, 
            (Address (Labels.named_label "print")), 
            [expr_val])
      ]
    )
  | Assign (target, expr, typ) ->
    let (expr_insts, expr_val) = expr_to_mir expr in
    let move_sz = size_of_type typ in 
    (match target with 
    | Var var_name -> 
      Mir.(
        expr_insts @
        [ Assign (Var var_name, Value expr_val, move_sz)])
    | MemberOf (record, field_name, record_typ) ->
      let (record_insts, record_val) = expr_to_mir record in
      let field_offset = 
        get_offset_of_field word_size must_be_aligned record_typ field_name 
      in
      let addr_of_field : Mir.expr = 
        Operation (record_val, Plus, Const field_offset)
      in
      Mir.(
        record_insts @
        (*TODO: this may cause undue interference*)
        expr_insts @
        [ Store (addr_of_field, Value expr_val, move_sz)]
      )
    | _ -> failwith "Attempt to assign to invalid type"
    )
  | IfUnit (cond, stmt) ->
    let (cond_insts, cond_val) = expr_to_mir cond in
    let stmt_insts = reg_stm stmt in
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
    )
  | Seq (stmt1, stmt2) ->
    let stmt1_insts = reg_stm stmt1 in
    let stmt2_insts = reg_stm stmt2 in
      stmt1_insts @ stmt2_insts
  | Break -> 
    (match loop_end with
    | Some loop_end ->
      [Goto (loop_end, None)]
    | None ->
      failwith "Attempt to break without being inside a loop")
  | Nothing -> []

and expr_to_mir 
  (must_be_aligned : bool) 
  (word_size : int) 
  (loop_end : Labels.t option) 
  (ast : Ast.expr) 
   = 
  let size_of_type = size_of_type word_size in
  let reg_etm = expr_to_mir must_be_aligned word_size loop_end in
  let lookup = Symbols.SymbolTable.find in
  match ast with
  | MemberOf (expr, var, typ) ->
    let (expr_insts, expr_val) = reg_etm expr in
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
    ), Lval (Temp res)
  | Var var ->
    [], (Lval (Var (var)))
  | Num int -> 
    [], (Const int)
  | Bool b -> 
    [], 
    (*May change in future*)
    (if b then 
      (Const 1)
    else
      (Const 0))
  | Nullptr ->
    [], (Const 0)
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
          let (expr_insts, expr_val) = reg_etm expr in
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
    alloc_mem :: initialize_fields, Lval (Temp res)
  | If (cond, then_expr, else_expr, typ) ->
    let result_reg = Regs.new_temp () in
    let (cond_insts, cond_val) = reg_etm cond in
    let (then_insts, then_val) = reg_etm then_expr in
    let (else_insts, else_val) = reg_etm else_expr in
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
      MakeLabel end_label]), Lval (Temp result_reg)
  | Let (var_name, var_typ, init_expr, body_expr, _, is_recursive) ->
    let (init_insts, (init_val : Mir.value)) = 
      if is_expr_ftmlk init_expr then
        [],
          (ftmlk_to_mir must_be_aligned word_size init_expr (Some var_name) is_recursive)
      else
        reg_etm init_expr
    in
    let (body_insts, body_val) = 
      reg_etm body_expr
    in
      init_insts @
      [Mir.Assign (Var var_name, (Value init_val), size_of_type var_typ)] @
      body_insts, body_val
  | BinOp (left_expr, op, right_expr, typ) ->
    let (left_insts, left_val) = reg_etm left_expr in
    let (right_insts, right_val) = reg_etm right_expr in
    let result = Regs.new_temp () in
    left_insts @
    right_insts @
    [Mir.Assign (Temp result, Operation (left_val, op, right_val), size_of_type typ)],
    (Lval (Temp result))
  | ESeq (stmt, expr) ->
    let (stmt_insts) = 
      stat_to_mir must_be_aligned word_size loop_end stmt
    in
    let (expr_insts, expr_val) = reg_etm expr in
    stmt_insts @ expr_insts, expr_val
  | Ftmlk _ ->
    let func_addr = 
      ftmlk_to_mir must_be_aligned word_size ast None false
    in
    ([], func_addr)
  | FtmlkApp (func, args, _) ->
    let process_args (arg_insts, arg_vals) arg = 
      let (arg_inst, arg_val) = reg_etm arg in
      (arg_insts @ arg_inst, arg_vals @ [arg_val])
    in
    let 
      (arg_insts, arg_vals) = List.fold_left process_args ([], []) args
    in
    let result = Regs.new_temp () in
    let (func_insts, func_val) = reg_etm func in
    Mir.(
      arg_insts @
      func_insts @
      [Call ((Some (Temp result)), func_val, arg_vals)]
    ), Lval (Temp result)

and ftmlk_to_mir 
  (must_be_aligned : bool) 
  (word_size : int) 
  (ftmlk_expr : Ast.expr)
  (func_name : string option)
  (is_recursive : bool) : Mir.value =
  (*At this point there should be no closures. All
  accesses to variables from enclosing functions will go through
  the static link*)
  let (args, body) = 
    match ftmlk_expr with
    | Ftmlk (a, b, _) -> (a, b)
    | _ -> failwith "Called ftmlk_to_mir on non-ftmlk"
  in
  let process_args (insts : Mir.stmt list)
    ((var_name, _, _) : string * Types.t * bool ref) = 
    insts @ [Mir.Arg var_name]
  in
  let func_num = !func_counter |> string_of_int in 
  func_counter := !func_counter + 1;
  let func_label = Labels.named_label (
    match func_name with
    | Some func_name -> func_num ^ "func_" ^ func_name
    | None -> func_num ^ "F") in
  let (arg_insts) = 
    let (arg_insts) = 
      List.fold_left process_args [] args
    in
    if is_recursive then
        Mir.(Assign (Var func_label, Value (Address func_label), word_size)) ::
        arg_insts
    else
      arg_insts
  in
  let (func_body, func_val) = 
    expr_to_mir must_be_aligned word_size None body
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
  let (main) = stat_to_mir must_be_aligned word_size None ast in
  ((Mir.MakeLabel (Labels.named_label "main") :: main @ [Mir.Return (Const 0)]) :: !all_mir)
