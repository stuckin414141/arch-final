(*Fresh renamings --- this will be a map from variable names
to the current variable name that should be used*)

type var_symb = string * int
type renamings = var_symb Symbols.SymbolTable.t

let get_fresh_renaming var renamings : (var_symb * renamings) =
  match Symbols.SymbolTable.find_opt var renamings with
  | Some (_, cur_num) ->
    let new_num = cur_num + 1 in
    (var, new_num), (Symbols.SymbolTable.add var (var, new_num) renamings)
  | None ->
    (var, 0), (Symbols.SymbolTable.add var (var, 0) renamings)

let string_of_var_symb ((str, num) : var_symb) : string = 
  str ^ (string_of_int num)

(*The argument passed in will be named env, the thing we declare
is going to be called SL*)
let write_closure_val depth desired_depth offset rval renamings =
let rec go desired_depth depth (cur_sl : Mir.lval) : Mir.stmt list=
  if desired_depth = depth then
    let sl_loc = Regs.new_temp () in
    let rval_temp = Regs.new_temp () in
    Mir.(
      [ Assign (Temp sl_loc, Operation (Lval cur_sl, Ast.Plus, Const offset));
        Assign (Temp rval_temp, rval);
        Store (Temp sl_loc, Lval (Temp rval_temp))]
    )
  else
    let next_sl = Regs.new_temp () in
    Mir.(
      Assign (Temp next_sl, Deref cur_sl) ::
      go desired_depth (depth - 1) (Temp next_sl)
    )
  in
    let outer_sl : string = 
      get_fresh_renaming "SL" renamings |> fst |> string_of_var_symb
    in
      go desired_depth depth (Mir.Var outer_sl)

(*First is depth, second is closure size*)
type func = int * int
(*TODO: add some sort of closure size/current offset field*)
(*TODO: the closure*)
let insts (inst, _, _) = inst
let rec lower_stmt func loop_end ast renamings : (Mir.stmt list * renamings * func) =
    let (depth, closure_offset) = func in
    let write_closure_val = write_closure_val depth
    in
  match ast with
  | Ast.While (cond, body) ->
    let start = Labels.new_label () in
    let end_label = Labels.new_label () in
    let (cond_inst, cond_val) = 
      lower_expr func loop_end cond renamings 
    in
    Mir.(
      [Label start] @
      cond_inst @
      [If (cond_val, Goto end_label)] @
      (lower_stmt func end_label body renamings |> insts) @
      [Label end_label]
    ), renamings, func
  | Ast.LetStmt (var_name, _, init_expr, closure, recursive) ->
    let (fresh_symb, updated_renamings) = 
      get_fresh_renaming var_name renamings 
    in 
    let (init_inst, init_val) = 
      if recursive then
        lower_expr func loop_end init_expr updated_renamings
      else
        lower_expr func loop_end init_expr renamings
    in
    (*TODO: change to be word_size*)
    let updated_closure_offset = closure_offset + 8 in
    if !closure then
        init_inst @ (write_closure_val depth (closure_offset) init_val renamings), 
        updated_renamings, (depth, updated_closure_offset)
    else
    Mir.(
      init_inst @
      [Assign (Var (fresh_symb |> string_of_var_symb), init_val)]
    ), updated_renamings, func
  | _ -> failwith "unrecognized"

and lower_expr _ _ ast _ : Mir.stmt list * Mir.rval = 
  match ast with
  | _ -> failwith "unrecognized"