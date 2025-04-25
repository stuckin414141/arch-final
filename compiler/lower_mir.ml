(*Fresh renamings --- this will be a map from variable names
to the current variable name that should be used*)

type var_symb = string * int
type renamings = var_symb Symbols.SymbolTable.t

(*List of all statements that we wish to generate*)
let lowered_funcs  = ref []

let get_fresh_naming var renamings : (var_symb * renamings) =
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
    let outer_sl : string = Symbols.SymbolTable.find "SL" renamings |> string_of_var_symb in
    go desired_depth depth (Mir.Var outer_sl)

(*First is depth, second is closure size*)
type func = int * int

(*Word size, in bytes*)
let word_size = 8

let insts (inst, _, _) = inst
let rec lower_stmt func loop_end ast renamings : (Mir.stmt list * renamings * func) =
    let (depth, closure_offset) = func in
    let write_closure_val = write_closure_val depth
    in
  match ast with
  | Ast.While (cond, body) ->
    let start = Labels.new_label () in
    let end_label = Labels.new_label () in
    let (cond_inst, cond_val, _) = 
      lower_expr func loop_end cond renamings 
    in
    Mir.(
      [MakeLabel start] @
      cond_inst @
      [If (cond_val, Goto end_label)] @
      (lower_stmt func end_label body renamings |> insts) @
      [MakeLabel end_label]
    ), renamings, func
  | Ast.LetStmt (var_name, _, init_expr, closure, recursive) ->
    let (fresh_symb, updated_renamings) = 
      get_fresh_naming var_name renamings 
    in 
    let (init_inst, init_val, _) = 
      if recursive then
        lower_expr func loop_end init_expr updated_renamings
      else
        lower_expr func loop_end init_expr renamings
    in
    let updated_closure_offset = closure_offset + word_size in
    if !closure then
        init_inst @ (write_closure_val depth (closure_offset) init_val renamings), 
        updated_renamings, (depth, updated_closure_offset)
    else
    Mir.(
      init_inst @
      [Assign (Var (fresh_symb |> string_of_var_symb), init_val)]
    ), updated_renamings, func
  | _ -> failwith "unrecognized"
and lower_expr func loop_end ast renamings : Mir.stmt list * Mir.rval * func = 
  match ast with
  (*TODO: argument order may be wrong*)
  | Ast.FtmlkApp (func_expr, args) ->
    (*Function expression, then each of the arguments will be evaluated in order
    * Non-functional programmers beware!
    *)
    let (func_inst, func_val, _) = 
        lower_expr func loop_end func_expr renamings
    in
    let process_arg ((cur_inst, cur_arg_val) : Mir.stmt list * Mir.rval list) arg = 
        let (arg_inst, arg_val, _) = 
            lower_expr func loop_end arg renamings
        in
        (arg_inst @ cur_inst, arg_val :: cur_arg_val)
    in
    let (init_insts, arg_vals) = 
        List.fold_left process_arg (func_inst, []) args
    in
    let sl_rval = Mir.(Val (Lval (Var "SL"))) in
    let res_reg = Regs.new_temp () in
        Mir.(
            init_insts @
            [Call ((Some (Temp res_reg)), func_val, sl_rval :: arg_vals)]
        ), Val (Lval (Temp res_reg)), func
    | Ast.Ftmlk (args, body) ->
        let insert_args renamings arg = 
            let (_, updated_renamings) = 
                get_fresh_naming arg renamings
            in
                updated_renamings
        in
        (*We insert the environment argument*)
        let args = ("env", Types.Unit, ref false) :: args in
        let func_renaming_env = 
            let get_arg_name (s, _, _) = s in
            List.fold_left insert_args renamings (List.map get_arg_name args)
        in
        let (depth, _) = func in
        let (lowered_body, func_val, _) = 
            lower_expr (depth+1, word_size) "" body func_renaming_env
        in
        let func_address = 
            Labels.new_label ()
        in
        let full_body = 
            let func_sl = 
                Symbols.SymbolTable.find "SL" func_renaming_env in
            let func_env = 
                Symbols.SymbolTable.find "env" func_renaming_env in
            Mir.(
                [Mir.MakeLabel func_address;
                (*TODO: external_call to allocate memory for the static link*)
                Store (Var (func_sl |> string_of_var_symb), 
                Lval (Var (func_env |> string_of_var_symb)))] @
                lowered_body @
                [Mir.Return func_val]
            )
        in
        lowered_funcs := full_body :: !lowered_funcs;
        let closure_ptr = Regs.new_temp () in
        let env_slot = Regs.new_temp () in
        let make_func_val = 
        Mir.(
            [
                (*TODO: allocate closure*)
                Store (Temp closure_ptr, Address func_address);
                Assign (Temp env_slot, 
                    Operation (Lval (Temp closure_ptr), Ast.Plus, Const 8));
                Store (Temp env_slot, Lval (Var "SL"))
            ]
        )
        in
        make_func_val, Val (Lval ( Temp closure_ptr)), func
  | _ -> failwith "unrecognized"