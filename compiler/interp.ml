(*All temps/variables will be the size of words*)
type var_env = int Array.t Symbols.SymbolTable.t
module TempEnv = Map.Make(Int)
type temp_env = int Array.t TempEnv.t
(* We will be storing where all the labels point to*)
(* since our labels are just strings this works quite well*)
type address_table = Mir.stmt list TempEnv.t
type label_table = int Symbols.SymbolTable.t
type addr_to_label = string TempEnv.t

type reserved_func = Print | MoreMem | Nope



(*Least significant byte at index 0 will be the largest*)
(*This more-or-less does 255-byte base conversions*)
module WordInteger = struct
  type t = int list
  let word_size = 8

  let num_to_word (num : int) : t = 
    let rec logic (num : int) (place : int) : t = 
      if place < 0 then
        []
      else
        let remainder = num mod 255 in
        remainder :: (logic (num / 255) (place - 1))
    in
    logic num 7

  let word_to_num (word : t) : int =
    let rec logic (word : t) (place : int) : int =
        let place_val = 255. ** (float_of_int place) |> int_of_float in
        match word with
        | byt :: rest ->
          byt * place_val + (logic rest (place + 1))
        | [] -> 0
    in
    logic word 0

  let num_to_arr (num : int) = num |> num_to_word |> Array.of_list
  let arr_to_num (arr : int Array.t) = arr |> Array.to_list |> word_to_num
    
end

let is_reserved_func (value : Mir.value) : reserved_func = 
  match value with
  | Address name ->
      if name = "print" then 
        Print
      else if name = "gib_mem" then 
        MoreMem
      else
        Nope
  | _ -> Nope

(*Written by claude. *)
(* Evaluates a binary operation on two integer values *)
let eval_binop (left: int) (op: Ast.ast_binop) (right: int) : int =
  match op with
  (* Arithmetic operations *)
  | Ast.Plus -> left + right
  | Ast.Minus -> left - right
  | Ast.Times -> left * right
  | Ast.Div -> 
      if right = 0 then failwith "Division by zero"
      else left / right
  | Ast.Mod -> 
      if right = 0 then failwith "Modulo by zero"
      else left mod right
  
  (* Logical operations - treating 0 as false, non-zero as true *)
  | Ast.And -> 
      if left <> 0 && right <> 0 then 1 else 0
  | Ast.Or -> 
      if left <> 0 || right <> 0 then 1 else 0
  
  (* Comparison operations *)
  | Ast.Eq -> if left = right then 1 else 0
  | Ast.Neq -> if left <> right then 1 else 0
  | Ast.Lt -> if left < right then 1 else 0
  | Ast.Gt -> if left > right then 1 else 0
  | Ast.Leq -> if left <= right then 1 else 0
  | Ast.Geq -> if left >= right then 1 else 0
  
  (* Bitwise operations *)
  | Ast.Shl -> left lsl right
  | Ast.Shr -> left lsr right
  | Ast.BAnd -> left land right
  | Ast.BOr -> left lor right
  | Ast.BXor -> left lxor right

let get_block_label (stmts : Mir.stmt list) : Labels.t = 
  match stmts with
  | head_stmt :: _ ->
    (match head_stmt with
    | MakeLabel lab -> lab
    | _ -> failwith "Basic block should begin with label")
  | _ -> failwith "Basic block shouldn't be empty"

(*Since addresses are also numbers on real hardware, we build two jump tables*)
(* The first goes from indices/numbers to addresses*)
(* The second goes from labels to indices/numbres *)
let cur_label_num = ref 0
let rec build_jump_table (bb : Basicblocks.t list) : 
address_table * label_table * addr_to_label = 
  match bb with
  | (stmts, _, _) :: bb ->
    let lab = get_block_label stmts in 
    let cur_num = !cur_label_num in 
    cur_label_num := !cur_label_num + 1;
    let (address_table, label_table, addr_to_label) = build_jump_table bb in
    (TempEnv.add cur_num stmts address_table,
    Symbols.SymbolTable.add lab cur_num label_table,
    TempEnv.add cur_num lab addr_to_label
    )
  | [] -> (TempEnv.empty, Symbols.SymbolTable.empty, TempEnv.empty)

let interpreter (blocks : Basicblocks.t list) = 
  let (address_table, label_table, _) = build_jump_table blocks in
  let get_stmts_of_lab lab = 
    let lab_addr = Symbols.SymbolTable.find lab label_table in
    TempEnv.find lab_addr address_table
  in
  let main_stmts = get_stmts_of_lab "main" in
  let next_avail_addr = ref 0 in 
  let memory = Array.make 10000 0 in
  let return_slot : int Array.t option ref = ref None in
  let eval_lval (vars : var_env) (temps : temp_env) (lval : Mir.lval) = 
    match lval with
    | Var v ->
      let value = Symbols.SymbolTable.find v vars in 
      value
    | Temp t -> 
      let value = TempEnv.find t temps in
      value
  in
  let write_to_lval (env) (lval : Mir.lval) (src_val) = 
    let (vars, temps, args) = env in
    match lval with
    | Var v ->
      (Symbols.SymbolTable.add v src_val vars, temps, args)
    | Temp t ->
      (vars, TempEnv.add t src_val temps, args)
  in
  let eval_val (vars : var_env) (temps : temp_env) (value : Mir.value) = 
      match value with
      | Lval lval -> eval_lval vars temps lval
      | Const (num) -> num |> WordInteger.num_to_arr
      | Address (lab) ->
        Symbols.SymbolTable.find lab label_table |> WordInteger.num_to_arr
  in
  let eval_expr (vars : var_env) (temps : temp_env) (expr : Mir.expr) = 
    match expr with
      | Operation (val1, op, val2) ->  
        let num1 = eval_val vars temps val1 |> WordInteger.arr_to_num in
        let num2 = eval_val vars temps val2 |> WordInteger.arr_to_num in
        let result = eval_binop num1  op num2 in
        result |> WordInteger.num_to_arr
      | Value v -> eval_val vars temps v
      | Not v -> 
        let value = (eval_val vars temps v) |> WordInteger.arr_to_num in
        (if value = 1 then 0 else 1) |> WordInteger.num_to_arr

  in
  let rec interpret (env : var_env * temp_env * (int Array.t list)) (stmts : Mir.stmt list) = 
    let (vars, temps, args) = env in
    match stmts with
    | stmt :: other_stmts ->
      (match stmt with
      | Arg var ->
        let arg_val, rest_args = 
          (match args with 
          | arg :: rest_args -> arg, rest_args
          | [] -> failwith ("Not enough arguments, stopped at " ^ var))
        in
        let updated_env = 
          (Symbols.SymbolTable.add var arg_val vars, temps, rest_args)
        in
        interpret updated_env other_stmts
      | Goto (target, cond) ->
        (match cond with
        | Some c ->
          let cond_val = eval_expr vars temps c |> WordInteger.arr_to_num in
          if cond_val = 1 then
            interpret env (target |> get_stmts_of_lab)
          else
            interpret env other_stmts
        | None -> 
          interpret env (target |> get_stmts_of_lab))
      | Assign (lval, expr, _) ->
        let src_val = eval_expr vars temps expr in
        let modified_env = write_to_lval env lval src_val in
        interpret modified_env other_stmts
      | AssignDeref (lval, expr, sz) ->
        let src_addr = eval_expr vars temps expr |> WordInteger.arr_to_num in
        let src_val = Array.make sz 0 in 
        for i = 0 to sz - 1 do 
          Array.set src_val i (Array.get memory (src_addr + i))
        done;
        let env = 
          (match lval with 
          | Var v ->
            (Symbols.SymbolTable.add v src_val vars, temps, args)
          | Temp t ->
            (vars, TempEnv.add t src_val temps, args))
        in
        interpret env other_stmts
      | Store (dest, src, sz) ->
        let dest_addr = eval_expr vars temps dest |> WordInteger.arr_to_num in
        let src_val = eval_expr vars temps src in
        for i = 0 to sz - 1 do 
          Array.set memory (dest_addr + i) (Array.get src_val i)
        done;
        interpret env other_stmts
      | Call (recv_lval, loc, call_args) ->
        let handle_print (_ : unit) = 
            print_endline (call_args |> List.hd |> eval_val vars temps |> 
            WordInteger.arr_to_num |> string_of_int);
          interpret env other_stmts
        in
        let handle_more_mem (_ : unit) = 
            let sz_requested = 
              call_args |> List.hd |> eval_val vars temps |> WordInteger.arr_to_num in
            let addr = !next_avail_addr in
            next_avail_addr := !next_avail_addr + sz_requested;
            addr
        in
        let args_to_pass = 
            List.fold_right 
              (fun arg args -> eval_val vars temps arg :: args)
              call_args
              []
        in
        (match is_reserved_func loc with
        | Print -> handle_print ()
        | MoreMem ->
            let mem_slot = handle_more_mem () in
            (match recv_lval with
            | Some lval ->
              let modified_env = 
                write_to_lval env lval (mem_slot |> WordInteger.num_to_arr)
              in
              interpret modified_env other_stmts
            | None -> 
              interpret env other_stmts)
        | Nope ->
          let func_addr = 
            loc |>
            eval_val vars temps |>
            WordInteger.arr_to_num 
          in
          let func_stmts = TempEnv.find func_addr address_table in
          let old_ret_slot = !return_slot in
          return_slot := None;
          interpret (vars, temps, args_to_pass) func_stmts;
          (match recv_lval with 
          | Some lval ->
            let retval = (
              match !return_slot with
              | Some rval -> rval
              | None -> failwith "Function expected to return value"
            )
            in
            return_slot := old_ret_slot;
            let modified_env = write_to_lval env lval retval in
            interpret modified_env other_stmts
          | None -> interpret env other_stmts)
          )
      | MakeLabel _ -> interpret env other_stmts
      | Return rval ->
        return_slot := Some (rval |> eval_val vars temps);
      )
    | [] -> 
      print_endline "Ftmlk.";
      ()
  in
  interpret (Symbols.SymbolTable.empty, TempEnv.empty, []) main_stmts