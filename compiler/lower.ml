
type var_alloc_location = IsEscaping | InClosure | Neither

(* What we expect from an architecture implementation *)
module type ArchFrame = sig 
  val word_size : int
  type func
  type var
  val new_func : Labels.t -> (bool * int) list -> func
  (*Takes in a function and some escaping information, returns a var *)
  val alloc_var : int -> func -> bool -> var * func

  val access_var : var -> Hir.expr

end

module X64Frame : ArchFrame = struct
    (*TODO: don't handle closures within frame. It's not architecture-specific*)
    type func = Labels.t * int * (bool * int) list
    type var = Reg of Regs.t | FrameVar of int

    let word_size = 8
    let frame_reg = Regs.new_temp ()
    let new_func (loc : Labels.t) (formals : (bool * int) list) : func = 
        let update_stack stack (loc, sz) = 
            if loc then
                stack + sz
            else
                stack
        in
        let updated_stack_size = 
            List.fold_left update_stack 8 formals
        in
        (loc, updated_stack_size, formals)
    let alloc_var (size : int) (func : func) (loc : bool) = 
        if loc then
            Reg (Regs.new_temp ()), func
        else
            let (loc, stac, formals) = func in
            let var_access = FrameVar stac in
            let updated_func = (loc, stac + size, formals) in
            (var_access, updated_func)
    let access_var var = 
        match var with
        | Reg r -> Hir.Reg r
        | FrameVar offset -> 
            Hir.(Deref 
                (Binop (
                    Reg frame_reg, 
                    Ast.Plus,
                    Const offset
                )))
            
end

(*
*frame creation cannot be divorced from lowering
* The general algorithm: we allocate space for variables as we go along
* immediately after processing a function, we append instructions to allocate the requisite space
* and then add it to an internal list of functions to process
* this also has the helpful upshot of (mostly) resolving closure conversions
*)
module LowerAst (Arch : ArchFrame) = struct
    (*parent function and current function*)
    type func = {parent: func option; func: Arch.func; closure_size: int}
    (*Enclosing function, access, if it's stored in closure + offset*)
    type var_loc = InClosure of int | InFunc of Arch.var
    type var = int * var_loc
    type value = Var of var | Func of func
    type value_env = value Symbols.SymbolTable.t
    type env = (func * value_env)

    let add_var_closure (({parent; func; closure_size} : func)) sz =
        let new_closure_size = closure_size + sz in
        (InClosure closure_size, 
        ({parent=parent; func=func; closure_size=new_closure_size} : func))
    
    let rec lower_stmt _ _ ast (_ : env) = 
        match ast with
        | _ -> failwith "not recognized"

    and lower_expr depth loop_end ast (env : env) = 
        (*This will be the static link declared by us/the current function*)
        let current_sl = 
            let (_, values) = env in 
            match Symbols.SymbolTable.find "SL" values with
            | Var (_, v) -> 
            (match v with 
            | InFunc var -> Arch.access_var var
            | InClosure _ -> failwith "Static link shouldn't be in closure"
            )
            | Func _ -> failwith "static link should be a variable"
        in
        let rec unwind_static_links (desired : int) (cur : int) = 
            if desired = cur then
                current_sl
            else
                Hir.Deref (unwind_static_links desired (cur - 1))
        in
        let access_var (var : var) = 
            let (var_depth, var_loc) = var in
            match var_loc with
            | InFunc var_loc ->
                Arch.access_var var_loc
            | InClosure offset ->
                Hir.(Deref (Binop (unwind_static_links var_depth depth, Ast.Plus, Const offset)))
        in
        match ast with
        | Ast.Var var_name ->
            let (_, old_env) = env in
            let var_wrapper = Symbols.SymbolTable.find var_name old_env in
            let var_access = (match var_wrapper with
            | Var var -> var
            | Func _ -> failwith "not a variable")
            in
            (access_var var_access, env)
        | Let (var_name, _, init_expr, body_expr, is_closure, _) ->
            if !is_closure then
                let ({parent; func; closure_size}, values) = env in
                let offset = closure_size in
                let var_loc = InClosure offset in
                let updated_values = 
                    Symbols.SymbolTable.add var_name (Var (depth, var_loc)) values
                in
                let var_size = Arch.word_size in
                let updated_func = 
                    {parent; func; closure_size=closure_size + var_size}
                in
                let (init_tree, (updated_func, _)) = 
                    (*the use of updated func here means that any internal 
                    declarations in the init expression that use stack/closure
                    space will not conflict with the closure of the outer variable*)
                    (*TODO: is this necessary ^ ? recursion might be the edge case*)
                    lower_expr depth loop_end init_expr (updated_func, values)
                in
                let (body_tree, (updated_func, _)) = 
                    lower_stmt depth loop_end body_expr (updated_func, updated_values)
                in
                let ir_tree = Hir.(
                    ESeq (
                        Store (
                            Binop(current_sl, Ast.Plus, Const offset),
                            init_tree
                        ),
                        body_tree
                    )
                )
                in
                ir_tree, (updated_func, values)
            else
                let ({parent; func; closure_size}, values) = env in
                let var_size = Arch.word_size in
                let (var_access, updated_func) = 
                    Arch.alloc_var var_size func false
                in
                let var_loc = InFunc var_access in
                let updated_values = 
                    Symbols.SymbolTable.add var_name (Var (depth, var_loc)) values
                in
                let (init_tree, (updated_func, _)) = 
                    lower_expr depth loop_end init_expr ({parent; func=updated_func; closure_size}, values)
                in
                let (body_tree, (updated_func, _)) = 
                    lower_stmt depth loop_end body_expr (updated_func, updated_values)
                in
                let ir_tree = Hir.(
                    ESeq (
                        Store (
                            access_var (depth, var_loc),
                            init_tree
                        ),
                        body_tree
                    )
                )
                in
                (*TODO: do we still have to pass updated func? (since 
                there is a guarantee that any variables declared within init
                or body will not be alive past the end of this function)*)
                ir_tree, (updated_func, values)
        | _ -> failwith "not recognized"
end

module type LowerAstSig = sig
    type value
    type func
    type value_env = value Symbols.SymbolTable.t
    type tree
    type env = (func * value_env)
    val lower_stmt : int -> Labels.t option -> Ast.stmt -> env -> tree * env
    val lower_expr : int -> Labels.t option -> Ast.expr -> env -> tree * env 
end