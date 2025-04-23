
type var_alloc_location = IsEscaping | InClosure | Neither

module Labels = struct
  type t = string
  let tempCount = ref 0
  let new_label () = 
    let l = !tempCount in
    tempCount := l + 1;
    "L" ^ string_of_int l
  let named_label name = name
end

module Regs = struct
    type t = int
    let regCount = ref 0
    let new_temp () = 
        let r = !regCount in
        regCount := r + 1;
        r
end

(* What we expect from an architecture implementation *)
module type ArchFrame = sig 
  val word_size : int
  type func
  type var
  val new_func : Labels.t -> (var_alloc_location * int) list -> func
  (*Takes in a function and some escaping information, returns a var *)
  val alloc_var : int -> func -> var_alloc_location -> var

end

module X86Frame = struct
    type func = Labels.t * int * int * (var_alloc_location * int) list
    type var = Reg of Regs.t | FrameVar of int | ClosureVar of int

    (*TODO: don't handle closures within frame. It's not architecture-specific*)

    let new_func (loc : Labels.t) (formals : (var_alloc_location * int) list) : func = 
        let update_stack stack (loc, sz) = 
            if loc = IsEscaping then
                stack + sz
            else
                stack
        in
        let updated_stack_size = 
            List.fold_left update_stack 8 formals
        in
        let update_heap heap (loc, sz) = 
            if loc = InClosure then
                heap + sz 
            else
                heap
        in
        let updated_heap_size = 
            List.fold_left update_heap 0 formals
        in
        (loc, updated_stack_size, updated_heap_size, formals)
    let alloc_var (size : int) (func : func) (loc : var_alloc_location) = 
        match loc with
        | Neither -> Reg (Regs.new_temp ()), func
        | InClosure ->
            let (loc, stac, closure, formals) = func in
            let var_access = ClosureVar closure in
            let updated_func = (loc, stac, closure + size, formals) in
            (var_access, updated_func)
        | IsEscaping -> 
            let (loc, stac, closure, formals) = func in
            let var_access = FrameVar stac in
            let updated_func = (loc, stac + size, closure, formals) in
            (var_access, updated_func)
end

(*
*frame creation cannot be divorced from lowering
* The general algorithm: we allocate space for variables as we go along
* immediately after processing a function, we append instructions to allocate the requisite space
* and then add it to an internal list of functions to process
* this also has the helpful upshot of (mostly) resolving closure conversions
*)
module LowerAst = struct
    (*Location of function, stack size, heap/closure size, list of formals + is escaping*)
    type func = Labels.t * int * int * (var_alloc_location * int) list
    type var_access = Reg of Regs.t | FrameVar of int | ClosureVar of int
    type value = Var of {enclosing: func; var: var_access} 
                    | Func of {parent : func option; func: func}
    (*TODO: since stack setup varies by arch, you probably need to put this in arch*)
    
end

module type LowerAstSig = sig
    type value
    type func
    type value_env = value Symbols.SymbolTable.t
    type tree
    type env = (func * value_env)
    val lower_stmt : Ast.stmt -> env -> tree * env
    val lower_expr : Ast.expr -> env -> tree * env 
end