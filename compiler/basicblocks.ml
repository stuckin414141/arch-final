(*We now build basic blocks*)

type t = Mir.stmt list * Labels.t list * Labels.t list

let stmts_begin_uncond (stmts : Mir.stmt list) = 
  match stmts with
  | stmt1 :: _ ->
    (match stmt1 with
    | Goto (_, None) -> true
    | _ -> false)
  | [] -> false

  let stmts_begin_label (stmts : Mir.stmt list) = 
    match stmts with
    | stmt1 :: _ ->
      (match stmt1 with
      | MakeLabel _ -> true
      | _ -> false)
    | [] -> false

    let insert_uncond_jump (stmts : Mir.stmt list) = 
      let lab = Labels.new_label () in
      Mir.(
        [Goto (lab, None);
        MakeLabel lab] @
        stmts
      )
  
let stmt_to_basic_block (stmts : Mir.stmt list) : t list = 
  let all_blocks : t list ref = ref [] in
  let rec logic (stmts : Mir.stmt list) (cur : t) : unit = 
    let (cur_stmts, cur_succ, cur_pred) = cur in
    match stmts with
    | [] -> ()
    | stmt1 :: rest ->
      (match stmt1 with
      | Goto (lab, is_cond) ->
        (match is_cond with
        | Some _ ->
          let cur_with_stmt1 = (cur_stmts @ [stmt1], lab :: cur_succ, cur_pred) in
          if (stmts_begin_uncond rest) then
            (* we leave it for the next to deal with*)
            logic rest cur_with_stmt1
          else
            (*Create a new label / unconditional jump *)
            logic (insert_uncond_jump rest) cur_with_stmt1
        | None -> 
            (*We have an unconditional jump*)
            (*Check that the next statement is a label and then tie this off*)
            if stmts_begin_label rest then 
              all_blocks := (cur_stmts @ [stmt1], lab :: cur_succ, cur_pred) :: !all_blocks;
              logic rest ([], [], [])
          )
      | _ -> logic rest (cur_stmts @ [stmt1], cur_succ, cur_pred)
      )
  in
  logic stmts ([], [], []);
  !all_blocks