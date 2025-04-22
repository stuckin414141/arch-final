
let test_lex filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  let rec do_it () =
    let t = Compiler.Lexer.token lexbuf in
    print_endline ((Compiler.Util.token_to_string t ) ^ "   ");
    if String.length (Compiler.Util.token_to_string t) = 3 && String.sub (Compiler.Util.token_to_string t) 0 3 = "EOF" then
      ()
    else
      do_it ()
  in
  do_it ();
  close_in file

let filename = Sys.argv.(1)
let () = test_lex filename