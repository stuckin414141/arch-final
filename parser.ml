
let test_parse filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  let untyped_ast = Compiler.Grammar.program Compiler.Lexer.token lexbuf in
  Compiler.Util.print_untyped_ast untyped_ast;
  let (_, _, ast) = Compiler.Typecheck.analyze untyped_ast in
  let _ = Compiler.Escape.analyze ast in
  Compiler.Util.print_ast_stmt 0 ast

let filename = Sys.argv.(1)
let () = test_parse filename