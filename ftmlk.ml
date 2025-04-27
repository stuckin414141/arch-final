let get_all_tokens src = 
  let lexbuf = Lexing.from_string src in
  let rec logic s = 
    let t = Compiler.Lexer.token lexbuf in
    let updated = (s ^ " " ^ Compiler.Util.string_of_token t ^ "\n") in
    if t = Compiler.Grammar.EOF then
      updated
    else
      logic updated
    in
    logic ""

let get_untyped_ast src =
        Lexing.from_string src |> 
        Compiler.Grammar.program Compiler.Lexer.token

open Core

let read_file (filename : string) : string = 
        try Stdio.In_channel.read_all filename with
        | _ -> print_endline "Error: file not found" ; exit 0
let filename_param = 
    let open Command.Param in
        anon ("filename" %: string)

let output filename append content = 
    let remove_option s = 
        match s with
        | Some h -> h
        | None -> failwith "Must specify a file"
    in
    let output_file = 
        match (
            filename |> 
            Stdlib.String.split_on_char '/' |>
            List.rev |> List.hd |> remove_option |>
            Stdlib.String.split_on_char '.' |> List.hd) with
        | Some f -> f ^ "." ^ append
        | None -> filename ^ "." ^ append
    in
    print_endline output_file;
    Stdio.Out_channel.output_string 
    (Stdio.Out_channel.create output_file ~fail_if_exists:false)
    content

let compile filename debug = 
    let output = output filename in
    let src = read_file filename in
    if debug then
        output "lex" (get_all_tokens src);
    let untyped_ast = get_untyped_ast src in
    if debug then
        output "ua" (Compiler.Util.string_of_untyped_ast untyped_ast);
    let (_, _, typed_ast) = Compiler.Typecheck.analyze untyped_ast in
    (if debug then
        output "ast" (Compiler.Util.string_of_ast typed_ast));
    typed_ast

let lex_cmd = 
    Command.basic
        ~summary:"Generate stream of tokens"
        (Command.Param.map filename_param ~f: 
        (fun filename () -> let _ = compile filename true in ()))

let () = Command_unix.run lex_cmd