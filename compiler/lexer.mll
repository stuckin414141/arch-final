{
    open Grammar
  let next_line (lexbuf : Lexing.lexbuf) =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
    { pos with 
               pos_lnum = pos.pos_lnum + 1;
               pos_cnum = 0;
    }
  let add_col (_ : Lexing.lexbuf) _ : unit = ()
  let inc_col (lexbuf : Lexing.lexbuf) = add_col lexbuf 1
}

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let whitespace = (' '|'\t')

rule token = parse
| '\n' {next_line lexbuf; token lexbuf}
| whitespace+ {token lexbuf}
| "//" {comment lexbuf}
| "int" {INT}
| "bool" {BOOL}
| "char" {CHAR}
| "long" {LONG}
| "short" {SHORT}
| "print" {PRINT}
| "while" {WHILE}
| "if" {IF}
| "then" {THEN}
| "break" {BREAK}
| "ftmlk" {FTMLK}
| "else" {ELSE}
| "let" {LET}
| "in" {IN}
| ":=" {ASSIGN}
| "!=" {NEQ}
| "==" {EQ}
| "<<" {SHL}
| ">>" {SHR}
| "<=" {LE}
| ">=" {GE}
| "->" {ARROW}
| "&&" {AND}
| "||" {OR}
| ";" {SEMICOLON}
| "," {COMMA}
| ":" {COLON}
| "(" {LPAREN}
| ")" {RPAREN}
| "{" {LBRACE}
| "}" {RBRACE}
| "+" {PLUS}
| "-" {MINUS}
| "*" {TIMES}
| "%" {MODULO}
| "/" {DIVIDE}
| "=" {EQ}
| "!=" {NEQ}
| "<" {LT}
| ">" {GT}
| "&" {BAND}
| "|" {BOR}
| "^" {BXOR}
| upper|lower (digit|upper|lower|"_")* {ID (Lexing.lexeme lexbuf)}
| digit+ {NUM (int_of_string (Lexing.lexeme lexbuf))}
| eof {EOF}

and comment = parse
| '\n' {next_line lexbuf; token lexbuf}
| _ {comment lexbuf}
| eof {EOF}