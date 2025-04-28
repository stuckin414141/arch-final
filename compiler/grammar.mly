%{
    open Untyped_ast
%}

%token EOF
%token <string> ID
%token <int> NUM
%token NULLPTR
%token COMMA COLON SEMICOLON LPAREN RPAREN LBRACE RBRACE
%token PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE BOR BAND BXOR SHL SHR
%token AND OR ASSIGN
%token IF THEN ELSE WHILE LET IN TYPE
%token DOT
%token BREAK
%token REC
%token TRUE FALSE
%token FTMLK
%token MODULO
%token PRINT
%token ARROW

%start program
%type<Untyped_ast.stmt> program

%left SEMICOLON
%nonassoc ELSE
%nonassoc THEN
%left ASSIGN
%nonassoc IN
%left OR
%left AND
%left BOR
%left BXOR
%left BAND
%nonassoc EQ NEQ LT LE GT GE
%left SHL SHR
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%nonassoc LPAREN
%left DOT
%right ARROW

%%

program:
    | stmt EOF { $1}
    | stmt SEMICOLON EOF { $1 }

stmt:
    | PRINT LPAREN expr RPAREN { Print($3)}
    | LET ID COLON type_parse EQ expr { LetStmt ($2, $4, $6, ref false, false)}
    | LET REC ID COLON type_parse EQ expr { LetStmt ($3, $5, $7, ref false, true)}
    | stmt SEMICOLON stmt {Seq($1, $3)}
    | WHILE expr LBRACE stmt RBRACE {While($2, $4)}
    | lvalue ASSIGN expr {Assign($1, $3)}
    | IF expr LBRACE stmt RBRACE { IfUnit($2, $4) }
    | TYPE ID EQ type_parse { TypeDecl ($2, $4)}
    | BREAK { Break }
expr:
    | LPAREN expr RPAREN { $2 }
    | lvalue { $1 }
    | TRUE { Bool(true) }
    | FALSE { Bool(false) }
    | NULLPTR { Nullptr }
    | expr LPAREN exprlist RPAREN { FtmlkApp($1, $3) }
    | NUM { Num($1) }
    | LET ID COLON type_parse EQ expr IN expr { Let($2, $4, $6, $8, ref false, false) }
    | LET REC ID COLON type_parse EQ expr IN expr { Let($3, $5, $7, $9, ref false, true) }
    | IF expr THEN expr ELSE expr { If($2, $4, $6) }
    |  expr PLUS expr { BinOp($1, Plus, $3) }
    |  expr MINUS expr { BinOp($1, Minus, $3) }
    |  expr TIMES expr { BinOp($1, Times, $3) }
    | expr DIVIDE expr { BinOp($1, Div, $3)}
    | expr MODULO expr { BinOp($1, Mod, $3)}
    | expr EQ expr { BinOp($1, Eq, $3) }
    | expr NEQ expr { BinOp($1, Neq, $3) }
    | expr LT expr { BinOp($1, Lt, $3) }
    | expr LE expr { BinOp($1, Leq, $3) }
    | expr GT expr { BinOp($1, Gt, $3) }
    | expr GE expr { BinOp($1, Geq, $3) }
    | expr AND expr { BinOp($1, And, $3) }
    | expr OR expr { BinOp($1, Or, $3) }
    | expr BOR expr { BinOp($1, BOr, $3) }
    | expr BAND expr { BinOp($1, BAnd, $3) }
    | expr BXOR expr { BinOp($1, BXor, $3) }
    | expr SHL expr { BinOp($1, Shl, $3) }
    | expr SHR expr { BinOp($1, Shr, $3) }
    | stmt SEMICOLON expr { ESeq($1, $3) }
    | LBRACE fieldlist RBRACE { RecordExp ($2) }
    | FTMLK LPAREN decllist RPAREN LBRACE expr RBRACE 
        { Ftmlk ($3, $6)} 

lvalue:
    | ID { Var($1) }
    | expr DOT ID {MemberOf ($1, $3)}

decllist:
    | { [] }
    | ID COLON type_parse { [($1, $3, ref false)] }
    | ID COLON type_parse COMMA decllist { (($1, $3, ref false) :: $5) }

exprlist:
    | expr { [$1] }
    | expr COMMA exprlist { $1 :: $3 }

fieldlist:
    | ID EQ expr { [($1, $3)]}
    | ID EQ expr SEMICOLON fieldlist { ($1, $3) :: $5}


record_type:
    | ID COLON type_parse {[($1, $3)]}
    | ID COLON type_parse SEMICOLON record_type {($1, $3) :: $5}

type_parse:
    | ID { Typ ($1) }
    | LBRACE record_type RBRACE { RecordType ($2)}
    | LPAREN type_parse RPAREN { $2 } 
    | type_parse ARROW type_parse { ArrowType ($1, $3)}