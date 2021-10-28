type token =
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | ASSIGN
  | PLUSASSIGN
  | MINUSASSIGN
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | REFEQ
  | NOT
  | AND
  | OR
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | COMMA
  | SEMI
  | IF
  | ELIF
  | ELSE
  | FOR
  | WHILE
  | BREAK
  | CONTINUE
  | RETURN
  | MAIN
  | FUNCTION
  | TRUE
  | FALSE
  | NULL
  | INT of (int)
  | FLOAT
  | BOOLEAN
  | MATRIX
  | ILIT of (int)
  | FLIT of (float)
  | BOOL of (bool)
  | SLIT of (string)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
