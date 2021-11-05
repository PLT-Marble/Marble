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
  | NULL
  | INT of (int)
  | FLOAT
  | BOOLEAN
  | MATRIX
  | ILIT of (int)
  | FLIT of (float)
  | TRUE of (bool)
  | FALSE of (bool)
  | SLIT of (string)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
