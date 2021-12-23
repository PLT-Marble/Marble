%{
open Ast
open Lexing
let parse_error s =
      begin
        try
          let start_pos = Parsing.symbol_start_pos ()
          and end_pos = Parsing.symbol_end_pos () in
          Printf.printf "File \"%s\", line %d, characters %d-%d: \n"
            start_pos.pos_fname
            start_pos.pos_lnum
            (start_pos.pos_cnum - start_pos.pos_bol)
            (end_pos.pos_cnum - start_pos.pos_bol)
        with Invalid_argument(_) -> ()
      end;
      Printf.printf "Syntax error: %s\n" s;
      raise Parsing.Parse_error
%}

%token PLUS MINUS TIMES DIVIDE MOD
%token EQ NEQ LT LEQ GT GEQ
%token NOT AND OR
%token ASSIGN PLUSASSIGN MINUSASSIGN
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE COMMA SEMI
%token IF ELSE
%token WHILE FOR
%token RETURN FUNCTION
%token NULL
%token INT FLOAT BOOL MATRIX

%token <int> ILIT
%token <float> FLIT
%token <bool> BLIT
%token <string> ID
%token EOF

%left IF ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left LBRACK RBRACK
%right NOT

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [])               }
 | decls vdecl { (($2 :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1)) }


vdecl: dtype ID SEMI { $1, $2 }

fdecl: dtype FUNCTION ID LPAREN formals RPAREN LBRACE stmts RBRACE {
    {
        return = $1;
        fname = $3;
        formals = List.rev $5;
        stmts = List.rev $8;
    }
}

formals: 
/* nothing */ { [] }
| dtype ID { [($1, $2)] }
| formals COMMA dtype ID { ($3, $4) :: $1 }

dtype: 
| INT { Int }
| FLOAT { Float }
| BOOL { Bool }
| MATRIX { Matrix }

stmts: 
  /* nothing */  { [] }
| stmts stmt { $2 :: $1 }

stmt:  
  expr SEMI { Expr ($1) }
| RETURN expr SEMI { Return ($2) }
| dtype ID SEMI { VDeclare($1, $2) }
| assignstmt SEMI { AssignStmt($1) }
| WHILE LPAREN expr RPAREN LBRACE stmts RBRACE  {While($3, List.rev $6)}
| FOR LPAREN assignstmt SEMI expr SEMI assignstmt RPAREN LBRACE stmts RBRACE {For($3, $5, $7, List.rev $10)} 
| IF LPAREN expr RPAREN LBRACE stmts RBRACE {If($3, List.rev $6)}
| IF LPAREN expr RPAREN LBRACE stmts RBRACE ELSE LBRACE stmts RBRACE {IfElse($3, List.rev $6, List.rev $10)}  

assignstmt:
  dtype ID ASSIGN expr { VDeAssign($1, $2, $4) }
| ID PLUSASSIGN expr { Assign($1, Binop(Id($1), Add, $3)) }
| ID MINUSASSIGN expr { Assign($1, Binop(Id($1), Sub, $3)) }
| ID ASSIGN expr { Assign($1, $3) }
| expr LBRACK expr COMMA expr RBRACK ASSIGN expr { MAssign($1, $3, $5, $8) }

expr: 
 ILIT  { ILit($1) }
| FLIT { FLit($1) }
| BLIT { BLit($1) }
| matrix { MLit($1) }
| expr LBRACK expr COMMA expr RBRACK { Access($1, $3, $5) }
| ID   { Id($1) }
| expr PLUS expr   { Binop($1, Add, $3) }
| expr MINUS expr  { Binop($1, Sub, $3) }
| expr TIMES expr  { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| ID LPAREN inputs RPAREN { Func($1, $3) }
| MINUS expr %prec NOT   { Unary(Neg, $2) }
| NOT expr         { Unary(Not, $2) }
| expr AND expr    { Binop($1, And, $3) }
| expr OR expr     { Binop($1, Or, $3) }  
| expr MOD expr { Binop($1, Mod, $3) }
| expr EQ expr  { Binop($1, Eq, $3) }
| expr NEQ expr { Binop($1, Neq, $3) }
| expr LT expr  { Binop($1, Less, $3) }
| expr LEQ expr { Binop($1, Leq, $3) }
| expr GT expr  { Binop($1, Greater, $3) }
| expr GEQ expr { Binop($1, Geq, $3) }

inputs:
 /* nothing */  { [] }
| expr           { [$1] }
| expr COMMA inputs    { $1 :: $3 }

matrix:
  LBRACK matrix_row_list RBRACK { $2 }

matrix_row_list:
|  matrix_row { [$1] }
| matrix_row SEMI matrix_row_list { $1 :: $3 }

matrix_row: 
| expr { [$1] }
| expr COMMA matrix_row { $1 :: $3 }