%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE MOD 
%token ASSIGN PLUSASSIGN MINUSASSIGN 
%token EQ NEQ LT LEQ GT GEQ REFEQ
%token NOT AND OR
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE COMMA SEMI
%token IF ELIF ELSE
%token FOR WHILE BREAK CONTINUE
%token RETURN MAIN FUNCTION
%token NULL
%token INT FLOAT BOOLEAN MATRIX

%token <int> ILIT
%token <float> FLIT
%token <bool> BLIT
%token <string> SLIT
%token <string> MLIT
%token <string> ID
%token EOF

%left IF THEN ELSE
%left OR
%left AND
%right ASSIGN
%left EQ NEQ REFEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT


%start program
%type <Ast.program> program

%%

program: decls main EOF {{ 
    decls = $1;
    main = $2; 
}}

decls: 
/* nothing */ { { vars = []; funcs = []; } }
| decls vdecl { { vars = $2 :: $1.vars; funcs = $1.funcs; } }
| decls fdecl { { vars = $1.vars; funcs = $2 :: $1.funcs; } }

vdecl: dtype ID SEMI { $1, $2 }
// int i;
// might want to allow statements
// danger to leave i uninitialzized
// function a(){
//   i = 2;
// }

fdecl: FUNCTION ID LPAREN formals RPAREN LBRACE stmts RBRACE {
    {
        fname = $2;
        formals = List.rev $4;
        stmts = List.rev $7;
    }
}

main: MAIN LPAREN RPAREN LBRACE stmts RBRACE {
    {
        stmts = List.rev $5
    }
}

formals: 
/* nothing */ { [] }
| dtype ID { [($1, $2)] }
| formals COMMA dtype ID { ($3, $4) :: $1 }

dtype: 
| INT { Int }
| BOOLEAN { Bool }
| FLOAT { Float }
| MATRIX { Matrix }

stmts: 
  /* nothing */  { [] }
| stmts stmt { $2 :: $1 }

stmt:  
  expr SEMI { Expr ($1) }
| RETURN expr SEMI { Return ($2) }
// | IF LPAREN expr RPAREN LBRACE stmts RBRACE elifstmts {If($3, $6, $8)}
// | IF LPAREN expr RPAREN LBRACE stmts RBRACE elifstmts ELSE LBRACE stmts RBRACE {IfElse($3, $6, $8, $11)} 
// replace stmt with dtype ID ASSIGN expr SEMI { VDeAssign($1, $2, $4) }
// | FOR LPAREN assignstmt SEMI expr SEMI expr RPAREN LBRACE stmts RBRACE {For($3, $5, $7, $10)} 
// | WHILE LPAREN expr RPAREN LBRACE stmts RBRACE  {While($3, $6)}
| dtype ID ASSIGN expr SEMI { VDeAssign($1, $2, $4) }
| ID ASSIGN expr SEMI { Assign($1, $3) }
| vdecl { VDeclare($1) }
// | assignstmt SEMI { AssignStmt($1) }
// | ID PLUSASSIGN expr { Assign($1, Binop($1, AddEq, $3)) }
// | ID MINUSASSIGN expr { Assign($1, Binop($1, SubEq, $3)) }

// elifstmts:
//   /* nothing */  { [] }
// | ELIF LPAREN expr RPAREN LBRACE stmts RBRACE elifstmts { Elif($3, $6, $8) }
// int i = 0
// for(i+=2; i<10; i++)
// assignstmt:

expr: 
 ILIT  { ILit($1) }
// | FLIT { fLit($1) }
// | BLIT { bLit($1) }
| matrix { MLit($1) }
| ID   { Id($1) }
// | ID LPAREN inputs RPAREN { Func($1,$3) }
// | LPAREN expr RPAREN { $2 }
// | MINUS expr       { Unary($2) }
// | NOT expr         { Negate($2) }  
// | expr AND expr    { Binop($1, And, $3) }
// | expr OR expr     { Binop($1, Or, $3) }
| expr PLUS expr   { Binop($1, Add, $3) }
// | expr MINUS expr  { Binop($1, Sub, $3) }
// | expr TIMES expr  { Binop($1, Mult, $3) }
// | expr DIVIDE expr { Binop($1, Div, $3) }
// | expr MOD expr { Binop($1, Mod, $3) }
// | expr EQ expr  { Binop($1, Eq, $3) }
// | expr NEQ expr { Binop($1, Neq, $3) }
// | expr LT expr  { Binop($1, Less, $3) }
// | expr LEQ expr { Binop($1, Leq, $3) }
// | expr GT expr  { Binop($1, Greater, $3) }
// | expr GEQ expr { Binop($1, Geq, $3) }
// | expr REFEQ expr  { Binop($1, Req, $3) }

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
| element { [$1] }
| element COMMA matrix_row { $1 :: $3 }

element:
  FLIT { FLit($1) }