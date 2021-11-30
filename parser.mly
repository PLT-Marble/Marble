%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE
%token ASSIGN 
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE COMMA SEMI
%token IF ELIF ELSE
%token WHILE FOR
%token RETURN MAIN FUNCTION
%token NULL
%token INT FLOAT BOOL MATRIX

%token <int> ILIT
%token <float> FLIT
%token <bool> BLIT
%token <string> ID
%token EOF

%left IF THEN ELSE
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE MOD

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
| WHILE LPAREN expr RPAREN LBRACE stmts RBRACE  {While($3, $6)}
| FOR LPAREN assignstmt SEMI expr SEMI expr RPAREN LBRACE stmts RBRACE {For($3, $5, $7, $10)} 
| IF LPAREN expr RPAREN LBRACE stmts RBRACE {If($3, $6)}
| IF LPAREN expr RPAREN LBRACE stmts RBRACE ELSE LBRACE stmts RBRACE {IfElse($3, $6, $10)} 
//| IF LPAREN expr RPAREN LBRACE stmts RBRACE elifstmts {If($3, $6, $8)}
//| IF LPAREN expr RPAREN LBRACE stmts RBRACE elifstmts ELSE LBRACE stmts RBRACE {IfElse($3, $6, $8, $11)} 

assignstmt:
  dtype ID ASSIGN expr { VDeAssign($1, $2, $4) }
| ID ASSIGN expr { Assign($1, $3) }
| expr LBRACK expr COMMA expr RBRACK ASSIGN expr { MAssign($1, $3, $5, $8) }

//elifstmts:
//   /* nothing */  { [] }
// | elifstmts elifstmt {$2 :: $1}

//elifstmt:
//| ELIF LPAREN expr RPAREN LBRACE stmts RBRACE { Elif($3, $6) }
// int i = 0
// for(i+=2; i<10; i++)


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