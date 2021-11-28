%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE
%token ASSIGN 
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE COMMA SEMI
%token RETURN MAIN FUNCTION
%token NULL
%token INT FLOAT

%token <int> ILIT
%token <float> FLIT
%token <string> ID
%token EOF

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

stmts: 
  /* nothing */  { [] }
| stmts stmt { $2 :: $1 }

stmt:  
  expr SEMI { Expr ($1) }
| RETURN expr SEMI { Return ($2) }
| dtype ID SEMI { VDeclare($1, $2) }
| assignstmt SEMI { AssignStmt($1) }

assignstmt:
  dtype ID ASSIGN expr { VDeAssign($1, $2, $4) }
| ID ASSIGN expr { Assign($1, $3) }


expr: 
 ILIT  { ILit($1) }
| FLIT { FLit($1) }
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