%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE MOD 
%token ASSIGN PLUSASSIGN MINUSASSIGN 
%token EQ NEQ LT LEQ GT GEQ REFEQ
%token NOT AND OR
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE COMMA SEMI
%token IF ELIF ELSE
%token FOR WHILE BREAK CONTINUE
%token RETURN MAIN FUNCTION
%token TRY CATCH THROW
%token NULL
%token INT FLOAT BOOLEAN MATRIX

%token <int> INT
%token <int> ILIT
%token <float> FLIT
%token <string> SLIT
%token <string> ID
%token EOF

%left IF THEN ELSE
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program: dcels main EOF { 
    { 
        dcels = $1
        main = $2 
    } 
}

dcels: 
/* epsilon */ { [], [] }
| decls vdecl { (($2 :: fst $1), snd $1) }
| decls fdecl { (fst $1, ($2 :: snd $1)) }

vdecl: typ ID SEMI { ($1, $2, Noexpr) }

fdecl: FUNCTION ID LPAREN formals RPAREN LBRACE stmts RBRACE {
    {
        fname = $2
        formals = List.rev $4
        stmts = List.rev $7
    }
}

main: MAIN LPAREN RPAREN LBRACE stmts RBRACE {
    {
        stmts = List.rev $5
    }
}

formals: 
/* nothing */ { [] }
| typ ID { [($1, $2, Noexpr)] }
| formals COMMA typ ID { ($3, $4, Noexpr) :: $1 }

typ: 
| INT { Int }
| BOOLEAN { Bool }
| FLOAT { Float }
| MATRIX { Matrix }


stmts: 
  /* nothing */  { [] }
| stmt
| stmt stmts { $1 :: $2 }

stmt:  
  expr SEMI { Expr ($1) }
| RETURN expr SEMI { Return ($2) }
| IF LPAREN expr RPAREN LBRACE stmts RBRACE elifstmts {If($3, $6, $8)}
| IF LPAREN expr RPAREN LBRACE stmts RBRACE elifstmts ELSE LBRACE stmts RBRACE {IfElse($3, $6, $8, $11)} 
| FOR LPAREN stmt SEMI expr SEMI expr RPAREN LBRACE stmts RBRACE {For($3, $5, $7, $10)} 
| WHILE LPAREN expr RPAREN LBRACE stmts RBRACE  {While($3, 6)}
| vdecls { VDeclare($1) }
| dtype ID ASSIGN expr SEMI { VDeAssign($1, $2, $4) }
| ID ASSIGN expr SEMI { Assign($1, $3) }

elifstmts:
  /* nothing */  { [] }
| ELIF LPAREN expr RPAREN LBRACE stmts RBRACE elifstmts { Elif($3, $6, $8) }


expr: 
 ILIT 
| FLIT 
| BOOL 
| SLIT
| MLIT
| ID 
| LPAREN expr RPAREN 
| expr PLUS expr
| expr MINUS expr
| expr TIMES expr
| expr DIVIDE expr
| expr PLUSASSIGN expr 
| expr MINUSASSIGN expr
| expr EQ expr
| expr NEQ expr
| expr LT expr
| expr LEQ expr
| expr GT expr
| expr GET expr
| expr REFEQ expr 
| expr ASSIGN expr

// Matrix

MLIT: 
  LBRACK matrix_row_list RBRACK { $2 }

matrix_row_list:
  matrix_row { [$1] }
| matrix_row SEMI matrix_row_list { $1 :: $3 }

matrix_row: 
  elements { $1 }

elements: 
  element { [$1] }
| element COMMA elements { $1 :: $3 }

element: FLIT { $1 }