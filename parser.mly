%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE MOD 
%token ASSIGN PLUSASSIGN MINUSASSIGN 
%token EQ NEQ LT LEQ GT GEQ REFEQ
%token NOT AND OR
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE COMMA SEMI
%token IF ELIF ELSE
%token FOR WHILE BREAK CONTINUE
%token RETURN MAIN NULL FUNCTION
%token TRY CATCH THROW
%token IMPORT EXPORT
%token <int> ILIT
%token <float> FLIT
%token <string> SLIT
%token <string> ID
%token EOF

%left IF THEN ELSE
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE

%start main
%type <Ast.main> main

%%

class: ID LBRACE vdecls constrs fdecls mains RBRACE

constrs:
 /* epsilon */         
| constr 
| constr constrs

constr: CONSTRUCTOR LPAREN formals RPAREN LBRACE stmts RBRACE

formals: 
 dtype ID { [($1, $2)] }
| formals COMMA dtype ID { ($3, $4) :: $1 }

dtype: 
 ILIT
| BOOL 
| FLIT 
| SLIT 
| MLIT

mains:
 /* epsilon */
| main 

main: MAIN LPAREN formals RPAREN LBRACE stmts RBRACE

vdecls:
 /* epsilon */    { [] }
| vdecl vdecls { $1 :: $2 }

fdecls: 
 /* epsilon */   
| fdecl 
| fdecl fdecls

vdecl:  
 ILIT ID SEMI 
| BOOL ID SEMI 
| FLIT ID SEMI 
| SLIT ID SEMI
| MLIT ID SEMI

fdecl: FUNCTION ID LPAREN formals RPAREN LBRACE vdecls stmts RBRACE
{
    {
        fname = $2;
        formals = $4;
        vars = $7;
        body = $8;
    }
}

stmts: 
 /* epsilon */ { [] }
| stmts stmt { $2 :: $1 }

stmt:  
 expr SEMI { Expr $1 }
| RETURN expr SEMI { Return $2 }
| IF LPAREN expr RPAREN stmt { If($3, $5, []) }
| IF LPAREN expr RPAREN stmt ELSE stmt 
| IF LPAREN expr RPAREN LBRACE stmts RBRACE ELIF LPAREN expr RPAREN LBRACE stmts RBRACE ELSE LBRACE stmts RBRACE 
  {If($3, $6, $10, $13, $17)} 
| FOR LPAREN expr SEMI expr SEMI expr RPAREN LBRACE stmts RBRACE 
| WHILE LPAREN expr RPAREN LBRACE stmts RBRACE 
| dtype ID = expr

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