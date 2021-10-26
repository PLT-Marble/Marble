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

%start expr
%type <Ast.expr> expr

%%

class: ID LBRACE filddecls constrs fdecls mains RBRACE

constrs:
 %empty
| constr 
| constr constrs

constr: CONSTRUCTOR LPAREN formals RPAREN LBRACE stmts RBRACE

formals: 
 dtype ID 
| formals, dtype ID

dtype: 
 ILIT 
| BOOL 
| FLIT 
| SLIT 
| MLIT

mains:
 %empty
| main

main: MAIN LPAREN formals RPAREN LBRACE stmts RBRACE

filddecls:
 %empty    
| filddecl 
| filddecl filddecls

fdecls: 
 %empty   
| fdecl 
| fdecl fdecls

filddecl:  
 ILIT ID SEMI 
| BOOL ID SEMI 
| FLIT ID SEMI 
| SLIT ID SEMI
| MLIT ID SEMI

fdecl: FUNCTION ID LPAREN formals RPAREN LBRACE vdecls stmts RBRACE

stmts: 
 %empty 
| stmt 
| stmts stmt

stmt:  
 expr SEMI 
| RETURN expr SEMI 
| IF LPAREN expr RPAREN stmt 
| IF LPAREN expr RPAREN stmt ELSE stmt 
| IF LPAREN expr RPAREN LBRACE stmts RBRACE elif LPAREN expr RPAREN LBRACE stmts RBRACE else LBRACE stmts RBRACE  
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

MLIT: LBRACE matrix_row_list RBRACE   

matrix_row_list: 
 matrix_row 
| matrix_row SEMI matrix_row_list

matrix_row: elements

elements: element | element, elements

element: FLIT

// expr:
//  expr PLUS   expr { Binop($1, Add, $3) }
// | expr MINUS  expr { Binop($1, Sub, $3) }
// | expr TIMES  expr { Binop($1, Mul, $3) }
// | expr DIVIDE expr { Binop($1, Div, $3) }
// | ILIT          { Lit($1) }
// | ID              { Var($1) }
// | ID ASSIGN expr  { Assign($1, $3) }
// | IF expr THEN expr ELSE expr { Condition($2, $4, $6) }