%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE MOD ASSIGN
%token EQ NEQ LT LEQ GT GEQ 
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

expr:
| expr PLUS   expr { Binop($1, Add, $3) }
| expr MINUS  expr { Binop($1, Sub, $3) }
| expr TIMES  expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| ILIT          { Lit($1) }
| ID              { Var($1) }
| ID ASSIGN expr  { Assign($1, $3) }
| IF expr THEN expr ELSE expr { Condition($2, $4, $6) }
