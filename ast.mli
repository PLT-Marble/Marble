type operator = Add | Sub | Mul | Div

type expr =
  | Binop of expr * operator * expr
  | Lit of int
  | Var of string
  | Assign of string * expr
  | Seq of expr * expr
  | Condition of expr * expr * expr

type typ = Int | Bool | Float | Matrix

type stmt =
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type bind = typ * string

type func_decl = { fname : string; formals : bind list; stmts : stmt list }

type main = { stmts : stmt list }

type program = bind list * func_decl list * main