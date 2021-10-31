type operator =
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Eq
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | Req

type expr =
  | Binop of expr * operator * expr
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

type fdecl = { fname : string; formals : bind list; stmts : stmt list }

type main = { stmts : stmt list }

type decls = { vars : bind list; func : fdecl list }

type program = { decls : decls; main : main }
