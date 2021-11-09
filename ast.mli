(*
   Type checking / inference
   or define str + int -> ???
   1. parser throw compile time errors when seeing str  +int
   2. don't throw error in parser, but throw runtime error

  we have to do all the type checking in the run time (function return type???)

*)

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
  | Ilit of int
  | FLit of string
  | MLit of expr list
  | Bool of bool
  | Id of string
  | Binop of expr * operator * expr
  | Func of string * expr list
  | Unary of expr
  | Negate of expr

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
