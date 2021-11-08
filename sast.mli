(*
   Type checking / inference
   or define str + int -> ???
   1. parser throw compile time errors when seeing str  +int
   2. don't throw error in parser, but throw runtime error

  we have to do all the type checking in the run time (function return type???)

*)
open Ast

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

type sexpr =
  | Ilit of int
  | FLit of string
  | Mlit of string
  | Bool of bool
  | Id of string
  | Binop of expr * operator * expr
  | Func of string * expr list
  | Unary of expr
  | Negate of expr

type typ = Int | Bool | Float | Matrix

type sstmt =
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type sfdecl = { sfname : string; sformals : bind list; sstmts : sstmt list }

type smain = { sstmts : sstmt list }

type sdecls = { svars : bind list; sfunc : sfdecl list }

type sprogram = { sdecls : sdecls; smain : smain }
