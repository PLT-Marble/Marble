(*
   Type checking / inference
   or define str + int -> ???
   1. parser throw compile time errors when seeing str  +int
   2. don't throw error in parser, but throw runtime error

  we have to do all the type checking in the run time (function return type???)

*)
open Ast


type sexpr =
  | SIlit of int
  | SFLit of string
  | SMLit of sexpr list
  | SBool of bool
  | SId of string
  | SBinop of sexpr * operator * sexpr
  | SFunc of string * sexpr list
  | SUnary of sexpr
  | SNegate of sexpr

(*think might not need type *)
type typ = Int | Bool | Float | Matrix 

type selifstmts = SElif of sexpr * sstmt list * selifstmts

type sassignstmt = 
  | SVDeAssign of typ * string * sexpr
  | SAssign of string * sexpr

type sstmt =
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt list * sstmt list
  | SIfElse of sexpr * sstmt list * selifstmts * sstmt list 
  | SFor of sexpr * sexpr * sexpr * sstmt list
  | SWhile of sexpr * sstmt list
  | SVDeclare of typ * string
  | SAssignStmt of sassignstmt

type sfdecl = { sfname : string; sformals : bind list; sstmts : sstmt list }

type smain = { sstmts : sstmt list }

type sdecls = { svars : bind list; sfunc : sfdecl list }

type sprogram = { sdecls : sdecls; smain : smain }
