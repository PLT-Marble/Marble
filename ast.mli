type operator = Add | Sub | Mul | Div

type expr =
    Binop of expr * operator * expr
  | Lit of int
  | Var of string
  | Assign of string * expr
  | Seq of expr * expr
  | Condition of expr * expr * expr
