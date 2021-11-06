(*
   Type checking / inference
   or define str + int -> ???
   1. parser throw compile time errors when seeing str  +int
   2. don't throw error in parser, but throw runtime error

  we have to do all the type checking in the run time (function return type???)

*)

type operator =
  | Add
  (* | Sub
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
  | Req *)

type expr =
  | Binop of expr * operator * expr
  | Id of string
  | ILit of int

type typ = 
  Int 
  | Bool 
  | Float 
  | Matrix

type vdecl = typ * string

type assignstmt = 
    VDeAssign of typ * string * expr
  | Assign of string * expr

type stmt =
  | Assign of string * expr
  | Expr of expr
  | Return of expr
  | VDeclare of vdecl
  | AssignStmt of assignstmt

type bind = typ * string

type main = { stmts : stmt list }

type fdecl = {
  fname : string;
  formals : bind list;
  stmts : stmt list;
}

type decls = { 
  vars : bind list; 
  funcs : fdecl list;
}

type program = {
  decls: decls;
  main: main
}

(* Pretty-printing functions from microc *)
let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Matrix -> "matrix"

let string_of_op = function
  Add -> "+"

let rec string_of_expr = function
    ILit(l) -> string_of_int l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2

let rec string_of_stmt = function
  Expr(expr) -> string_of_expr expr ^ ";\n"
| Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
| Assign(v, e) -> v ^ " = " ^ string_of_expr e ^ ";\n"
| VDeclare(t, id) -> string_of_typ t ^ " " ^ id ^ ";\n"
| AssignStmt(_) -> ""

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.stmts) ^
  "}\n"

let string_of_decls (decls) =
  String.concat "" (List.map string_of_vdecl decls.vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl decls.funcs)

let string_of_program (program) = 
  (string_of_decls program.decls) ^ "\n" ^
  String.concat "\n" (List.map string_of_stmt program.main.stmts)