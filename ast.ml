type operator = Add | Sub | Mul | Div
(* | And
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
  | FLit of float
(* | MLit of (expr list) list *)

type dtype =
  | Int
  | Bool
  | Float
  (* | Matrix *)
  | Null

type stmt =
  | Assign of string * expr
  | Expr of expr
  | Return of expr
  | VDeclare of dtype * string
  | VDeAssign of dtype * string * expr
  | Block of stmt list

type bind = dtype * string

type main = { stmts : stmt list }

type fdecl = { fname : string; formals : bind list; stmts : stmt list }

type decls = { vars : bind list; funcs : fdecl list }

type program = { vars : bind list; funcs : fdecl list }

(* Pretty-printing functions from microc *)
let string_of_typ = function
  | Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  (* | Matrix -> "matrix" *)
  | Null -> "null"

let string_of_op = function Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

let rec string_of_expr = function
  | ILit l -> string_of_int l
  | FLit l -> string_of_float l
  | Id s -> s
  | Binop (e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
(* | MLit(l) -> let string_of_row l =
   String.concat ", " (List.map string_of_expr l)
   in
   String.concat "\n" (List.map string_of_row l) *)

let rec string_of_stmt = function
  | Expr expr -> string_of_expr expr ^ ";\n"
  | Return expr -> "return: " ^ string_of_expr expr ^ ";\n"
  | Assign (v, e) -> "Assign: " ^ v ^ " = " ^ string_of_expr e ^ ";\n"
  | VDeclare (t, id) -> "VDeclare: " ^ string_of_typ t ^ " " ^ id ^ ";\n"
  | VDeAssign (t, id, expr) ->
      "VDeAssign: " ^ string_of_typ t ^ id ^ string_of_expr expr ^ ";\n"
  | Block stmts ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"

let string_of_vdecl (t, id) = "vdecl: " ^ string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  "fdecl: " ^ fdecl.fname ^ "("
  ^ String.concat ", " (List.map snd fdecl.formals)
  ^ ")\n{\n"
  ^ String.concat "" (List.map string_of_stmt fdecl.stmts)
  ^ "}\n"

let string_of_decls decls =
  String.concat "" (List.map string_of_vdecl decls.vars)
  ^ "\n"
  ^ String.concat "\n" (List.map string_of_fdecl decls.funcs)

let string_of_program program =
  string_of_decls program.decls
  ^ "\n" ^ "inside main: \n"
  ^ String.concat "\n" (List.map string_of_stmt program.main.stmts)
