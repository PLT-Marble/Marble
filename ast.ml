type operator =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Eq
  | Neq
  | Less
  | Leq
  | Greater
  | Geq

type uop = Neg | Not

type expr =
  | Binop of expr * operator * expr
  | ILit of int
  | FLit of float
  | BLit of bool
  | MLit of expr list list
  | Id of string
  | Func of string * expr list
  | Access of expr * expr * expr
  | Unary of uop * expr

type dtype = Int | Float | Bool | Matrix | Null

(*type elifstmt = Elif of expr * stmt list*)

type assignstmt =
  | VDeAssign of dtype * string * expr
  | Assign of string * expr
  | MAssign of expr * expr * expr * expr

type stmt =
  | Expr of expr
  | Return of expr
  | VDeclare of dtype * string
  | AssignStmt of assignstmt
  | If of expr * stmt list
  | IfElse of expr * stmt list * stmt list
  | For of assignstmt * expr * assignstmt * stmt list
  | While of expr * stmt list

type bind = dtype * string

type fdecl = {
  return : dtype;
  fname : string;
  formals : bind list;
  stmts : stmt list;
}

type program = bind list * fdecl list

(* Pretty-printing functions from microc *)
let string_of_typ = function
  | Int -> "int"
  | Null -> "null"
  | Float -> "float"
  | Bool -> "Bool"
  | Matrix -> "matrix"

let string_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function Neg -> "-" | Not -> "!"

let rec string_of_expr = function
  | ILit l -> string_of_int l
  | FLit l -> string_of_float l
  | BLit l -> string_of_bool l
  | MLit l ->
      let string_of_row l = String.concat "" (List.map string_of_expr l) in
      String.concat "" (List.map string_of_row l)
  | Id s -> s
  | Binop (e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unary (o, e) -> string_of_uop o ^ string_of_expr e
  | Func (id, inputs) ->
      id ^ "(" ^ String.concat ", " (List.map string_of_expr inputs) ^ ");\n"
  | Access (e1, e2, e3) ->
      string_of_expr e1 ^ " " ^ string_of_expr e2 ^ " " ^ string_of_expr e3

let string_of_assignstmt = function
  | VDeAssign (t, id, expr) ->
      "VDeAssign: " ^ string_of_typ t ^ id ^ string_of_expr expr ^ ";\n"
  | Assign (v, e) -> "Assign: " ^ v ^ " = " ^ string_of_expr e ^ ";\n"
  | MAssign (id, r, c, v) ->
      "MAssign: " ^ string_of_expr id ^ "[" ^ string_of_expr r ^ ", "
      ^ string_of_expr c ^ "] = " ^ string_of_expr v ^ ";\n"

let rec string_of_stmt = function
  | Expr expr -> string_of_expr expr ^ ";\n"
  | Return expr -> "return: " ^ string_of_expr expr ^ ";\n"
  | AssignStmt assignstmt -> string_of_assignstmt assignstmt
  | VDeclare (t, id) -> "VDeclare: " ^ string_of_typ t ^ " " ^ id ^ ";\n"
  | While (e, ss) ->
      "While(" ^ string_of_expr e ^ ") { "
      ^ String.concat "\n" (List.map string_of_stmt ss)
      ^ " }\n"
  | For (as1, e, as2, ss) ->
      "For( " ^ string_of_assignstmt as1 ^ "; " ^ string_of_expr e ^ "; "
      ^ string_of_assignstmt as2 ^ ") { "
      ^ String.concat "\n" (List.map string_of_stmt ss)
      ^ " }\n"
  | If (e, ss) ->
      "If( " ^ string_of_expr e ^ ") {"
      ^ String.concat "\n" (List.map string_of_stmt ss)
      ^ " }\n"
  | IfElse (e, ss1, ss2) ->
      "If( " ^ string_of_expr e ^ ") {"
      ^ String.concat "\n" (List.map string_of_stmt ss1)
      ^ " } Else{ "
      ^ String.concat "\n" (List.map string_of_stmt ss2)
      ^ " }\n"

let string_of_vdecl (t, id) = "vdecl: " ^ string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  "fdecl: " ^ fdecl.fname ^ "("
  ^ String.concat ", " (List.map snd fdecl.formals)
  ^ ")\n{\n"
  ^ String.concat "" (List.map string_of_stmt fdecl.stmts)
  ^ "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars)
  ^ "\n"
  ^ String.concat "\n" (List.map string_of_fdecl funcs)
