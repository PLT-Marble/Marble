open Ast

type sexpr = dtype * sx
and sx =
  | SBinop of sexpr * operator * sexpr
  | SId of string
  | SILit of int
  | SFLit of float
  | SBLit of bool
  | SMLit of (sexpr list) list
  | SFunc of string * (sexpr list)
  | SAccess of sexpr * sexpr * sexpr
  | SUnary of uop * sexpr

(*type selifstmt = SElif of sexpr * sstmt list*)

type sassignstmt =
  | SVDeAssign of dtype * string * sexpr
  | SAssign of string * sexpr
  | SMAssign of sexpr * sexpr * sexpr * sexpr

type sstmt =
  | SExpr of sexpr
  | SReturn of sexpr
  | SVDeclare of dtype * string
  | SAssignStmt of sassignstmt
  | SIf of sexpr * sstmt list
  | SIfElse of sexpr * sstmt list * sstmt list
  | SFor of sassignstmt * sexpr * sassignstmt * sstmt list
  | SWhile of sexpr * sstmt list

type sbind = dtype * string

type smain = { sstmts : sstmt list }

type sfdecl = {
  sfname : string;
  sformals : sbind list;
  sstmts : sstmt list;
}

type sdecls = { 
  svars : sbind list; 
  sfuncs : sfdecl list;
}

type sprogram = {
  sdecls: sdecls;
  smain: smain
}

(* Pretty-printing functions from microc *)

let rec string_of_sexpr (t, e) = 
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SILit(l) -> string_of_int l
  | SFLit(l) -> string_of_float l
  | SBLit(l) -> string_of_bool l
  | SMLit(l) -> 
    let string_of_row l =
      String.concat "" (List.map string_of_sexpr l) in
      String.concat "" (List.map string_of_row l)
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnary(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SFunc(id, inputs) -> id ^ "(" ^ String.concat ", " (List.map string_of_sexpr inputs) ^";\n"
  | SAccess(e1, e2, e3) ->    
    string_of_sexpr e1 ^ " " ^ string_of_sexpr e2 ^ " " ^ string_of_sexpr e3
  ) ^ ")"

let rec string_of_sassignstmt = function
  SAssign(v, e) -> "Assign: " ^ v ^ " = " ^ string_of_sexpr e ^ ";\n"
  | SVDeAssign(t, id, sexpr) -> "VDeAssign: " ^ string_of_typ t ^ id ^ string_of_sexpr sexpr ^ ";\n"
  | SMAssign(id, r, c, v) -> "MAssign: " ^ string_of_sexpr id ^ "[" ^ string_of_sexpr r ^ ", " ^ string_of_sexpr c ^ "] = " ^ string_of_sexpr v ^ ";\n" 


let rec string_of_sstmt = function
  SExpr(sexpr) -> string_of_sexpr sexpr ^ ";\n"
| SReturn(sexpr) -> "return: " ^ string_of_sexpr sexpr ^ ";\n"
| SAssignStmt(sassignstmt) -> string_of_sassignstmt sassignstmt
| SVDeclare(t, id) -> "VDeclare: " ^ string_of_typ t ^ " " ^ id ^ ";\n"
| SWhile(e, ss) -> "While(" ^ string_of_sexpr e ^ ") { " ^ String.concat "\n" (List.map string_of_sstmt ss) ^ " }\n"
| SFor(as1,e,as2,ss) -> "For( " ^ string_of_sassignstmt as1 ^ "; " ^ string_of_sexpr e ^ "; " ^ string_of_sassignstmt as2 ^ ") { " ^ String.concat "\n" (List.map string_of_sstmt ss) ^ " }\n"
| SIf(e, ss) -> "If( " ^ string_of_sexpr e ^ ") {" ^ String.concat "\n" (List.map string_of_sstmt ss) ^ " }\n"
| SIfElse(e, ss1, ss2) -> "If( " ^ string_of_sexpr e ^ ") {" ^ String.concat "\n" (List.map string_of_sstmt ss1) ^ " } Else{ " ^ String.concat "\n" (List.map string_of_sstmt ss2) ^ " }\n"

let string_of_svdecl (t, id) = "vdecl: " ^ string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_sfdecl fdecl =
  "fdecl: " ^ fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sstmts) ^
  "}\n"

let string_of_sdecls (decls) =
  String.concat "" (List.map string_of_svdecl decls.svars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl decls.sfuncs)

let string_of_sprogram (program) = 
  (string_of_sdecls program.sdecls) ^ "\n" ^
  "inside main: \n" ^ String.concat "\n" (List.map string_of_sstmt program.smain.sstmts)