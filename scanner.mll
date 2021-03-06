{ open Parser
(* Reference: https://ocaml.org/manual/lexyacc.html *)
(* 2.7 keywords *)
let keyword_table = Hashtbl.create 53
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      [        
        "return", RETURN;
        "function", FUNCTION;
        "int", INT;
        "float", FLOAT;
        "bool", BOOL;
        "matrix", MATRIX;
        "null", NULL;        
        "while", WHILE;
        "for", FOR;
        "if", IF;
        "else", ELSE;
      ]
}

let digit = ['0' - '9']
let digits = digit+
let float = digits '.' digits
let quote = ['\'' '\"']

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
(* 2.2 comments *)
| "//" { comment lexbuf } 
| "/*" { comments lexbuf }
(* 2.1 types *)
| digits as lit { ILIT(int_of_string lit) }
| float as lit { FLIT(float_of_string lit) }
| "true" { BLIT(true) }
| "false" { BLIT(false) }
(* 2.6 operators *)
| '='        { ASSIGN }
| "+="       { PLUSASSIGN }
| "-="       { MINUSASSIGN }
| '+'        { PLUS } 
| '-'        { MINUS }
| '*'        { TIMES }
| '/'        { DIVIDE }
| '%'        { MOD }
| "=="       { EQ }
| "!="       { NEQ }
| '<'        { LT }
| "<="       { LEQ }
| ">"        { GT }
| ">="       { GEQ }
| "!"        { NOT }
| "&&"       { AND }
| "||"       { OR }
(* 2.7 seperators *)
| '('        { LPAREN }
| ')'        { RPAREN }
| '['        { LBRACK }
| ']'        { RBRACK }
| '{'        { LBRACE }
| '}'        { RBRACE }
| ','        { COMMA }
| ';'        { SEMI }
(* 2.7 keywords *)
| ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * as id
  { (*print_endline "scanner find id: ";
    print_endline id; *)
    try
      Hashtbl.find keyword_table id
    with Not_found ->
      ID id }
(*  *)
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comments = parse
  "*/" { tokenize lexbuf }
  | _    { comments lexbuf }
and comment = parse
  '\n' { tokenize lexbuf }
  | _ { comment lexbuf }
