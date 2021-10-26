{ open Parser
(* Reference: https://ocaml.org/manual/lexyacc.html *)
(* 2.7 keywords *)
let keyword_table = Hashtbl.create 53
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      [ "if", IF;
        "elif", ELIF;
        "else", ELSE;
        "for", FOR;
        "while", WHILE;
        "break", BREAK;
        "continue", CONTINUE;
        "main", MAIN;
        "constructor", CONSTRUCTOR;
        "return", RETURN;
        "function", FUNCTION;
        "class", CLASS;
        "import", IMPORT;
        "int", INT;
        "float", FLOAT;
        "boolean", BOOLEAN;
        "matrix", MATRIX;
        "null", NULL;
        "string", STRING;
      ]
}

let digit = ['0' - '9']
let digits = digit+
let float = digits '.' digits
let boolean = "true" | "false"
let quote = ['\'' '\"']
let matrix = '[' ((float,)* float? ';')* ((float,)* float?)? ']'

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
(* 2.2 comments *)
| "//" { comment lexbuf } 
| "/*" { comments lexbuf }
(* 2.1 types *)
| digits as lit { ILIT(int_of_string lit) }
| float as lit { FLIT(float_of_string lit) }
| boolean as lit { BOOL(bool_of_string lit) }
| '"' ([^ '"']* as lit) '"' { SLIT(lit) }
| matrix as lit { MLIT(lit)}
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
| "~="       { REFEQ }
(* 2.7 seperators *)
| '('        { LPAREN }
| ')'        { RPAREN }
| '['        { LBRACK }
| ']'        { RBRACK }
| '{'        { LBRACE }
| '}'        { RBRACE }
| ','        { COMMA }
| ';'        { SEMI }
| '.'        { DOT }
(* 2.7 keywords *)
| ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * as id
  { print_endline "find id: ";
    print_endline id;
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
