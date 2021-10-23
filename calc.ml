open Ast

module StringMap = Map.Make(String);;
let variables = ref StringMap.empty;;

let rec eval = function 
    Lit(x)            -> x
  | Var(x) -> StringMap.find x !variables
  | Binop(e1, op, e2) ->
      let v1  = eval e1 in
      let v2 = eval e2 in
      (match op with
	Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> v1 / v2)
  | Assign(name, v) -> 
      let ev = eval v in
      variables := StringMap.add name ev !variables;
      ev
  | Condition(e1, e2, e3) ->
      if (eval e1) = 0 then eval e3 else eval e2
  | Seq(x, y) -> let _ = eval x in eval y 

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lexbuf in
  let result = eval expr in
  print_endline (string_of_int result)
  
(* debug
let _ =
  let tuples = StringMap.fold (fun k v l -> (k, v) :: l) !variables [] in
  List.iter (fun (k, v) ->
    print_endline (k ^ " " ^ string_of_int v)) tuples
     *)