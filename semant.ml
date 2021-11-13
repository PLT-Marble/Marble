open Ast
open Sast

module StringMap = Map.Make (String)

let check (program) =
  let funcs = program.decls.funcs
  in
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
      (Null, b) -> raise (Failure ("illegal null " ^ kind ^ " " ^ b))
    | _ -> ()) binds;
  let rec dups = function
      [] -> ()
    |	((_,n1) :: (_,n2) :: _) when n1 = n2 -> raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
    | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in
  let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
    StringMap.empty (program.decls.vars)
  in
  let type_of_identifier s =
    try StringMap.find s symbols
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
  let rec check_expr = function
     Id n -> (type_of_identifier n, SId n)
    | ILit l -> (Int, SILit l)
    | Binop(e1, op, e2) as e -> 
      let (t1, e1') = check_expr e1 
      and (t2, e2') = check_expr e2 in
      (* All binary operators require operands of the same type *)
      let same = t1 = t2 in
      (* Determine expression type based on operator and operand types *)
      let ty = match op with
        Add | Sub | Mul | Div when same && t1 = Int   -> Int
      | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Func(id, inputs) ->
          let sinputs = List.map check_expr inputs in
          (Int, SFunc(id, sinputs))
  in  
  let rec check_stmt = function
    | Assign (n, e) -> SAssign (n, check_expr e)
    | Expr e -> SExpr (check_expr e)
    | Return e -> SReturn (check_expr e)
    | VDeclare (ty, n) -> SVDeclare(ty, n)
    | VDeAssign (ty, n, e) -> SVDeAssign(ty, n, check_expr e)
  and check_stmt_list = function
    | [ (Return _ as s) ] -> [ check_stmt s ]
    | Return _ :: _ -> raise (Failure "Unreachable statments after return")
    | s :: ss -> check_stmt s :: check_stmt_list ss
    | [] -> []
  in
  let check_decls vars funcs = 
    let check_function func =      
    { 
      sfname = func.fname; 
      sformals = func.formals; 
      sstmts = check_stmt_list func.stmts 
    }
    in 
    {
      svars = vars;
      sfuncs = List.map check_function funcs;
    }  
  in
  {
    sdecls = check_decls program.decls.vars program.decls.funcs;
    smain = { sstmts = check_stmt_list program.main.stmts }
  }
;;