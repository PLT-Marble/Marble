(* Semantic checking for the BLAStoff compiler *)

open Ast
open Sast

module StringMap = Map.Make (String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.
   Check each global variable, then check each function *)

let check (program) =
  let funcs = program.decls.funcs
  in
  (* let check_vars loc stmt_lst =
    let add_decl lst = function
      | Expr e ->
        (match e with
        | Id var -> var :: lst
        | _ -> lst)
      | _ -> lst
    in
    let decls = List.fold_left add_decl [] stmt_lst in
    let rec check_dups = function
      | [] -> ()
      | n1 :: n2 :: _ when n1 = n2 -> raise (Failure ("duplicate " ^ n1 ^ " in " ^ loc))
      | _ :: tl -> check_dups tl
    in
    check_dups (List.sort compare decls)
  in *)
  (* let check_binds (kind : string) (binds : bind list) =
    List.iter (function
      (Null, b) -> raise (Failure ("illegal null " ^ kind ^ " " ^ b))
    | _ -> ()) binds;
  let rec dups = function
      [] -> ()
    |	((_,n1) :: (_,n2) :: _) when n1 = n2 -> raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
    | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in *)
  let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
  StringMap.empty (program.decls.vars)
  in
  (* Return a variable from our local symbol table *)
  let type_of_identifier s =
  try StringMap.find s symbols
  with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name 
    {
      fname = name; 
      formals = [(ty, "x")];
      stmts = [] 
    } map
    in List.fold_left add_bind StringMap.empty [ ("print", Int);
			                         ("printb", Bool);
			                         ("printf", Float);
			                         ("printbig", Int) ]
  (* Add function name to symbol table *)
  in
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *) in
    match fd with
    (* No duplicate functions or redefinitions of built-ins *)
    | _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ -> StringMap.add n fd map
  in
  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls funcs in
  let find_func fname =
    try StringMap.find fname function_decls with
    | Not_found -> raise (Failure ("Undeclared function " ^ fname))
  in
  let rec check_expr = function
     Id n -> (type_of_identifier n, SId n)
    | ILit l -> (Int, SILit l)
    | FLit l -> (Float, SFLit l)
    (* | MLit m -> SMLit m *)
    | Binop(e1, op, e2) as e -> 
      let (t1, e1') = check_expr e1 
      and (t2, e2') = check_expr e2 in
      (* All binary operators require operands of the same type *)
      let same = t1 = t2 in
      (* Determine expression type based on operator and operand types *)
      let ty = match op with
        Add | Sub | Mul | Div when same && t1 = Int   -> Int
      | Add | Sub | Mul | Div when same && t1 = Float -> Float
      | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
  in
  let rec check_stmt = function
    | Assign (n, e) -> SAssign (n, check_expr e)
    | Expr e -> SExpr (check_expr e)
    | Block bl -> SBlock (check_stmt_list bl)
    | Return e -> SReturn (check_expr e)
    | VDeclare (ty, n) -> SVDeclare(ty, n)
    | VDeAssign (ty, n, e) -> SVDeAssign(ty, n, check_expr e)
  and check_stmt_list = function
    | [ (Return _ as s) ] -> [ check_stmt s ]
    | Return _ :: _ -> raise (Failure "Unreachable statments after return")
    | Block sl :: ss -> check_stmt_list (sl @ ss)
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
